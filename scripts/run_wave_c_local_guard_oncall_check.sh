#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
STRICT=false
OUTPUT_FILE=""
QUIET=false

usage() {
  cat <<'USAGE'
Wave C B129 Local Guard Oncall Check

用途：
  以值班/cron 友好方式执行 local-first 守护检查，并输出单行状态。

用法：
  scripts/run_wave_c_local_guard_oncall_check.sh [options]

选项：
  --run-id ID       指定 run_id
  --output FILE     输出报告路径
  --strict          非 PASS 返回非 0
  --quiet           仅输出单行状态
  --help            显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --output)
      OUTPUT_FILE="$2"
      shift 2
      ;;
    --strict)
      STRICT=true
      shift
      ;;
    --quiet)
      QUIET=true
      shift
      ;;
    --help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage
      exit 1
      ;;
  esac
done

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="tmp/test-reports/wave_c_b129_oncall_check_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

ONCALL_DIR="tmp/test-reports"
bundle_report="$ONCALL_DIR/wave_c_b125_local_guard_bundle_${RUN_ID}.md"
history_report="$ONCALL_DIR/wave_c_b126_local_guard_history_${RUN_ID}.md"

bundle_log="$ONCALL_DIR/wave_c_b125_local_guard_bundle_${RUN_ID}.oncall.log"
history_log="$ONCALL_DIR/wave_c_b126_local_guard_history_${RUN_ID}.oncall.log"

run_step() {
  local cmd="$1"
  local log="$2"

  set +e
  eval "$cmd" > "$log" 2>&1
  local ec=$?
  set -e

  echo "$ec"
}

bundle_exit=$(run_step \
  "bash scripts/run_wave_c_local_first_guard_bundle.sh --run-id ${RUN_ID} --strict --output ${bundle_report}" \
  "$bundle_log")

history_exit=$(run_step \
  "bash scripts/summarize_wave_c_local_guard_history.sh --run-id ${RUN_ID} --strict --output ${history_report}" \
  "$history_log")

workflow_state="UNKNOWN"
if [[ -f ".github/workflows/wave-c-quick-sprint-manual.yml.disabled" && ! -f ".github/workflows/wave-c-quick-sprint-manual.yml" ]]; then
  workflow_state="DISABLED"
elif [[ -f ".github/workflows/wave-c-quick-sprint-manual.yml" ]]; then
  workflow_state="ENABLED"
fi

overall="PASS"
if [[ "$bundle_exit" != "0" || "$history_exit" != "0" || "$workflow_state" != "DISABLED" ]]; then
  overall="FAIL"
fi

history_trend="UNKNOWN"
if [[ -f "$history_report" ]]; then
  history_trend="$(rg -o "trend_state:[[:space:]]*\*\*[A-Z_]+\*\*" "$history_report" | head -1 | sed -E 's/.*\*\*([A-Z_]+)\*\*/\1/' || true)"
  history_trend="${history_trend:-UNKNOWN}"
fi

{
  echo "# Wave C B129 Local Guard Oncall Check"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- overall: **$overall**"
  echo
  echo "## Checks"
  echo
  echo "| check | value | result |"
  echo "|------|-------|--------|"
  echo "| workflow_state | $workflow_state | $([[ "$workflow_state" == "DISABLED" ]] && echo PASS || echo FAIL) |"
  echo "| b125_exit | $bundle_exit | $([[ "$bundle_exit" == "0" ]] && echo PASS || echo FAIL) |"
  echo "| b126_exit | $history_exit | $([[ "$history_exit" == "0" ]] && echo PASS || echo FAIL) |"
  echo "| history_trend | $history_trend | $([[ "$history_trend" == "STABLE" ]] && echo PASS || echo FAIL) |"
  echo
  echo "## Artifacts"
  echo
  echo "- b125_report: $bundle_report"
  echo "- b126_report: $history_report"
  echo "- b125_log: $bundle_log"
  echo "- b126_log: $history_log"
} > "$OUTPUT_FILE"

if [[ "$QUIET" == "true" ]]; then
  echo "WAVE_C_LOCAL_GUARD status=$overall run_id=$RUN_ID workflow=$workflow_state trend=$history_trend"
else
  echo "[INFO] overall=$overall"
  echo "[PASS] report generated: $OUTPUT_FILE"
  echo "WAVE_C_LOCAL_GUARD status=$overall run_id=$RUN_ID workflow=$workflow_state trend=$history_trend"
fi

if [[ "$STRICT" == "true" && "$overall" != "PASS" ]]; then
  exit 1
fi

exit 0
