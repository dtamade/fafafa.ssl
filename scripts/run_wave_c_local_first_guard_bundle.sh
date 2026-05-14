#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
STRICT=false
REPORTS_DIR="tmp/test-reports"
OUTPUT_FILE=""

usage() {
  cat <<'USAGE'
Wave C B125 Local-First Guard Bundle

用途：
  一次执行 B123 + B124 本地优先守护门禁，生成汇总报告。

用法：
  scripts/run_wave_c_local_first_guard_bundle.sh [options]

选项：
  --run-id ID       指定 run_id
  --reports-dir DIR 报告目录（默认 tmp/test-reports）
  --output FILE     输出汇总报告
  --strict          任一步骤失败返回非 0
  --help            显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --reports-dir)
      REPORTS_DIR="$2"
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
  OUTPUT_FILE="$REPORTS_DIR/wave_c_b125_local_guard_bundle_${RUN_ID}.md"
fi

mkdir -p "$REPORTS_DIR"

continuity_report="$REPORTS_DIR/wave_c_b123_local_first_continuity_${RUN_ID}.md"
drift_report="$REPORTS_DIR/wave_c_b124_local_drift_watch_${RUN_ID}.md"

continuity_log="$REPORTS_DIR/wave_c_b123_local_first_continuity_${RUN_ID}.log"
drift_log="$REPORTS_DIR/wave_c_b124_local_drift_watch_${RUN_ID}.log"

shell_join() {
  local parts=()
  local part
  for part in "$@"; do
    parts+=("$(printf '%q' "$part")")
  done
  local IFS=' '
  echo "${parts[*]}"
}

run_step() {
  local step_name="$1"
  local log="$2"
  local cmd_desc="$3"
  shift 3

  echo "[wave-c-b125] [$step_name] $cmd_desc" >&2

  set +e
  "$@" > "$log" 2>&1
  local ec=$?
  set -e

  echo "[wave-c-b125] [$step_name] exit=$ec log=$log" >&2
  echo "$ec"
}

b123_cmd_words=(
  bash
  scripts/check_wave_c_local_first_continuity.sh
  --run-id "$RUN_ID"
  --strict
  --output "$continuity_report"
)
continuity_exit=$(run_step \
  "b123_local_first_continuity" \
  "$continuity_log" \
  "$(shell_join "${b123_cmd_words[@]}")" \
  "${b123_cmd_words[@]}")

b124_cmd_words=(
  bash
  scripts/check_wave_c_local_drift_watch.sh
  --run-id "$RUN_ID"
  --strict
  --output "$drift_report"
)
drift_exit=$(run_step \
  "b124_local_drift_watch" \
  "$drift_log" \
  "$(shell_join "${b124_cmd_words[@]}")" \
  "${b124_cmd_words[@]}")

overall="PASS"
if [[ "$continuity_exit" != "0" || "$drift_exit" != "0" ]]; then
  overall="FAIL"
fi

continuity_state="UNKNOWN"
if [[ -f "$continuity_report" ]]; then
  continuity_state="$(rg -o "local_first_state:[[:space:]]*\*\*[A-Z_]+\*\*" "$continuity_report" | head -1 | sed -E 's/.*\*\*([A-Z_]+)\*\*/\1/' || true)"
  continuity_state="${continuity_state:-UNKNOWN}"
fi

drift_state="UNKNOWN"
if [[ -f "$drift_report" ]]; then
  drift_state="$(rg -o "local_drift_state:[[:space:]]*\*\*[A-Z_]+\*\*" "$drift_report" | head -1 | sed -E 's/.*\*\*([A-Z_]+)\*\*/\1/' || true)"
  drift_state="${drift_state:-UNKNOWN}"
fi

{
  echo "# Wave C B125 Local-First Guard Bundle"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- overall: **$overall**"
  echo
  echo "## Step Matrix"
  echo
  echo "| step | exit | state | report | log |"
  echo "|------|------|-------|--------|-----|"
  echo "| B123 local continuity | $continuity_exit | $continuity_state | $continuity_report | $continuity_log |"
  echo "| B124 local drift watch | $drift_exit | $drift_state | $drift_report | $drift_log |"
  echo
  echo "## Decision"
  echo
  if [[ "$overall" == "PASS" ]]; then
    echo "- local-first guard chain is stable."
  else
    echo "- local-first guard chain is blocked; fix failed step before next iteration."
  fi
} > "$OUTPUT_FILE"

echo "[INFO] overall=$overall"
echo "[PASS] bundle report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$overall" != "PASS" ]]; then
  exit 1
fi

exit 0
