#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
OUTPUT_FILE=""
STRICT=false

usage() {
  cat <<'USAGE'
Wave C B146 CI Re-enable Submission Pack

用途：
  生成恢复 CI 的提交包（不执行 enable）。

用法：
  scripts/prepare_wave_c_ci_reenable_submission_pack.sh [options]

选项：
  --run-id ID      指定 run_id
  --output FILE    输出报告路径
  --strict         状态非 READY_TO_SUBMIT 返回非 0
  --help           显示帮助
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
  OUTPUT_FILE="test-reports/wave_c_b146_ci_reenable_submission_pack_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

latest_packet="$(ls -1t test-reports/wave_c_b137_pre_ci_reenable_packet_*.md 2>/dev/null | head -1 || true)"
latest_fullgate="$(ls -1t test-reports/wave_c_b138_pre_ci_reenable_full_gate_*.md 2>/dev/null | head -1 || true)"
latest_status_json="$(ls -1t test-reports/wave_c_b142_local_guard_status_*.json 2>/dev/null | head -1 || true)"
latest_alert="$(ls -1t test-reports/wave_c_b143_alert_thresholds_*.md 2>/dev/null | head -1 || true)"
latest_ops_pack="$(ls -1t test-reports/wave_c_b144_local_guard_ops_pack_*.md 2>/dev/null | head -1 || true)"

extract_marked_state() {
  local file="$1"
  local key="$2"
  if [[ -z "$file" || ! -f "$file" ]]; then
    echo "MISSING"
    return 0
  fi
  local value
  value="$(rg -o "${key}:[[:space:]]*\*\*[A-Z_]+\*\*" "$file" | head -1 | sed -E 's/.*\*\*([A-Z_]+)\*\*/\1/' || true)"
  echo "${value:-UNKNOWN}"
}

extract_json_value() {
  local file="$1"
  local key="$2"
  if [[ -z "$file" || ! -f "$file" ]]; then
    echo "MISSING"
    return 0
  fi
  sed -n -E "s/.*\"${key}\"[[:space:]]*:[[:space:]]*\"([^\"]*)\".*/\1/p" "$file" | head -1
}

workflow_state="UNKNOWN"
if [[ -f ".github/workflows/wave-c-quick-sprint-manual.yml.disabled" && ! -f ".github/workflows/wave-c-quick-sprint-manual.yml" ]]; then
  workflow_state="DISABLED"
elif [[ -f ".github/workflows/wave-c-quick-sprint-manual.yml" ]]; then
  workflow_state="ENABLED"
fi

packet_state="$(extract_marked_state "$latest_packet" "packet_state")"
fullgate_state="$(extract_marked_state "$latest_fullgate" "overall")"
alert_level="$(extract_marked_state "$latest_alert" "alert_level")"
ops_pack_state="$(extract_marked_state "$latest_ops_pack" "overall")"
status_overall="$(extract_json_value "$latest_status_json" "overall_state")"

submission_state="READY_TO_SUBMIT"
if [[ "$workflow_state" != "DISABLED" || "$packet_state" != "READY_FOR_APPROVAL" || "$fullgate_state" != "PASS" || "$alert_level" != "NONE" || "$ops_pack_state" != "PASS" || "$status_overall" != "HEALTHY" ]]; then
  submission_state="HOLD"
fi

{
  echo "# Wave C B146 CI Re-enable Submission Pack"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- submission_state: **$submission_state**"
  echo
  echo "## Inputs"
  echo
  echo "- packet_report: ${latest_packet:-<none>}"
  echo "- full_gate_report: ${latest_fullgate:-<none>}"
  echo "- status_json: ${latest_status_json:-<none>}"
  echo "- alert_report: ${latest_alert:-<none>}"
  echo "- ops_pack_report: ${latest_ops_pack:-<none>}"
  echo
  echo "## Gate Checks"
  echo
  echo "| check | value | expected | result |"
  echo "|------|-------|----------|--------|"
  echo "| workflow_state | $workflow_state | DISABLED | $([[ "$workflow_state" == "DISABLED" ]] && echo PASS || echo FAIL) |"
  echo "| packet_state | $packet_state | READY_FOR_APPROVAL | $([[ "$packet_state" == "READY_FOR_APPROVAL" ]] && echo PASS || echo FAIL) |"
  echo "| fullgate_state | $fullgate_state | PASS | $([[ "$fullgate_state" == "PASS" ]] && echo PASS || echo FAIL) |"
  echo "| status_overall | $status_overall | HEALTHY | $([[ "$status_overall" == "HEALTHY" ]] && echo PASS || echo FAIL) |"
  echo "| alert_level | $alert_level | NONE | $([[ "$alert_level" == "NONE" ]] && echo PASS || echo FAIL) |"
  echo "| ops_pack_state | $ops_pack_state | PASS | $([[ "$ops_pack_state" == "PASS" ]] && echo PASS || echo FAIL) |"
  echo
  echo "## Boundary"
  echo
  echo "- 未获批前，不执行 enable 操作。"
  echo "- 获批后，建议先 enable，再立即执行 oncall strict 复核。"
} > "$OUTPUT_FILE"

echo "[INFO] submission_state=$submission_state"
echo "[PASS] submission pack generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$submission_state" != "READY_TO_SUBMIT" ]]; then
  exit 1
fi

exit 0
