#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
INPUT_JSON=""
OUTPUT_FILE=""
STRICT=false
LOCAL_GUARD_REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-tmp/wave_c_local_guard_reports}"
REPORTS_DIR="${FAFAFA_WAVE_C_CI_REENABLE_REPORTS_DIR:-tmp/wave_c_ci_reenable_reports}"

usage() {
  cat <<'USAGE'
Wave C B143 Local Guard Alert Thresholds

用途：
  基于 B142 状态 JSON 生成告警等级判定（NONE/WARN/CRITICAL）。

用法：
  scripts/check_wave_c_local_guard_alert_thresholds.sh [options]

选项：
  --run-id ID       指定 run_id
  --input FILE      指定 B142 JSON（默认最新）
  --output FILE     输出报告路径（默认 tmp/wave_c_ci_reenable_reports/wave_c_b143_alert_thresholds_<run_id>.md）
  --strict          alert_level 非 NONE 返回非 0
  --help            显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --input)
      INPUT_JSON="$2"
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

if [[ -z "$INPUT_JSON" ]]; then
  INPUT_JSON="$(ls -1t "$LOCAL_GUARD_REPORTS_DIR"/wave_c_b142_local_guard_status_*.json 2>/dev/null | head -1 || true)"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$REPORTS_DIR/wave_c_b143_alert_thresholds_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

if [[ -z "$INPUT_JSON" || ! -f "$INPUT_JSON" ]]; then
  echo "[ERROR] status json not found" >&2
  exit 1
fi

extract_json_value() {
  local key="$1"
  sed -n -E "s/.*\"${key}\"[[:space:]]*:[[:space:]]*\"([^\"]*)\".*/\1/p" "$INPUT_JSON" | head -1
}

overall_state="$(extract_json_value overall_state)"
workflow_state="$(extract_json_value workflow_state)"
full_gate_state="$(extract_json_value full_gate_state)"
consistency_state="$(extract_json_value consistency_state)"

alert_level="NONE"
reason="all checks healthy"

if [[ "$overall_state" != "HEALTHY" ]]; then
  alert_level="WARN"
  reason="overall_state is not HEALTHY"
fi

if [[ "$workflow_state" != "DISABLED" || "$full_gate_state" != "PASS" || "$consistency_state" != "CONSISTENT" ]]; then
  alert_level="CRITICAL"
  reason="workflow/full_gate/consistency critical mismatch"
fi

{
  echo "# Wave C B143 Alert Thresholds"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- input_json: $INPUT_JSON"
  echo "- alert_level: **$alert_level**"
  echo "- reason: $reason"
} > "$OUTPUT_FILE"

echo "[INFO] alert_level=$alert_level"
echo "[PASS] alert report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$alert_level" != "NONE" ]]; then
  exit 1
fi

exit 0
