#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COMMON_LIB="$SCRIPT_DIR/wave_c_audit_note_common.sh"
if [[ ! -f "$COMMON_LIB" ]]; then
  echo "[ERROR] common lib not found: $COMMON_LIB" >&2
  exit 1
fi
# shellcheck source=/dev/null
source "$COMMON_LIB"

RUN_ID="$(date +%Y%m%d_%H%M%S)"
INPUT_FILE=""
OUTPUT_FILE=""
STRICT=false
REPORTS_DIR="${FAFAFA_WAVE_C_CI_REENABLE_REPORTS_DIR:-tmp/wave_c_ci_reenable_reports}"

usage() {
  cat <<'USAGE'
Wave C B147 CI Re-enable Submission Pack Check

用途：
  检查 B146 提交包完整性与关键状态。

用法：
  scripts/check_wave_c_ci_reenable_submission_pack.sh [options]

选项：
  --run-id ID      指定 run_id
  --input FILE     指定 B146 提交包（默认最新）
  --output FILE    输出检查报告（默认 tmp/wave_c_ci_reenable_reports/wave_c_b147_submission_pack_check_<run_id>.md）
  --strict         check_state 非 PASS 返回非 0
  --help           显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --input)
      INPUT_FILE="$2"
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

if [[ -z "$INPUT_FILE" ]]; then
  INPUT_FILE="$(ls -1t "$REPORTS_DIR"/wave_c_b146_ci_reenable_submission_pack_*.md 2>/dev/null | head -1 || true)"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$REPORTS_DIR/wave_c_b147_submission_pack_check_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

if [[ -z "$INPUT_FILE" || ! -f "$INPUT_FILE" ]]; then
  echo "[ERROR] submission pack not found" >&2
  exit 1
fi

checks=(
  "submission_state"
  "workflow_state"
  "packet_state"
  "fullgate_state"
  "status_overall"
  "alert_level"
  "ops_pack_state"
)

missing=0
rows=""
for token in "${checks[@]}"; do
  if rg -q "$token" "$INPUT_FILE"; then
    rows+="| $token | PASS |\n"
  else
    rows+="| $token | FAIL |\n"
    missing=1
  fi
done

ready_state="$(rg -o "submission_state:[[:space:]]*\*\*[A-Z_]+\*\*" "$INPUT_FILE" | head -1 | sed -E 's/.*\*\*([A-Z_]+)\*\*/\1/' || true)"
ready_state="${ready_state:-UNKNOWN}"

check_state="PASS"
if [[ "$missing" -ne 0 || "$ready_state" != "READY_TO_SUBMIT" ]]; then
  check_state="FAIL"
fi

projected_b148_alert_state="CLEAR"
if [[ "$check_state" != "PASS" ]]; then
  projected_b148_alert_state="WARN"
fi

projected_b149_audit_alert_note="$(wave_c_map_alert_state_to_audit_note "$projected_b148_alert_state")"

{
  echo "# Wave C B147 Submission Pack Check"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- input_file: $INPUT_FILE"
  echo "- check_state: **$check_state**"
  echo
  echo "## Token Checks"
  echo
  echo "| token | result |"
  echo "|-------|--------|"
  printf "%b" "$rows"
  echo
  echo "## Downstream Audit Projection"
  echo
  echo "- projected_b148_alert_state: **$projected_b148_alert_state**"
  echo "- projected_b149_audit_alert_note: **$projected_b149_audit_alert_note**"
  echo
  echo "## Decision"
  echo
  echo "- submission_state: $ready_state"
} > "$OUTPUT_FILE"

echo "[INFO] check_state=$check_state"
echo "[PASS] check report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$check_state" != "PASS" ]]; then
  exit 1
fi

exit 0
