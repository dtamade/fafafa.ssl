#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
INPUT_FILE=""
OUTPUT_FILE=""
STRICT=false

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
  --output FILE    输出检查报告
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
  INPUT_FILE="$(ls -1t docs/test_reports/WAVE_C_B146_CI_REENABLE_SUBMISSION_PACK_*.md 2>/dev/null | head -1 || true)"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="docs/test_reports/WAVE_C_B147_SUBMISSION_PACK_CHECK_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

if [[ -z "$INPUT_FILE" || ! -f "$INPUT_FILE" ]]; then
  echo "[ERROR] submission pack not found" >&2
  exit 1
fi

checks=(
  "submission_state"
  "workflow_state"
  "signoff_state"
  "enable_state"
  "packet_signoff_state"
  "packet_enable_state"
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
