#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
STRICT=false
OUTPUT_FILE=""
LOGS_DIR="tmp/test-reports"
SIGNOFF_RECORD=""
PREREQ_REPORT=""
PACKET_REPORT=""
APPROVAL_INPUT=""

usage() {
  cat <<'USAGE'
Wave C B149 CI Re-enable Submission Bundle

用途：
  一次执行 B146/B147/B148 并输出恢复 CI 审批提交打包报告。

用法：
  scripts/run_wave_c_ci_reenable_submission_bundle.sh [options]

选项：
  --run-id ID      指定 run_id
  --signoff-record FILE 指定 B113 signoff record
  --prereq-report FILE  指定 B115 prereq report
  --packet-report FILE  指定 B116 enablement packet
  --approval-input FILE 指定 B148 输入文件（默认使用 B116 packet）
  --logs-dir DIR    指定 step log 输出目录（默认 tmp/test-reports）
  --output FILE    输出报告路径
  --strict         任一步骤失败返回非 0
  --help           显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --signoff-record)
      SIGNOFF_RECORD="$2"
      shift 2
      ;;
    --prereq-report)
      PREREQ_REPORT="$2"
      shift 2
      ;;
    --packet-report)
      PACKET_REPORT="$2"
      shift 2
      ;;
    --approval-input)
      APPROVAL_INPUT="$2"
      shift 2
      ;;
    --logs-dir)
      LOGS_DIR="$2"
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
  OUTPUT_FILE="docs/test_reports/WAVE_C_B149_CI_REENABLE_SUBMISSION_BUNDLE_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"
mkdir -p "$LOGS_DIR"

b146_report="docs/test_reports/WAVE_C_B146_CI_REENABLE_SUBMISSION_PACK_${RUN_ID}.md"
b147_report="docs/test_reports/WAVE_C_B147_SUBMISSION_PACK_CHECK_${RUN_ID}.md"
b148_report="docs/test_reports/WAVE_C_B148_CI_REENABLE_APPROVAL_BRIEF_${RUN_ID}.md"

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

  echo "[wave-c-b149] [$step_name] $cmd_desc" >&2

  set +e
  "$@" > "$log" 2>&1
  local ec=$?
  set -e

  echo "[wave-c-b149] [$step_name] exit=$ec log=$log" >&2
  echo "$ec"
}

extract_marked_state() {
  local file="$1"
  local key="$2"
  if [[ ! -f "$file" ]]; then
    echo "MISSING"
    return 0
  fi

  local value
  value="$(rg -o "${key}:[[:space:]]*\*\*[A-Z_]+\*\*" "$file" | head -1 | sed -E 's/.*\*\*([A-Z_]+)\*\*/\1/' || true)"
  echo "${value:-UNKNOWN}"
}

b146_log="$LOGS_DIR/wave_c_b146_ci_reenable_submission_pack_${RUN_ID}.b149.log"
b147_log="$LOGS_DIR/wave_c_b147_submission_pack_check_${RUN_ID}.b149.log"
b148_log="$LOGS_DIR/wave_c_b148_ci_reenable_approval_brief_${RUN_ID}.b149.log"

if [[ -z "$SIGNOFF_RECORD" ]]; then
  SIGNOFF_RECORD="$(ls -1t docs/test_reports/WAVE_C_B113_RELEASE_SIGNOFF_RECORD_*.md 2>/dev/null | head -1 || true)"
fi
if [[ -z "$PREREQ_REPORT" ]]; then
  PREREQ_REPORT="$(ls -1t tmp/test-reports/wave_c_b115_workflow_enable_prereq_*.md 2>/dev/null | head -1 || true)"
fi
if [[ -z "$PACKET_REPORT" ]]; then
  PACKET_REPORT="$(ls -1t docs/test_reports/WAVE_C_B116_ENABLEMENT_REQUEST_PACKET_*.md 2>/dev/null | head -1 || true)"
fi
if [[ -z "$APPROVAL_INPUT" ]]; then
  APPROVAL_INPUT="$PACKET_REPORT"
fi

b146_cmd_words=(
  bash
  scripts/prepare_wave_c_ci_reenable_submission_pack.sh
  --run-id "$RUN_ID"
  --signoff-record "$SIGNOFF_RECORD"
  --prereq-report "$PREREQ_REPORT"
  --packet-report "$PACKET_REPORT"
  --strict
  --output "$b146_report"
)
b147_cmd_words=(
  bash
  scripts/check_wave_c_ci_reenable_submission_pack.sh
  --run-id "$RUN_ID"
  --strict
  --input "$b146_report"
  --output "$b147_report"
)
b148_cmd_words=(
  bash
  scripts/generate_wave_c_ci_reenable_approval_brief.sh
  --run-id "$RUN_ID"
  --input "$APPROVAL_INPUT"
  --output "$b148_report"
)

b146_exit=$(run_step "b146_submission_pack" "$b146_log" "$(shell_join "${b146_cmd_words[@]}")" "${b146_cmd_words[@]}")
b147_exit=$(run_step "b147_pack_check" "$b147_log" "$(shell_join "${b147_cmd_words[@]}")" "${b147_cmd_words[@]}")
b148_exit=$(run_step "b148_approval_brief" "$b148_log" "$(shell_join "${b148_cmd_words[@]}")" "${b148_cmd_words[@]}")

overall="PASS"
if [[ "$b146_exit" != "0" || "$b147_exit" != "0" || "$b148_exit" != "0" ]]; then
  overall="FAIL"
fi

submission_state="$(extract_marked_state "$b146_report" "submission_state")"
check_state="$(extract_marked_state "$b147_report" "check_state")"
brief_submission_state="$(extract_marked_state "$b148_report" "brief_state")"

{
  echo "# Wave C B149 CI Re-enable Submission Bundle"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- overall: **$overall**"
  echo "- submission_state: **$submission_state**"
  echo "- check_state: **$check_state**"
  echo
  echo "## Step Matrix"
  echo
  echo "| step | exit | output | log |"
  echo "|------|------|--------|-----|"
  echo "| B146 submission pack | $b146_exit | $b146_report | $b146_log |"
  echo "| B147 pack check | $b147_exit | $b147_report | $b147_log |"
  echo "| B148 approval brief | $b148_exit | $b148_report | $b148_log |"
  echo
  echo "## Summary"
  echo
  echo "- b146_submission_state: $submission_state"
  echo "- b147_check_state: $check_state"
  echo "- b148_submission_state: $brief_submission_state"
  echo "- boundary: 保持 workflow disabled，待审批后再执行 enable。"
} > "$OUTPUT_FILE"

echo "[INFO] overall=$overall"
echo "[PASS] submission bundle report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$overall" != "PASS" ]]; then
  exit 1
fi

exit 0
