#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
STRICT=false
OUTPUT_FILE=""

usage() {
  cat <<'USAGE'
Wave C B149 CI Re-enable Submission Bundle

用途：
  一次执行 B146/B147/B148 并输出恢复 CI 审批提交打包报告。

用法：
  scripts/run_wave_c_ci_reenable_submission_bundle.sh [options]

选项：
  --run-id ID      指定 run_id
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
  OUTPUT_FILE="test-reports/wave_c_b149_ci_reenable_submission_bundle_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

b146_report="test-reports/wave_c_b146_ci_reenable_submission_pack_${RUN_ID}.md"
b147_report="test-reports/wave_c_b147_submission_pack_check_${RUN_ID}.md"
b148_report="test-reports/wave_c_b148_ci_reenable_approval_brief_${RUN_ID}.md"

run_step() {
  local cmd="$1"
  local log="$2"

  set +e
  eval "$cmd" > "$log" 2>&1
  local ec=$?
  set -e

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

b146_log="test-reports/wave_c_b146_ci_reenable_submission_pack_${RUN_ID}.b149.log"
b147_log="test-reports/wave_c_b147_submission_pack_check_${RUN_ID}.b149.log"
b148_log="test-reports/wave_c_b148_ci_reenable_approval_brief_${RUN_ID}.b149.log"

b146_exit=$(run_step "bash scripts/prepare_wave_c_ci_reenable_submission_pack.sh --run-id ${RUN_ID} --strict --output ${b146_report}" "$b146_log")
b147_exit=$(run_step "bash scripts/check_wave_c_ci_reenable_submission_pack.sh --run-id ${RUN_ID} --strict --input ${b146_report} --output ${b147_report}" "$b147_log")
b148_exit=$(run_step "bash scripts/generate_wave_c_ci_reenable_approval_brief.sh --run-id ${RUN_ID} --input ${b146_report} --output ${b148_report}" "$b148_log")

overall="PASS"
if [[ "$b146_exit" != "0" || "$b147_exit" != "0" || "$b148_exit" != "0" ]]; then
  overall="FAIL"
fi

submission_state="$(extract_marked_state "$b146_report" "submission_state")"
check_state="$(extract_marked_state "$b147_report" "check_state")"
brief_submission_state="$(extract_marked_state "$b148_report" "submission_state")"

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
