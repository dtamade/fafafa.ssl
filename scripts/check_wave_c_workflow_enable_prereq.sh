#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
STRICT=false
SIGNOFF_RECORD="docs/test_reports/WAVE_C_B113_RELEASE_SIGNOFF_RECORD_2026-02-08.md"
ACCEPTANCE_REPORT="docs/test_reports/WAVE_C_B114_CLOSURE_ACCEPTANCE_RESULT_2026-02-08.md"
WORKFLOW_TEMPLATE=".github/workflows/wave-c-quick-sprint-manual.yml.disabled"
OUTPUT_FILE=""

usage() {
  cat <<'USAGE'
Wave C B115 Workflow Enable Prereq Check

用途：
  在启用手动 CI workflow 前，检查签核与验收前置条件。

用法：
  scripts/check_wave_c_workflow_enable_prereq.sh [options]

选项：
  --run-id ID            指定 run_id
  --signoff-record FILE  指定签核记录
  --acceptance FILE      指定闭环验收报告
  --workflow FILE        指定 workflow 模板文件
  --output FILE          输出报告路径
  --strict               状态非 READY_FOR_ENABLE 返回非 0
  --help                 显示帮助
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
    --acceptance)
      ACCEPTANCE_REPORT="$2"
      shift 2
      ;;
    --workflow)
      WORKFLOW_TEMPLATE="$2"
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
  OUTPUT_FILE="test-reports/wave_c_b115_workflow_enable_prereq_${RUN_ID}.md"
fi

check_file_state() {
  local file="$1"
  if [[ -f "$file" ]]; then
    echo "PASS"
  else
    echo "FAIL"
  fi
}

extract_value_after_colon() {
  local file="$1"
  local key="$2"
  grep -E -- "- ${key}:" "$file" | head -1 | sed -E "s/.*- ${key}:[[:space:]]*//"
}

signoff_state="UNKNOWN"
if [[ -f "$SIGNOFF_RECORD" ]]; then
  signoff_state="$(extract_value_after_colon "$SIGNOFF_RECORD" "signoff_state")"
fi

acceptance_hint="UNKNOWN"
if [[ -f "$ACCEPTANCE_REPORT" ]]; then
  if rg -q "Quick bundle overall" "$ACCEPTANCE_REPORT"; then
    acceptance_hint="PASS"
  else
    acceptance_hint="FAIL"
  fi
fi

signoff_check="FAIL"
if [[ "$signoff_state" == "APPROVED" ]]; then
  signoff_check="PASS"
fi

acceptance_check="FAIL"
if [[ "$acceptance_hint" == "PASS" ]]; then
  acceptance_check="PASS"
fi

workflow_check="$(check_file_state "$WORKFLOW_TEMPLATE")"

enable_state="READY_FOR_ENABLE"
if [[ "$signoff_check" != "PASS" || "$acceptance_check" != "PASS" || "$workflow_check" != "PASS" ]]; then
  enable_state="HOLD"
fi

{
  echo "# Wave C B115 Workflow Enable Prereq"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- signoff_record: $SIGNOFF_RECORD"
  echo "- acceptance_report: $ACCEPTANCE_REPORT"
  echo "- workflow_template: $WORKFLOW_TEMPLATE"
  echo "- enable_state: **$enable_state**"
  echo
  echo "## Checks"
  echo
  echo "| check | value | result |"
  echo "|------|-------|--------|"
  echo "| signoff_state | $signoff_state | $signoff_check |"
  echo "| acceptance_bundle | $acceptance_hint | $acceptance_check |"
  echo "| workflow_template_exists | $WORKFLOW_TEMPLATE | $workflow_check |"
} > "$OUTPUT_FILE"

echo "[INFO] enable_state=$enable_state"
echo "[PASS] report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$enable_state" != "READY_FOR_ENABLE" ]]; then
  exit 1
fi

exit 0
