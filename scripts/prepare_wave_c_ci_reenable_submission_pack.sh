#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
OUTPUT_FILE=""
STRICT=false
SIGNOFF_RECORD=""
PREREQ_REPORT=""
PACKET_REPORT=""

usage() {
  cat <<'USAGE'
Wave C B146 CI Re-enable Submission Pack

用途：
 基于 signoff/prereq/enablement packet 生成恢复 CI 的审批提交包（不执行 enable）。

用法：
  scripts/prepare_wave_c_ci_reenable_submission_pack.sh [options]

选项：
  --run-id ID            指定 run_id
  --signoff-record FILE  指定 B113 signoff record
  --prereq-report FILE   指定 B115 prereq report
  --packet-report FILE   指定 B116 enablement packet
  --output FILE          输出报告路径
  --strict               状态非 READY_TO_SUBMIT 返回非 0
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
    --prereq-report)
      PREREQ_REPORT="$2"
      shift 2
      ;;
    --packet-report)
      PACKET_REPORT="$2"
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
  OUTPUT_FILE="docs/test_reports/WAVE_C_B146_CI_REENABLE_SUBMISSION_PACK_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

if [[ -z "$SIGNOFF_RECORD" ]]; then
  SIGNOFF_RECORD="$(ls -1t docs/test_reports/WAVE_C_B113_RELEASE_SIGNOFF_RECORD_*.md 2>/dev/null | head -1 || true)"
fi
if [[ -z "$PREREQ_REPORT" ]]; then
  PREREQ_REPORT="$(ls -1t tmp/test-reports/wave_c_b115_workflow_enable_prereq_*.md 2>/dev/null | head -1 || true)"
fi
if [[ -z "$PREREQ_REPORT" ]]; then
  PREREQ_REPORT="$(ls -1t test-reports/wave_c_b115_workflow_enable_prereq_*.md 2>/dev/null | head -1 || true)"
fi
if [[ -z "$PACKET_REPORT" ]]; then
  PACKET_REPORT="$(ls -1t docs/test_reports/WAVE_C_B116_ENABLEMENT_REQUEST_PACKET_*.md 2>/dev/null | head -1 || true)"
fi

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

extract_value_after_colon() {
  local file="$1"
  local key="$2"
  if [[ -z "$file" || ! -f "$file" ]]; then
    echo "MISSING"
    return 0
  fi
  grep -E -- "- ${key}:" "$file" | head -1 | sed -E "s/.*- ${key}:[[:space:]]*//" | sed -E 's/\*\*//g'
}

workflow_state="UNKNOWN"
if [[ -f ".github/workflows/wave-c-quick-sprint-manual.yml.disabled" && ! -f ".github/workflows/wave-c-quick-sprint-manual.yml" ]]; then
  workflow_state="DISABLED"
elif [[ -f ".github/workflows/wave-c-quick-sprint-manual.yml" ]]; then
  workflow_state="ENABLED"
fi

signoff_state="$(extract_value_after_colon "$SIGNOFF_RECORD" "signoff_state")"
enable_state="$(extract_value_after_colon "$PREREQ_REPORT" "enable_state")"
packet_signoff_state="$(extract_value_after_colon "$PACKET_REPORT" "signoff_state")"
packet_enable_state="$(extract_value_after_colon "$PACKET_REPORT" "enable_state")"

submission_state="READY_TO_SUBMIT"
if [[ "$workflow_state" != "DISABLED" || "$signoff_state" != "READY_FOR_APPROVAL" || "$enable_state" != "HOLD" || "$packet_signoff_state" == "MISSING" || "$packet_enable_state" == "MISSING" ]]; then
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
  echo "- signoff_record: ${SIGNOFF_RECORD:-<none>}"
  echo "- prereq_report: ${PREREQ_REPORT:-<none>}"
  echo "- packet_report: ${PACKET_REPORT:-<none>}"
  echo
  echo "## Gate Checks"
  echo
  echo "| check | value | expected | result |"
  echo "|------|-------|----------|--------|"
  echo "| workflow_state | $workflow_state | DISABLED | $([[ "$workflow_state" == "DISABLED" ]] && echo PASS || echo FAIL) |"
  echo "| signoff_state | $signoff_state | READY_FOR_APPROVAL | $([[ "$signoff_state" == "READY_FOR_APPROVAL" ]] && echo PASS || echo FAIL) |"
  echo "| enable_state | $enable_state | HOLD | $([[ "$enable_state" == "HOLD" ]] && echo PASS || echo FAIL) |"
  echo "| packet_signoff_state | $packet_signoff_state | READY_FOR_APPROVAL | $([[ "$packet_signoff_state" == "READY_FOR_APPROVAL" ]] && echo PASS || echo FAIL) |"
  echo "| packet_enable_state | $packet_enable_state | HOLD | $([[ "$packet_enable_state" == "HOLD" ]] && echo PASS || echo FAIL) |"
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
