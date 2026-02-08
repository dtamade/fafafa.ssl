#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
SIGNOFF_RECORD="docs/test_reports/WAVE_C_B113_RELEASE_SIGNOFF_RECORD_2026-02-08.md"
PREREQ_REPORT=""
OUTPUT_FILE=""

usage() {
  cat <<'USAGE'
Wave C B116 Enablement Packet Builder

用途：
  生成提交给人工审批的 workflow 启用请求包。

用法：
  scripts/prepare_wave_c_b116_enablement_packet.sh [options]

选项：
  --run-id ID            指定 run_id
  --signoff-record FILE  指定签核记录
  --prereq-report FILE   指定 B115 前置检查报告（默认最新）
  --output FILE          输出报告路径
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
    --output)
      OUTPUT_FILE="$2"
      shift 2
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

if [[ -z "$PREREQ_REPORT" ]]; then
  PREREQ_REPORT="$(ls -1t test-reports/wave_c_b115_workflow_enable_prereq_*.md 2>/dev/null | head -1 || true)"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="docs/test_reports/WAVE_C_B116_ENABLEMENT_REQUEST_PACKET_2026-02-08.md"
fi

extract_value_after_colon() {
  local file="$1"
  local key="$2"
  grep -E -- "- ${key}:" "$file" | head -1 | sed -E "s/.*- ${key}:[[:space:]]*//"
}

signoff_state="UNKNOWN"
if [[ -f "$SIGNOFF_RECORD" ]]; then
  signoff_state="$(extract_value_after_colon "$SIGNOFF_RECORD" "signoff_state")"
fi

enable_state="UNKNOWN"
if [[ -n "$PREREQ_REPORT" && -f "$PREREQ_REPORT" ]]; then
  enable_state="$(extract_value_after_colon "$PREREQ_REPORT" "enable_state" | sed -E 's/\*\*//g')"
fi

{
  echo "# Wave C B116 Enablement Request Packet"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- signoff_record: $SIGNOFF_RECORD"
  echo "- prereq_report: $PREREQ_REPORT"
  echo "- signoff_state: $signoff_state"
  echo "- enable_state: $enable_state"
  echo
  echo "## Request"
  echo
  echo "请审批是否允许启用 Wave C 手动 workflow 模板："
  echo "- source: .github/workflows/wave-c-quick-sprint-manual.yml.disabled"
  echo "- target: .github/workflows/wave-c-quick-sprint-manual.yml"
  echo
  echo "## Approval Rules"
  echo
  echo "1. 若 signoff_state != APPROVED，则不得启用。"
  echo "2. 若 enable_state != READY_FOR_ENABLE，则不得启用。"
  echo "3. 启用后仅允许 workflow_dispatch 手动触发。"
  echo
  echo "## Suggested Action"
  echo
  if [[ "$signoff_state" == "APPROVED" && "$enable_state" == "READY_FOR_ENABLE" ]]; then
    echo "- 建议：可以启用 workflow。"
  else
    echo "- 建议：保持禁用，等待人工签核完成。"
  fi
} > "$OUTPUT_FILE"

echo "[PASS] packet generated: $OUTPUT_FILE"

exit 0
