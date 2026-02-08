#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
ONCALL_REPORT=""
SNAPSHOT_REPORT=""
OUTPUT_FILE=""
STRICT=false

WORKFLOW_ENABLED_FILE=".github/workflows/wave-c-quick-sprint-manual.yml"
WORKFLOW_DISABLED_FILE=".github/workflows/wave-c-quick-sprint-manual.yml.disabled"

usage() {
  cat <<'USAGE'
Wave C B137 Pre-CI Re-enable Packet Builder

用途：
  在不启用 workflow 的前提下，生成“恢复 CI 前审批检查包”。

用法：
  scripts/prepare_wave_c_b137_pre_ci_reenable_packet.sh [options]

选项：
  --run-id ID            指定 run_id
  --oncall-report FILE   指定 B129 oncall 报告（默认最新）
  --snapshot-report FILE 指定 B132 snapshot 报告（默认最新）
  --output FILE          输出报告路径
  --strict               状态非 READY_FOR_APPROVAL 返回非 0
  --help                 显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --oncall-report)
      ONCALL_REPORT="$2"
      shift 2
      ;;
    --snapshot-report)
      SNAPSHOT_REPORT="$2"
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

if [[ -z "$ONCALL_REPORT" ]]; then
  ONCALL_REPORT="$(ls -1t test-reports/wave_c_b129_oncall_check_*.md 2>/dev/null | head -1 || true)"
fi

if [[ -z "$SNAPSHOT_REPORT" ]]; then
  SNAPSHOT_REPORT="$(ls -1t test-reports/wave_c_b132_local_first_status_snapshot_*.md 2>/dev/null | head -1 || true)"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="test-reports/wave_c_b137_pre_ci_reenable_packet_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

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

workflow_state="MISSING"
if [[ -f "$WORKFLOW_DISABLED_FILE" && ! -f "$WORKFLOW_ENABLED_FILE" ]]; then
  workflow_state="DISABLED"
elif [[ -f "$WORKFLOW_ENABLED_FILE" && ! -f "$WORKFLOW_DISABLED_FILE" ]]; then
  workflow_state="ENABLED"
elif [[ -f "$WORKFLOW_ENABLED_FILE" && -f "$WORKFLOW_DISABLED_FILE" ]]; then
  workflow_state="AMBIGUOUS"
fi

oncall_state="$(extract_marked_state "$ONCALL_REPORT" "overall")"
snapshot_state="$(extract_marked_state "$SNAPSHOT_REPORT" "snapshot_state")"

packet_state="READY_FOR_APPROVAL"
if [[ "$workflow_state" != "DISABLED" || "$oncall_state" != "PASS" || "$snapshot_state" != "GREEN" ]]; then
  packet_state="HOLD"
fi

suggested_action="保持 local-first；提交审批后再考虑 enable"
if [[ "$packet_state" == "HOLD" ]]; then
  suggested_action="先修复 FAIL/HOLD 项，再提交审批"
fi

{
  echo "# Wave C B137 Pre-CI Re-enable Packet"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- packet_state: **$packet_state**"
  echo
  echo "## Inputs"
  echo
  echo "- oncall_report: ${ONCALL_REPORT:-<none>}"
  echo "- snapshot_report: ${SNAPSHOT_REPORT:-<none>}"
  echo
  echo "## Checks"
  echo
  echo "| check | value | expected | result |"
  echo "|------|-------|----------|--------|"
  echo "| workflow_state | $workflow_state | DISABLED | $([[ "$workflow_state" == "DISABLED" ]] && echo PASS || echo FAIL) |"
  echo "| oncall_state | $oncall_state | PASS | $([[ "$oncall_state" == "PASS" ]] && echo PASS || echo FAIL) |"
  echo "| snapshot_state | $snapshot_state | GREEN | $([[ "$snapshot_state" == "GREEN" ]] && echo PASS || echo FAIL) |"
  echo
  echo "## Approval Boundary"
  echo
  echo "- 未获批前，不执行：\`bash scripts/toggle_wave_c_quick_sprint_workflow.sh enable\`"
  echo "- 获批后，先 enable 再立即执行 oncall strict 复核。"
  echo
  echo "## Suggested Action"
  echo
  echo "- $suggested_action"
} > "$OUTPUT_FILE"

echo "[INFO] packet_state=$packet_state"
echo "[PASS] report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$packet_state" != "READY_FOR_APPROVAL" ]]; then
  exit 1
fi

exit 0
