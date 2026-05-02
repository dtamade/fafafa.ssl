#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
INPUT_FILE=""
OUTPUT_FILE=""

usage() {
  cat <<'USAGE'
Wave C B148 CI Re-enable Approval Brief

用途：
  基于 B146 提交包或 B116 enablement packet 生成审批简报（单页）。

用法：
  scripts/generate_wave_c_ci_reenable_approval_brief.sh [options]

选项：
  --run-id ID      指定 run_id
  --input FILE     指定 B146 提交包（默认最新）
  --output FILE    输出简报路径
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
  INPUT_FILE="$(ls -1t docs/test_reports/WAVE_C_B116_ENABLEMENT_REQUEST_PACKET_*.md 2>/dev/null | head -1 || true)"
fi

if [[ -z "$INPUT_FILE" ]]; then
  INPUT_FILE="$(ls -1t docs/test_reports/WAVE_C_B146_CI_REENABLE_SUBMISSION_PACK_*.md 2>/dev/null | head -1 || true)"
fi

if [[ -z "$INPUT_FILE" ]]; then
  INPUT_FILE="$(ls -1t test-reports/wave_c_b146_ci_reenable_submission_pack_*.md 2>/dev/null | head -1 || true)"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="docs/test_reports/WAVE_C_B148_CI_REENABLE_APPROVAL_BRIEF_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

if [[ -z "$INPUT_FILE" || ! -f "$INPUT_FILE" ]]; then
  echo "[ERROR] approval input pack not found" >&2
  exit 1
fi

extract_marked_state() {
  local key="$1"
  rg -o "${key}:[[:space:]]*\*\*[A-Z_]+\*\*" "$INPUT_FILE" | head -1 | sed -E 's/.*\*\*([A-Z_]+)\*\*/\1/' || true
}

extract_value_after_colon() {
  local key="$1"
  grep -E -- "- ${key}:" "$INPUT_FILE" | head -1 | sed -E "s/.*- ${key}:[[:space:]]*//" | sed -E 's/\*\*//g'
}

submission_state="$(extract_marked_state submission_state)"
enable_state="$(extract_value_after_colon enable_state)"
signoff_state="$(extract_value_after_colon signoff_state)"

brief_state="UNKNOWN"
brief_summary="当前尚不满足审批提交条件，建议先修复门禁项。"

if [[ -n "$submission_state" && "$submission_state" != "UNKNOWN" ]]; then
  brief_state="$submission_state"
  if [[ "$submission_state" == "READY_TO_SUBMIT" ]]; then
    brief_summary="当前已满足提交审批条件，建议发起恢复 CI 审批。"
  fi
elif [[ "$signoff_state" == "READY_FOR_APPROVAL" || "$signoff_state" == "APPROVED" ]]; then
  brief_state="$signoff_state"
  if [[ "$signoff_state" == "READY_FOR_APPROVAL" && "$enable_state" == "HOLD" ]]; then
    brief_summary="当前技术证据链已完成，但仍待人工签核；建议提交审批，保持 workflow disabled。"
  elif [[ "$signoff_state" == "APPROVED" && "$enable_state" == "READY_FOR_ENABLE" ]]; then
    brief_summary="当前已满足审批后启用条件，可进入受控启用动作。"
  fi
fi

{
  echo "# Wave C B148 CI Re-enable Approval Brief"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- source_pack: $INPUT_FILE"
  echo "- brief_state: **$brief_state**"
  if [[ -n "$signoff_state" ]]; then
    echo "- signoff_state: $signoff_state"
  fi
  if [[ -n "$enable_state" ]]; then
    echo "- enable_state: $enable_state"
  fi
  echo
  echo "## Executive Summary"
  echo
  echo "- $brief_summary"
  echo
  echo "## Recommended Next Step"
  echo
  echo "- 审批前：保持 workflow disabled。"
  echo "- 审批后：再执行 enable，并立即做 oncall strict 复核。"
} > "$OUTPUT_FILE"

echo "[PASS] approval brief generated: $OUTPUT_FILE"
exit 0
