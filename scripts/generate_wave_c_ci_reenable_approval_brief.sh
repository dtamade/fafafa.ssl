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
CHECK_FILE=""
OUTPUT_FILE=""
REPORTS_DIR="${FAFAFA_WAVE_C_CI_REENABLE_REPORTS_DIR:-tmp/wave_c_ci_reenable_reports}"

usage() {
  cat <<'USAGE'
Wave C B148 CI Re-enable Approval Brief

用途：
  基于 B146 提交包生成审批简报（单页）。

用法：
  scripts/generate_wave_c_ci_reenable_approval_brief.sh [options]

选项：
  --run-id ID      指定 run_id
  --input FILE     指定 B146 提交包（默认最新）
  --check FILE     指定 B147 检查报告（默认按 run_id 推断）
  --output FILE    输出简报路径（默认 tmp/wave_c_ci_reenable_reports/wave_c_b148_ci_reenable_approval_brief_<run_id>.md）
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
    --check)
      CHECK_FILE="$2"
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
  INPUT_FILE="$(ls -1t "$REPORTS_DIR"/wave_c_b146_ci_reenable_submission_pack_*.md 2>/dev/null | head -1 || true)"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$REPORTS_DIR/wave_c_b148_ci_reenable_approval_brief_${RUN_ID}.md"
fi

if [[ -z "$CHECK_FILE" ]]; then
  CHECK_FILE="$REPORTS_DIR/wave_c_b147_submission_pack_check_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

if [[ -z "$INPUT_FILE" || ! -f "$INPUT_FILE" ]]; then
  echo "[ERROR] submission pack not found" >&2
  exit 1
fi

extract_marked_state() {
  local key="$1"
  rg -o "${key}:[[:space:]]*\*\*[A-Z0-9_]+\*\*" "$INPUT_FILE" | head -1 | sed -E 's/.*\*\*([A-Z0-9_]+)\*\*/\1/' || true
}

extract_marked_state_from_file() {
  local file="$1"
  local key="$2"
  rg -o "${key}:[[:space:]]*\*\*[A-Z0-9_]+\*\*" "$file" 2>/dev/null | head -1 | sed -E 's/.*\*\*([A-Z0-9_]+)\*\*/\1/' || true
}

extract_fail_tokens_from_check() {
  local file="$1"
  awk -F'|' '
    {
      token = ""
      result = ""
      col_count = 0
      for (i = 1; i <= NF; i++) {
        col = $i
        gsub(/^[[:space:]]+|[[:space:]]+$/, "", col)
        if (col == "") {
          continue
        }
        cols[++col_count] = col
      }

      if (col_count < 2) {
        next
      }

      token = cols[1]
      result = cols[2]
      if (tolower(token) == "token" || token ~ /^-+$/) {
        next
      }
      if (toupper(result) == "FAIL") {
        print token
      }
    }
  ' "$file" || true
}

submission_state="$(extract_marked_state submission_state)"
submission_state="${submission_state:-UNKNOWN}"

check_state="MISSING"
if [[ -f "$CHECK_FILE" ]]; then
  check_state="$(extract_marked_state_from_file "$CHECK_FILE" check_state)"
  check_state="${check_state:-UNKNOWN}"
fi

token_fail_count=0
fail_tokens=()
if [[ -f "$CHECK_FILE" ]]; then
  while IFS= read -r token; do
    if [[ -n "$token" ]]; then
      fail_tokens+=("$token")
      token_fail_count=$((token_fail_count + 1))
    fi
  done < <(extract_fail_tokens_from_check "$CHECK_FILE")
fi

alert_state="CLEAR"
if [[ "$submission_state" != "READY_TO_SUBMIT" || "$check_state" != "PASS" || "$token_fail_count" -gt 0 ]]; then
  alert_state="WARN"
fi

projected_b149_audit_alert_note="MISSING"
if [[ -f "$CHECK_FILE" ]]; then
  projected_b149_audit_alert_note="$(extract_marked_state_from_file "$CHECK_FILE" projected_b149_audit_alert_note)"
  projected_b149_audit_alert_note="${projected_b149_audit_alert_note:-UNKNOWN}"
fi

b149_audit_alert_note_preview="$(wave_c_map_alert_state_to_audit_note "$alert_state")"
b149_audit_alert_note_sync_state="$(wave_c_compute_audit_note_sync_state "$projected_b149_audit_alert_note" "$b149_audit_alert_note_preview")"

{
  echo "# Wave C B148 CI Re-enable Approval Brief"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- source_pack: $INPUT_FILE"
  echo "- source_check: $CHECK_FILE"
  echo "- submission_state: **$submission_state**"
  echo
  echo "## Consistency Alert Summary"
  echo
  echo "- source_check: $CHECK_FILE"
  echo "- check_state: $check_state"
  echo "- token_fail_count: $token_fail_count"
  echo "- alert_state: **$alert_state**"
  echo
  echo "## Audit Note Sync Preview"
  echo
  echo "- projected_b149_audit_alert_note: $projected_b149_audit_alert_note"
  echo "- b149_audit_alert_note_preview: **$b149_audit_alert_note_preview**"
  echo "- b149_audit_alert_note_sync_state: **$b149_audit_alert_note_sync_state**"
  echo
  echo "## Token Failures"
  echo
  echo "| token | result |"
  echo "|-------|--------|"
  if [[ "$token_fail_count" -eq 0 ]]; then
    echo "| <none> | PASS |"
  else
    for token in "${fail_tokens[@]}"; do
      echo "| $token | FAIL |"
    done
  fi
  echo
  echo "## Executive Summary"
  echo
  if [[ "$submission_state" == "READY_TO_SUBMIT" ]]; then
    echo "- 当前已满足提交审批条件，建议发起恢复 CI 审批。"
  else
    echo "- 当前尚不满足审批提交条件，建议先修复门禁项。"
  fi
  echo
  echo "## Recommended Next Step"
  echo
  echo "- 审批前：保持 workflow disabled。"
  echo "- 审批后：再执行 enable，并立即做 oncall strict 复核。"
} > "$OUTPUT_FILE"

echo "[PASS] approval brief generated: $OUTPUT_FILE"
exit 0
