#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

CHAIN_ID=""
EXECUTION_RECEIPT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_SAMPLE_B35.md"
CLOSURE_RECORD_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_SAMPLE_B36.md"
REMEDIATION_PLAN_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_CONSISTENCY_REMEDIATION_SAMPLE_B37.md"
BACKTEST_REPORT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST_SAMPLE_B38.md"
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计执行回执签批链路生成脚本（Draft）

用途：
  汇总执行回执、关闭校验、修复建议与阈值回测，输出可追踪的签批链路与放行建议。

用法：
  scripts/generate_archive_audit_execution_approval_chain_draft.sh [options]

选项：
  --chain-id ID              签批链路 ID（默认: yyyyMMdd_HHmmss）
  --execution-receipt FILE   执行回执文件（默认: docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_SAMPLE_B35.md）
  --closure-record FILE      关闭校验记录（默认: docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_SAMPLE_B36.md）
  --remediation-plan FILE    修复建议报告（默认: docs/test_reports/ARCHIVE_AUDIT_CONSISTENCY_REMEDIATION_SAMPLE_B37.md）
  --backtest-report FILE     阈值回测报告（默认: docs/test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST_SAMPLE_B38.md）
  --operator NAME            操作人/作业名（默认: codex）
  --output FILE              输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_<id>.md）
  --strict                   approval_status 非 pass 时返回非 0
  --dry-run                  仅打印计划，不写文件
  --help                     显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --chain-id)
      CHAIN_ID="$2"
      shift 2
      ;;
    --execution-receipt)
      EXECUTION_RECEIPT_FILE="$2"
      shift 2
      ;;
    --closure-record)
      CLOSURE_RECORD_FILE="$2"
      shift 2
      ;;
    --remediation-plan)
      REMEDIATION_PLAN_FILE="$2"
      shift 2
      ;;
    --backtest-report)
      BACKTEST_REPORT_FILE="$2"
      shift 2
      ;;
    --operator)
      OPERATOR="$2"
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
    --dry-run)
      DRY_RUN=true
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

if [[ -z "$CHAIN_ID" ]]; then
  CHAIN_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_${CHAIN_ID}.md"
fi

resolve_input_path() {
  local path="$1"

  if [[ "$path" == /* ]]; then
    echo "$path"
    return
  fi

  if [[ -f "$path" ]]; then
    echo "$path"
    return
  fi

  if [[ -f "$PROJECT_ROOT/$path" ]]; then
    echo "$PROJECT_ROOT/$path"
    return
  fi

  echo "$path"
}

resolve_output_path() {
  local path="$1"

  if [[ "$path" == /* ]]; then
    echo "$path"
  else
    echo "$PROJECT_ROOT/$path"
  fi
}

EXECUTION_RECEIPT_FILE="$(resolve_input_path "$EXECUTION_RECEIPT_FILE")"
CLOSURE_RECORD_FILE="$(resolve_input_path "$CLOSURE_RECORD_FILE")"
REMEDIATION_PLAN_FILE="$(resolve_input_path "$REMEDIATION_PLAN_FILE")"
BACKTEST_REPORT_FILE="$(resolve_input_path "$BACKTEST_REPORT_FILE")"
OUTPUT_FILE="$(resolve_output_path "$OUTPUT_FILE")"

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] chain_id=$CHAIN_ID"
  echo "[DRY-RUN] execution_receipt=$EXECUTION_RECEIPT_FILE"
  echo "[DRY-RUN] closure_record=$CLOSURE_RECORD_FILE"
  echo "[DRY-RUN] remediation_plan=$REMEDIATION_PLAN_FILE"
  echo "[DRY-RUN] backtest_report=$BACKTEST_REPORT_FILE"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$EXECUTION_RECEIPT_FILE" "$CLOSURE_RECORD_FILE" "$REMEDIATION_PLAN_FILE" "$BACKTEST_REPORT_FILE"; do
  if [[ ! -f "$file" ]]; then
    echo "[FAIL] input file not found: $file" >&2
    exit 1
  fi
done

trim() {
  echo "$1" | sed -E 's/^[[:space:]]+//; s/[[:space:]]+$//'
}

extract_metric() {
  local file="$1"
  local key="$2"

  grep -E "^\| ${key} \|" "$file" | head -1 | sed -E 's/^\|[^|]*\|[[:space:]]*//; s/[[:space:]]*\|[[:space:]]*$//' || true
}

normalize_stage_status() {
  local raw_status="$1"

  case "$raw_status" in
    pass|warn|fail|unknown)
      echo "$raw_status"
      ;;
    *)
      if [[ -z "$raw_status" ]]; then
        echo "unknown"
      else
        echo "unknown"
      fi
      ;;
  esac
}

status_to_approval_note() {
  case "$1" in
    pass) echo "approved" ;;
    warn) echo "conditional-approval" ;;
    fail) echo "rejected" ;;
    unknown) echo "manual-review-required" ;;
    *) echo "manual-review-required" ;;
  esac
}

status_to_sla() {
  case "$1" in
    pass) echo "0h" ;;
    warn) echo "4h" ;;
    fail) echo "<1h" ;;
    unknown) echo "manual-review" ;;
    *) echo "manual-review" ;;
  esac
}

execution_readiness="$(normalize_stage_status "$(trim "$(extract_metric "$EXECUTION_RECEIPT_FILE" "execution_readiness")")")"
execution_release_decision="$(trim "$(extract_metric "$EXECUTION_RECEIPT_FILE" "release_decision")")"

closure_status="$(normalize_stage_status "$(trim "$(extract_metric "$CLOSURE_RECORD_FILE" "closure_status")")")"
closure_release_advice="$(trim "$(extract_metric "$CLOSURE_RECORD_FILE" "release_advice")")"

remediation_status="$(normalize_stage_status "$(trim "$(extract_metric "$REMEDIATION_PLAN_FILE" "remediation_status")")")"
remediation_release_guidance="$(trim "$(extract_metric "$REMEDIATION_PLAN_FILE" "release_guidance")")"

backtest_status="$(normalize_stage_status "$(trim "$(extract_metric "$BACKTEST_REPORT_FILE" "backtest_status")")")"
backtest_release_guidance="$(trim "$(extract_metric "$BACKTEST_REPORT_FILE" "release_guidance")")"

[[ -z "$execution_release_decision" ]] && execution_release_decision="unknown"
[[ -z "$closure_release_advice" ]] && closure_release_advice="unknown"
[[ -z "$remediation_release_guidance" ]] && remediation_release_guidance="unknown"
[[ -z "$backtest_release_guidance" ]] && backtest_release_guidance="unknown"

rows_file="$(mktemp)"
escalation_rows_file="$(mktemp)"
trap 'rm -f "$rows_file" "$escalation_rows_file"' EXIT

approved_stages=0
conditional_stages=0
rejected_stages=0
pending_review_stages=0

add_stage() {
  local stage_id="$1"
  local stage_name="$2"
  local source_report="$3"
  local gate_metric="$4"
  local gate_value="$5"
  local stage_status="$6"
  local approver_role="$7"
  local followup_action="$8"

  local approval_note
  local target_sla

  approval_note="$(status_to_approval_note "$stage_status")"
  target_sla="$(status_to_sla "$stage_status")"

  case "$stage_status" in
    pass)
      approved_stages=$((approved_stages + 1))
      ;;
    warn)
      conditional_stages=$((conditional_stages + 1))
      ;;
    fail)
      rejected_stages=$((rejected_stages + 1))
      ;;
    unknown)
      pending_review_stages=$((pending_review_stages + 1))
      ;;
    *)
      pending_review_stages=$((pending_review_stages + 1))
      ;;
  esac

  echo "$stage_id|$stage_name|$source_report|$gate_metric|$gate_value|$stage_status|$approver_role|$target_sla|$approval_note|$followup_action" >> "$rows_file"

  if [[ "$stage_status" != "pass" ]]; then
    echo "$stage_id|$stage_name|$stage_status|$approver_role|$gate_value|$followup_action" >> "$escalation_rows_file"
  fi
}

add_stage \
  "S1" \
  "execution-readiness-gate" \
  "$EXECUTION_RECEIPT_FILE" \
  "execution_readiness" \
  "$execution_readiness" \
  "$execution_readiness" \
  "release-manager" \
  "$execution_release_decision"

add_stage \
  "S2" \
  "blocker-closure-gate" \
  "$CLOSURE_RECORD_FILE" \
  "closure_status" \
  "$closure_status" \
  "$closure_status" \
  "qa-secops" \
  "$closure_release_advice"

add_stage \
  "S3" \
  "consistency-remediation-gate" \
  "$REMEDIATION_PLAN_FILE" \
  "remediation_status" \
  "$remediation_status" \
  "$remediation_status" \
  "release-ops" \
  "$remediation_release_guidance"

add_stage \
  "S4" \
  "threshold-backtest-gate" \
  "$BACKTEST_REPORT_FILE" \
  "backtest_status" \
  "$backtest_status" \
  "$backtest_status" \
  "risk-owner+release-manager" \
  "$backtest_release_guidance"

total_stages=4

approval_status="pass"
release_decision="proceed-release"

if (( rejected_stages > 0 )); then
  approval_status="fail"
  release_decision="block-release-and-escalate"
elif (( conditional_stages > 0 || pending_review_stages > 0 )); then
  approval_status="warn"
  release_decision="hold-for-manual-approval"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Execution Approval Chain（Draft）

## 1) Metadata

| field | value |
|------|-------|
| chain_id | $CHAIN_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| execution_receipt_report | $EXECUTION_RECEIPT_FILE |
| closure_record_report | $CLOSURE_RECORD_FILE |
| remediation_plan_report | $REMEDIATION_PLAN_FILE |
| backtest_report | $BACKTEST_REPORT_FILE |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| execution_readiness | $execution_readiness |
| execution_release_decision | $execution_release_decision |
| closure_status | $closure_status |
| closure_release_advice | $closure_release_advice |
| remediation_status | $remediation_status |
| remediation_release_guidance | $remediation_release_guidance |
| backtest_status | $backtest_status |
| backtest_release_guidance | $backtest_release_guidance |

## 3) Approval Summary

| metric | value |
|--------|-------|
| total_stages | $total_stages |
| approved_stages | $approved_stages |
| conditional_stages | $conditional_stages |
| rejected_stages | $rejected_stages |
| pending_review_stages | $pending_review_stages |
| approval_status | $approval_status |
| release_decision | $release_decision |

## 4) Approval Chain Rows

| stage_id | stage_name | source_report | gate_metric | gate_value | stage_status | approver_role | target_sla | approval_note | followup_action |
|----------|------------|---------------|-------------|------------|--------------|---------------|------------|---------------|-----------------|
EOF_REPORT

while IFS='|' read -r stage_id stage_name source_report gate_metric gate_value stage_status approver_role target_sla approval_note followup_action; do
  echo "| $stage_id | $stage_name | $source_report | $gate_metric | $gate_value | $stage_status | $approver_role | $target_sla | $approval_note | $followup_action |" >> "$OUTPUT_FILE"
done < "$rows_file"

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Escalation Queue

| stage_id | stage_name | stage_status | owner | trigger | required_action |
|----------|------------|--------------|-------|---------|-----------------|
EOF_APPEND

if [[ -s "$escalation_rows_file" ]]; then
  while IFS='|' read -r stage_id stage_name stage_status owner trigger required_action; do
    echo "| $stage_id | $stage_name | $stage_status | $owner | $trigger | $required_action |" >> "$OUTPUT_FILE"
  done < "$escalation_rows_file"
else
  echo "| none | none | pass | none | none | none |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 6) Suggested Actions

- immediate:
  - $release_decision
- followup:
  - sync-approval-chain-to-receipt-and-gateboard
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$approval_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass approval status: $approval_status" >&2
  exit 1
fi

echo "[PASS] execution approval chain generated"
