#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RECORD_ID=""
EXECUTION_RECEIPT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_SAMPLE_B35.md"
REQUIRED_CLOSE_PERCENT=100
WAIVER_REASON=""
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
阻断项关闭校验与豁免记录脚本（Draft）

用途：
  校验执行回执中的 blocker 关闭状态，并输出豁免记录与放行建议。

用法：
  scripts/validate_archive_audit_blocker_closure_waiver_draft.sh [options]

选项：
  --record-id ID             记录 ID（默认: yyyyMMdd_HHmmss）
  --execution-receipt FILE   执行回执文件（默认: docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_SAMPLE_B35.md）
  --required-close-percent N 关闭率阈值（默认: 100）
  --waiver-reason TEXT       豁免原因（默认: pending-waiver-review）
  --operator NAME            操作人/作业名（默认: codex）
  --output FILE              输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_<id>.md）
  --strict                   closure_status 非 pass 时返回非 0
  --dry-run                  仅打印计划，不写文件
  --help                     显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --record-id)
      RECORD_ID="$2"
      shift 2
      ;;
    --execution-receipt)
      EXECUTION_RECEIPT_FILE="$2"
      shift 2
      ;;
    --required-close-percent)
      REQUIRED_CLOSE_PERCENT="$2"
      shift 2
      ;;
    --waiver-reason)
      WAIVER_REASON="$2"
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

if [[ -z "$RECORD_ID" ]]; then
  RECORD_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_${RECORD_ID}.md"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] record_id=$RECORD_ID"
  echo "[DRY-RUN] execution_receipt=$EXECUTION_RECEIPT_FILE"
  echo "[DRY-RUN] required_close_percent=$REQUIRED_CLOSE_PERCENT"
  echo "[DRY-RUN] waiver_reason=${WAIVER_REASON:-pending-waiver-review}"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

if [[ ! -f "$EXECUTION_RECEIPT_FILE" ]]; then
  echo "[FAIL] execution receipt not found: $EXECUTION_RECEIPT_FILE" >&2
  exit 1
fi

if ! [[ "$REQUIRED_CLOSE_PERCENT" =~ ^[0-9]+$ ]]; then
  echo "[FAIL] required-close-percent should be non-negative integer" >&2
  exit 1
fi

if [[ -z "$WAIVER_REASON" ]]; then
  WAIVER_REASON="pending-waiver-review"
fi

trim() {
  echo "$1" | sed -E 's/^[[:space:]]+//; s/[[:space:]]+$//'
}

extract_metric() {
  local file="$1"
  local key="$2"

  grep -E "^\| ${key} \|" "$file" | head -1 | sed -E 's/^\|[^|]*\|[[:space:]]*//; s/[[:space:]]*\|[[:space:]]*$//' || true
}

to_int_or_zero() {
  local value="$1"
  if [[ "$value" =~ ^[0-9]+$ ]]; then
    echo "$value"
  else
    echo 0
  fi
}

extract_section_rows() {
  local file="$1"
  local section_title="$2"

  awk -v section="$section_title" '
    index($0, "## " section) == 1 { in_section=1; header_skipped=0; next }
    in_section && /^## / { exit }
    in_section && /^\|/ {
      if ($0 ~ /^\|[- ]+\|/) next
      if (header_skipped == 0) {
        header_skipped=1
        next
      }
      print
    }
  ' "$file"
}

input_execution_readiness="$(trim "$(extract_metric "$EXECUTION_RECEIPT_FILE" "execution_readiness")")"
input_release_decision="$(trim "$(extract_metric "$EXECUTION_RECEIPT_FILE" "release_decision")")"

[[ -z "$input_execution_readiness" ]] && input_execution_readiness="unknown"
[[ -z "$input_release_decision" ]] && input_release_decision="unknown"

rows_file="$(mktemp)"
waiver_rows_file="$(mktemp)"
unclosed_rows_file="$(mktemp)"
trap 'rm -f "$rows_file" "$waiver_rows_file" "$unclosed_rows_file"' EXIT

total_items=0
done_items=0
waived_items=0
pending_items=0
inprogress_items=0
unknown_items=0
critical_unclosed=0
high_unclosed=0

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 _ <<< "$row"

  blocker_code="$(trim "$c1")"
  source="$(trim "$c2")"
  blocker_key="$(trim "$c3")"
  severity="$(trim "$c4")"
  owner="$(trim "$c5")"
  target_sla="$(trim "$c6")"
  action="$(trim "$c7")"
  execution_status="$(trim "$c8")"
  receipt_note="$(trim "$c9")"
  evidence="$(trim "$c10")"

  [[ -z "$blocker_code" || "$blocker_code" == "<BLK-001>" ]] && continue

  total_items=$((total_items + 1))

  closure_check="pass"
  closure_note="closed-or-accepted"

  case "$execution_status" in
    done)
      done_items=$((done_items + 1))
      closure_check="pass"
      closure_note="closed"
      ;;
    waived)
      waived_items=$((waived_items + 1))
      closure_check="warn"
      closure_note="waiver-recorded"
      echo "$blocker_code|$severity|$owner|$action|$WAIVER_REASON|$evidence" >> "$waiver_rows_file"
      ;;
    in-progress)
      inprogress_items=$((inprogress_items + 1))
      closure_check="fail"
      closure_note="still-in-progress"
      echo "$blocker_code|$severity|$owner|$action|$execution_status|$evidence" >> "$unclosed_rows_file"
      ;;
    pending)
      pending_items=$((pending_items + 1))
      closure_check="fail"
      closure_note="not-closed"
      echo "$blocker_code|$severity|$owner|$action|$execution_status|$evidence" >> "$unclosed_rows_file"
      ;;
    *)
      unknown_items=$((unknown_items + 1))
      closure_check="fail"
      closure_note="unknown-status"
      echo "$blocker_code|$severity|$owner|$action|$execution_status|$evidence" >> "$unclosed_rows_file"
      ;;
  esac

  if [[ "$closure_check" == "fail" ]]; then
    case "$severity" in
      critical) critical_unclosed=$((critical_unclosed + 1)) ;;
      high) high_unclosed=$((high_unclosed + 1)) ;;
      *) ;;
    esac
  fi

  echo "$blocker_code|$source|$blocker_key|$severity|$owner|$target_sla|$action|$execution_status|$closure_check|$closure_note|$receipt_note|$evidence" >> "$rows_file"
done < <(extract_section_rows "$EXECUTION_RECEIPT_FILE" "4) Execution Receipt Rows")

close_percent=0
if (( total_items > 0 )); then
  close_percent=$(( (done_items + waived_items) * 100 / total_items ))
fi

closure_status="pass"
release_advice="proceed"

if (( critical_unclosed > 0 || high_unclosed > 0 )); then
  closure_status="fail"
  release_advice="block-release-until-critical-high-closed"
elif (( close_percent < REQUIRED_CLOSE_PERCENT )); then
  closure_status="warn"
  release_advice="proceed-with-closure-plan"
fi

if (( pending_items > 0 || inprogress_items > 0 || unknown_items > 0 )); then
  if [[ "$closure_status" == "pass" ]]; then
    closure_status="warn"
    release_advice="proceed-with-closure-plan"
  fi
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Blocker Closure & Waiver Record（Draft）

## 1) Metadata

| field | value |
|------|-------|
| record_id | $RECORD_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| execution_receipt_report | $EXECUTION_RECEIPT_FILE |
| required_close_percent | $REQUIRED_CLOSE_PERCENT |
| waiver_reason_default | $WAIVER_REASON |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| input_execution_readiness | $input_execution_readiness |
| input_release_decision | $input_release_decision |
| total_items | $total_items |
| done_items | $done_items |
| waived_items | $waived_items |
| pending_items | $pending_items |
| inprogress_items | $inprogress_items |
| unknown_items | $unknown_items |

## 3) Closure Summary

| metric | value |
|--------|-------|
| close_percent | ${close_percent}% |
| closure_status | $closure_status |
| critical_unclosed | $critical_unclosed |
| high_unclosed | $high_unclosed |
| release_advice | $release_advice |

## 4) Closure Verification Rows

| blocker_code | source | blocker_key | severity | owner | target_sla | action | execution_status | closure_check | closure_note | receipt_note | evidence |
|--------------|--------|-------------|----------|-------|------------|--------|------------------|---------------|--------------|-------------|----------|
EOF_REPORT

if [[ -s "$rows_file" ]]; then
  while IFS='|' read -r blocker_code source blocker_key severity owner target_sla action execution_status closure_check closure_note receipt_note evidence; do
    echo "| $blocker_code | $source | $blocker_key | $severity | $owner | $target_sla | $action | $execution_status | $closure_check | $closure_note | $receipt_note | $evidence |" >> "$OUTPUT_FILE"
  done < "$rows_file"
else
  echo "| n/a | n/a | none | low | n/a | next-weekly | none | done | pass | no-items | n/a | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Waiver Records

| blocker_code | severity | owner | action | waiver_reason | evidence |
|--------------|----------|-------|--------|---------------|----------|
EOF_APPEND

if [[ -s "$waiver_rows_file" ]]; then
  while IFS='|' read -r blocker_code severity owner action waiver_reason evidence; do
    echo "| $blocker_code | $severity | $owner | $action | $waiver_reason | $evidence |" >> "$OUTPUT_FILE"
  done < "$waiver_rows_file"
else
  echo "| none | n/a | n/a | none | n/a | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 6) Unclosed Items

| blocker_code | severity | owner | action | execution_status | evidence |
|--------------|----------|-------|--------|------------------|----------|
EOF_APPEND

if [[ -s "$unclosed_rows_file" ]]; then
  while IFS='|' read -r blocker_code severity owner action execution_status evidence; do
    echo "| $blocker_code | $severity | $owner | $action | $execution_status | $evidence |" >> "$OUTPUT_FILE"
  done < "$unclosed_rows_file"
else
  echo "| none | n/a | n/a | none | done | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 7) Suggested Actions

- blocking:
  - $release_advice
- followup:
  - update-execution-receipt-and-recheck-closure
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$closure_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass closure status: $closure_status" >&2
  exit 1
fi

echo "[PASS] blocker closure and waiver record generated"
