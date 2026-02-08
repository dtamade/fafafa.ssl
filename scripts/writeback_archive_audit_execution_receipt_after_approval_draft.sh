#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

WRITEBACK_ID=""
EXECUTION_RECEIPT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_SAMPLE_B35.md"
APPROVAL_CHAIN_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md"
RETEST_GATE_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md"
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计签批后执行回执自动回写脚本（Draft）

用途：
  根据签批链路与重测门禁结果，自动生成执行回执回写载荷与状态摘要。

用法：
  scripts/writeback_archive_audit_execution_receipt_after_approval_draft.sh [options]

选项：
  --writeback-id ID          回写 ID（默认: yyyyMMdd_HHmmss）
  --execution-receipt FILE   执行回执文件（默认: docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_SAMPLE_B35.md）
  --approval-chain FILE      签批链路报告（默认: docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md）
  --retest-gate FILE         重测门禁报告（默认: docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md）
  --operator NAME            操作人/作业名（默认: codex）
  --output FILE              输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_<id>.md）
  --strict                   writeback_status 非 pass 时返回非 0
  --dry-run                  仅打印计划，不写文件
  --help                     显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --writeback-id)
      WRITEBACK_ID="$2"
      shift 2
      ;;
    --execution-receipt)
      EXECUTION_RECEIPT_FILE="$2"
      shift 2
      ;;
    --approval-chain)
      APPROVAL_CHAIN_FILE="$2"
      shift 2
      ;;
    --retest-gate)
      RETEST_GATE_FILE="$2"
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

if [[ -z "$WRITEBACK_ID" ]]; then
  WRITEBACK_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_${WRITEBACK_ID}.md"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] writeback_id=$WRITEBACK_ID"
  echo "[DRY-RUN] execution_receipt=$EXECUTION_RECEIPT_FILE"
  echo "[DRY-RUN] approval_chain=$APPROVAL_CHAIN_FILE"
  echo "[DRY-RUN] retest_gate=$RETEST_GATE_FILE"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$EXECUTION_RECEIPT_FILE" "$APPROVAL_CHAIN_FILE" "$RETEST_GATE_FILE"; do
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

normalize_receipt_status() {
  case "$1" in
    done|waived|pending|in-progress) echo "$1" ;;
    inprogress) echo "in-progress" ;;
    *) echo "pending" ;;
  esac
}

receipt_execution_readiness="$(trim "$(extract_metric "$EXECUTION_RECEIPT_FILE" "execution_readiness")")"
receipt_release_decision="$(trim "$(extract_metric "$EXECUTION_RECEIPT_FILE" "release_decision")")"
approval_status="$(trim "$(extract_metric "$APPROVAL_CHAIN_FILE" "approval_status")")"
approval_release_decision="$(trim "$(extract_metric "$APPROVAL_CHAIN_FILE" "release_decision")")"
retest_gate_status="$(trim "$(extract_metric "$RETEST_GATE_FILE" "regression_gate_status")")"
retest_release_advice="$(trim "$(extract_metric "$RETEST_GATE_FILE" "release_advice")")"

[[ -z "$receipt_execution_readiness" ]] && receipt_execution_readiness="unknown"
[[ -z "$receipt_release_decision" ]] && receipt_release_decision="unknown"
[[ -z "$approval_status" ]] && approval_status="unknown"
[[ -z "$approval_release_decision" ]] && approval_release_decision="unknown"
[[ -z "$retest_gate_status" ]] && retest_gate_status="unknown"
[[ -z "$retest_release_advice" ]] && retest_release_advice="unknown"

declare -A RETEST_STATUS_BY_BLOCKER
declare -A RETEST_SIGNAL_BY_BLOCKER
declare -A RETEST_EVIDENCE_BY_BLOCKER

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 c7 c8 _ <<< "$row"

  blocker_code="$(trim "$c1")"
  retest_status="$(trim "$c6")"
  gate_signal="$(trim "$c7")"
  evidence="$(trim "$c8")"

  if [[ -z "$blocker_code" || "$blocker_code" == "none" || "$blocker_code" == "<BLK-001>" ]]; then
    continue
  fi

  [[ -z "$retest_status" ]] && retest_status="unknown"
  [[ -z "$gate_signal" ]] && gate_signal="unknown"

  RETEST_STATUS_BY_BLOCKER["$blocker_code"]="$retest_status"
  RETEST_SIGNAL_BY_BLOCKER["$blocker_code"]="$gate_signal"
  RETEST_EVIDENCE_BY_BLOCKER["$blocker_code"]="$evidence"
done < <(extract_section_rows "$RETEST_GATE_FILE" "4) Retest Rows")

rows_file="$(mktemp)"
payload_rows_file="$(mktemp)"
unresolved_rows_file="$(mktemp)"
trap 'rm -f "$rows_file" "$payload_rows_file" "$unresolved_rows_file"' EXIT

total_items=0
retest_signaled_items=0
writeback_changed_items=0
writeback_done_items=0
writeback_waived_items=0
writeback_pending_items=0
writeback_inprogress_items=0
writeback_unknown_items=0
open_critical_items=0
open_high_items=0

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 _ <<< "$row"

  blocker_code="$(trim "$c1")"
  source="$(trim "$c2")"
  blocker_key="$(trim "$c3")"
  severity="$(trim "$c4")"
  owner="$(trim "$c5")"
  action="$(trim "$c7")"
  previous_status="$(normalize_receipt_status "$(trim "$c8")")"
  previous_note="$(trim "$c9")"
  evidence="$(trim "$c10")"

  if [[ -z "$blocker_code" || "$blocker_code" == "none" || "$blocker_code" == "<BLK-001>" ]]; then
    continue
  fi

  total_items=$((total_items + 1))

  retest_status="${RETEST_STATUS_BY_BLOCKER[$blocker_code]-}"
  gate_signal="${RETEST_SIGNAL_BY_BLOCKER[$blocker_code]-}"
  retest_evidence="${RETEST_EVIDENCE_BY_BLOCKER[$blocker_code]-}"

  writeback_status="$previous_status"
  writeback_note="keep-original-status"

  if [[ -n "$retest_status" ]]; then
    retest_signaled_items=$((retest_signaled_items + 1))

    case "$retest_status" in
      pass)
        writeback_status="done"
        writeback_note="retest-pass-writeback"
        ;;
      waived)
        writeback_status="waived"
        writeback_note="retest-waiver-writeback"
        ;;
      warn)
        writeback_status="in-progress"
        writeback_note="retest-warn-manual-review"
        ;;
      fail)
        writeback_status="pending"
        writeback_note="retest-fail-keep-open"
        ;;
      *)
        writeback_status="pending"
        writeback_note="unrecognized-retest-status-manual-check"
        ;;
    esac
  else
    gate_signal="no-retest-signal"
    retest_status="unknown"
    retest_evidence="$evidence"
    writeback_note="retain-without-retest-signal"
  fi

  if [[ "$writeback_status" != "$previous_status" ]]; then
    writeback_changed_items=$((writeback_changed_items + 1))
  fi

  case "$writeback_status" in
    done)
      writeback_done_items=$((writeback_done_items + 1))
      ;;
    waived)
      writeback_waived_items=$((writeback_waived_items + 1))
      ;;
    in-progress)
      writeback_inprogress_items=$((writeback_inprogress_items + 1))
      ;;
    pending)
      writeback_pending_items=$((writeback_pending_items + 1))
      ;;
    *)
      writeback_unknown_items=$((writeback_unknown_items + 1))
      ;;
  esac

  if [[ "$writeback_status" == "pending" || "$writeback_status" == "in-progress" ]]; then
    if [[ "$severity" == "critical" ]]; then
      open_critical_items=$((open_critical_items + 1))
    elif [[ "$severity" == "high" ]]; then
      open_high_items=$((open_high_items + 1))
    fi

    echo "$blocker_code|$severity|$owner|$action|$writeback_status|$gate_signal|$retest_evidence" >> "$unresolved_rows_file"
  fi

  echo "$blocker_code|$source|$blocker_key|$severity|$owner|$previous_status|$retest_status|$writeback_status|$writeback_note|$gate_signal|$retest_evidence" >> "$rows_file"
  echo "$blocker_code|$writeback_status|$writeback_note|$retest_evidence" >> "$payload_rows_file"
done < <(extract_section_rows "$EXECUTION_RECEIPT_FILE" "4) Execution Receipt Rows")

writeback_close_percent=0
if (( total_items > 0 )); then
  writeback_close_percent=$(( (writeback_done_items + writeback_waived_items) * 100 / total_items ))
fi

writeback_status="pass"
release_advice="proceed-and-sync-writeback"

if (( total_items == 0 )); then
  writeback_status="warn"
  release_advice="insufficient-writeback-items"
elif (( open_critical_items > 0 || open_high_items > 0 )) || [[ "$approval_status" == "fail" || "$retest_gate_status" == "fail" ]]; then
  writeback_status="fail"
  release_advice="block-release-and-keep-writeback-open"
elif (( writeback_pending_items > 0 || writeback_inprogress_items > 0 || writeback_unknown_items > 0 )) || [[ "$approval_status" == "warn" || "$retest_gate_status" == "warn" || "$approval_status" == "unknown" || "$retest_gate_status" == "unknown" ]]; then
  writeback_status="warn"
  release_advice="proceed-with-tracked-writeback"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Execution Receipt Writeback（Draft）

## 1) Metadata

| field | value |
|------|-------|
| writeback_id | $WRITEBACK_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| execution_receipt_report | $EXECUTION_RECEIPT_FILE |
| approval_chain_report | $APPROVAL_CHAIN_FILE |
| retest_gate_report | $RETEST_GATE_FILE |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| receipt_execution_readiness | $receipt_execution_readiness |
| receipt_release_decision | $receipt_release_decision |
| approval_status | $approval_status |
| approval_release_decision | $approval_release_decision |
| retest_gate_status | $retest_gate_status |
| retest_release_advice | $retest_release_advice |

## 3) Writeback Summary

| metric | value |
|--------|-------|
| total_items | $total_items |
| retest_signaled_items | $retest_signaled_items |
| writeback_changed_items | $writeback_changed_items |
| writeback_done_items | $writeback_done_items |
| writeback_waived_items | $writeback_waived_items |
| writeback_pending_items | $writeback_pending_items |
| writeback_inprogress_items | $writeback_inprogress_items |
| writeback_unknown_items | $writeback_unknown_items |
| writeback_close_percent | ${writeback_close_percent}% |
| open_critical_items | $open_critical_items |
| open_high_items | $open_high_items |
| writeback_status | $writeback_status |
| release_advice | $release_advice |

## 4) Writeback Rows

| blocker_code | source | blocker_key | severity | owner | previous_status | retest_status | writeback_status | writeback_note | gate_signal | writeback_evidence |
|--------------|--------|-------------|----------|-------|-----------------|---------------|------------------|----------------|-------------|--------------------|
EOF_REPORT

if [[ -s "$rows_file" ]]; then
  while IFS='|' read -r blocker_code source blocker_key severity owner previous_status retest_status writeback_status_row writeback_note gate_signal writeback_evidence; do
    echo "| $blocker_code | $source | $blocker_key | $severity | $owner | $previous_status | $retest_status | $writeback_status_row | $writeback_note | $gate_signal | $writeback_evidence |" >> "$OUTPUT_FILE"
  done < "$rows_file"
else
  echo "| none | n/a | none | n/a | n/a | n/a | unknown | pending | no-receipt-rows | no-signal | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Receipt Writeback Payload

| blocker_code | writeback_status | writeback_note | writeback_evidence |
|--------------|------------------|----------------|--------------------|
EOF_APPEND

if [[ -s "$payload_rows_file" ]]; then
  while IFS='|' read -r blocker_code writeback_status_row writeback_note writeback_evidence; do
    echo "| $blocker_code | $writeback_status_row | $writeback_note | $writeback_evidence |" >> "$OUTPUT_FILE"
  done < "$payload_rows_file"
else
  echo "| none | pending | no-payload | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 6) Unresolved After Writeback

| blocker_code | severity | owner | action | writeback_status | gate_signal | evidence |
|--------------|----------|-------|--------|------------------|-------------|----------|
EOF_APPEND

if [[ -s "$unresolved_rows_file" ]]; then
  while IFS='|' read -r blocker_code severity owner action writeback_status_row gate_signal writeback_evidence; do
    echo "| $blocker_code | $severity | $owner | $action | $writeback_status_row | $gate_signal | $writeback_evidence |" >> "$OUTPUT_FILE"
  done < "$unresolved_rows_file"
else
  echo "| none | n/a | n/a | none | done | none | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 7) Suggested Actions

- immediate:
  - $release_advice
- followup:
  - apply-writeback-payload-to-next-execution-receipt
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$writeback_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass writeback status: $writeback_status" >&2
  exit 1
fi

echo "[PASS] execution receipt writeback generated"
