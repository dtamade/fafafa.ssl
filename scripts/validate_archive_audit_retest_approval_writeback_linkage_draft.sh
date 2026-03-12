#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

LINKAGE_ID=""
RETEST_GATE_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md"
APPROVAL_CHAIN_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md"
WRITEBACK_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md"
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计重测-签批联动回写一致性校验脚本（Draft）

用途：
  校验 retest 与 writeback 的逐项映射，以及 approval/retest/writeback 门禁状态联动一致性。

用法：
  scripts/validate_archive_audit_retest_approval_writeback_linkage_draft.sh [options]

选项：
  --linkage-id ID            联动校验 ID（默认: yyyyMMdd_HHmmss）
  --retest-gate FILE         重测门禁报告（默认: docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md）
  --approval-chain FILE      签批链路报告（默认: docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md）
  --writeback FILE           回写报告（默认: docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md）
  --operator NAME            操作人/作业名（默认: codex）
  --output FILE              输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_RETEST_APPROVAL_WRITEBACK_LINKAGE_<id>.md）
  --strict                   linkage_status 非 pass 时返回非 0
  --dry-run                  仅打印计划，不写文件
  --help                     显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --linkage-id)
      LINKAGE_ID="$2"
      shift 2
      ;;
    --retest-gate)
      RETEST_GATE_FILE="$2"
      shift 2
      ;;
    --approval-chain)
      APPROVAL_CHAIN_FILE="$2"
      shift 2
      ;;
    --writeback)
      WRITEBACK_FILE="$2"
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

if [[ -z "$LINKAGE_ID" ]]; then
  LINKAGE_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_RETEST_APPROVAL_WRITEBACK_LINKAGE_${LINKAGE_ID}.md"
fi

resolve_input_path() {
  local path="$1"
  if [[ "$path" == /* ]]; then
    echo "$path"
  elif [[ -e "$path" ]]; then
    echo "$path"
  else
    echo "$PROJECT_ROOT/$path"
  fi
}

resolve_output_path() {
  local path="$1"
  if [[ "$path" == /* ]]; then
    echo "$path"
  else
    echo "$PROJECT_ROOT/$path"
  fi
}

RETEST_GATE_FILE="$(resolve_input_path "$RETEST_GATE_FILE")"
APPROVAL_CHAIN_FILE="$(resolve_input_path "$APPROVAL_CHAIN_FILE")"
WRITEBACK_FILE="$(resolve_input_path "$WRITEBACK_FILE")"
OUTPUT_FILE="$(resolve_output_path "$OUTPUT_FILE")"

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] linkage_id=$LINKAGE_ID"
  echo "[DRY-RUN] retest_gate=$RETEST_GATE_FILE"
  echo "[DRY-RUN] approval_chain=$APPROVAL_CHAIN_FILE"
  echo "[DRY-RUN] writeback=$WRITEBACK_FILE"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$RETEST_GATE_FILE" "$APPROVAL_CHAIN_FILE" "$WRITEBACK_FILE"; do
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

to_int_or_zero() {
  local value="$1"
  if [[ "$value" =~ ^[0-9]+$ ]]; then
    echo "$value"
  else
    echo 0
  fi
}

normalize_status() {
  case "$1" in
    pass|warn|fail|unknown|done|waived|pending|in-progress) echo "$1" ;;
    inprogress) echo "in-progress" ;;
    *) echo "unknown" ;;
  esac
}

expected_writeback_from_retest() {
  case "$1" in
    pass) echo "done" ;;
    waived) echo "waived" ;;
    warn) echo "in-progress" ;;
    fail) echo "pending" ;;
    *) echo "pending" ;;
  esac
}

approval_status="$(normalize_status "$(trim "$(extract_metric "$APPROVAL_CHAIN_FILE" "approval_status")")")"
retest_gate_status="$(normalize_status "$(trim "$(extract_metric "$RETEST_GATE_FILE" "regression_gate_status")")")"
writeback_status="$(normalize_status "$(trim "$(extract_metric "$WRITEBACK_FILE" "writeback_status")")")"

writeback_signaled_items="$(to_int_or_zero "$(extract_metric "$WRITEBACK_FILE" "retest_signaled_items")")"
writeback_changed_items="$(to_int_or_zero "$(extract_metric "$WRITEBACK_FILE" "writeback_changed_items")")"

[[ -z "$approval_status" ]] && approval_status="unknown"
[[ -z "$retest_gate_status" ]] && retest_gate_status="unknown"
[[ -z "$writeback_status" ]] && writeback_status="unknown"

declare -A PAYLOAD_STATUS_BY_BLOCKER
declare -A PAYLOAD_NOTE_BY_BLOCKER
declare -A PAYLOAD_EVIDENCE_BY_BLOCKER

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 _ <<< "$row"

  blocker_code="$(trim "$c1")"
  writeback_status_row="$(normalize_status "$(trim "$c2")")"
  writeback_note="$(trim "$c3")"
  writeback_evidence="$(trim "$c4")"

  if [[ -z "$blocker_code" || "$blocker_code" == "none" || "$blocker_code" == "<BLK-001>" ]]; then
    continue
  fi

  PAYLOAD_STATUS_BY_BLOCKER["$blocker_code"]="$writeback_status_row"
  PAYLOAD_NOTE_BY_BLOCKER["$blocker_code"]="$writeback_note"
  PAYLOAD_EVIDENCE_BY_BLOCKER["$blocker_code"]="$writeback_evidence"
done < <(extract_section_rows "$WRITEBACK_FILE" "5) Receipt Writeback Payload")

rows_file="$(mktemp)"
mismatch_file="$(mktemp)"
trap 'rm -f "$rows_file" "$mismatch_file"' EXIT

total_rows=0
matched_rows=0
mismatch_rows=0
missing_payload_rows=0

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 c7 c8 _ <<< "$row"

  blocker_code="$(trim "$c1")"
  retest_status_row="$(normalize_status "$(trim "$c6")")"
  gate_signal="$(trim "$c7")"
  retest_evidence="$(trim "$c8")"

  if [[ -z "$blocker_code" || "$blocker_code" == "none" || "$blocker_code" == "<BLK-001>" ]]; then
    continue
  fi

  total_rows=$((total_rows + 1))

  expected_writeback="$(expected_writeback_from_retest "$retest_status_row")"
  actual_writeback="${PAYLOAD_STATUS_BY_BLOCKER[$blocker_code]-missing}"

  row_status="pass"
  note="mapped-consistently"
  payload_evidence="${PAYLOAD_EVIDENCE_BY_BLOCKER[$blocker_code]-$retest_evidence}"

  if [[ "$actual_writeback" == "missing" ]]; then
    row_status="fail"
    note="missing-writeback-payload"
    missing_payload_rows=$((missing_payload_rows + 1))
    mismatch_rows=$((mismatch_rows + 1))
    echo "$blocker_code|$expected_writeback|missing|$note|$payload_evidence" >> "$mismatch_file"
  elif [[ "$actual_writeback" != "$expected_writeback" ]]; then
    row_status="fail"
    note="expected-${expected_writeback}-but-found-${actual_writeback}"
    mismatch_rows=$((mismatch_rows + 1))
    echo "$blocker_code|$expected_writeback|$actual_writeback|$note|$payload_evidence" >> "$mismatch_file"
  else
    matched_rows=$((matched_rows + 1))
  fi

  echo "$blocker_code|$retest_status_row|$expected_writeback|$actual_writeback|$row_status|$note|$gate_signal|$payload_evidence" >> "$rows_file"
done < <(extract_section_rows "$RETEST_GATE_FILE" "4) Retest Rows")

gate_alignment_status="pass"
gate_alignment_note="approval/retest/writeback status aligned"
if [[ "$approval_status" == "pass" && ( "$retest_gate_status" != "pass" || "$writeback_status" != "pass" ) ]]; then
  gate_alignment_status="fail"
  gate_alignment_note="approval pass conflicts with downstream non-pass"
elif [[ "$approval_status" == "fail" && ( "$retest_gate_status" == "pass" || "$writeback_status" == "pass" ) ]]; then
  gate_alignment_status="fail"
  gate_alignment_note="approval fail conflicts with downstream pass"
elif [[ "$approval_status" != "$retest_gate_status" || "$retest_gate_status" != "$writeback_status" ]]; then
  gate_alignment_status="warn"
  gate_alignment_note="gate statuses differ, manual alignment needed"
fi

linkage_status="pass"
release_advice="proceed-with-linkage-sync"

if (( writeback_signaled_items > 0 && writeback_changed_items == 0 )); then
  linkage_status="fail"
  release_advice="block-release-and-force-writeback-change"
fi

if (( mismatch_rows > 0 || missing_payload_rows > 0 )) || [[ "$gate_alignment_status" == "fail" ]]; then
  linkage_status="fail"
  release_advice="block-release-until-linkage-mismatch-cleared"
elif [[ "$gate_alignment_status" == "warn" ]]; then
  linkage_status="warn"
  release_advice="proceed-with-manual-linkage-review"
fi

if (( total_rows == 0 )); then
  linkage_status="warn"
  release_advice="insufficient-linkage-rows"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Retest-Approval Writeback Linkage Validation（Draft）

## 1) Metadata

| field | value |
|------|-------|
| linkage_id | $LINKAGE_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| retest_gate_report | $RETEST_GATE_FILE |
| approval_chain_report | $APPROVAL_CHAIN_FILE |
| writeback_report | $WRITEBACK_FILE |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| approval_status | $approval_status |
| retest_gate_status | $retest_gate_status |
| writeback_status | $writeback_status |
| writeback_signaled_items | $writeback_signaled_items |
| writeback_changed_items | $writeback_changed_items |

## 3) Linkage Summary

| metric | value |
|--------|-------|
| total_rows | $total_rows |
| matched_rows | $matched_rows |
| mismatch_rows | $mismatch_rows |
| missing_payload_rows | $missing_payload_rows |
| gate_alignment_status | $gate_alignment_status |
| gate_alignment_note | $gate_alignment_note |
| linkage_status | $linkage_status |
| release_advice | $release_advice |

## 4) Linkage Rows

| blocker_code | retest_status | expected_writeback_status | actual_writeback_status | row_status | note | gate_signal | evidence |
|--------------|---------------|---------------------------|-------------------------|------------|------|-------------|----------|
EOF_REPORT

if [[ -s "$rows_file" ]]; then
  while IFS='|' read -r blocker_code retest_status_row expected_writeback actual_writeback row_status note gate_signal evidence; do
    echo "| $blocker_code | $retest_status_row | $expected_writeback | $actual_writeback | $row_status | $note | $gate_signal | $evidence |" >> "$OUTPUT_FILE"
  done < "$rows_file"
else
  echo "| none | unknown | pending | missing | warn | no-retest-rows | n/a | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Mismatch Queue

| blocker_code | expected_writeback | actual_writeback | reason | evidence |
|--------------|--------------------|------------------|--------|----------|
EOF_APPEND

if [[ -s "$mismatch_file" ]]; then
  while IFS='|' read -r blocker_code expected_writeback actual_writeback reason evidence; do
    echo "| $blocker_code | $expected_writeback | $actual_writeback | $reason | $evidence |" >> "$OUTPUT_FILE"
  done < "$mismatch_file"
else
  echo "| none | n/a | n/a | no-mismatch | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 6) Suggested Actions

- immediate:
  - $release_advice
- followup:
  - regenerate-writeback-payload-after-retest-sync
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$linkage_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass linkage status: $linkage_status" >&2
  exit 1
fi

echo "[PASS] retest-approval-writeback linkage validation generated"
