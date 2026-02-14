#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

DASHBOARD_ID=""
HOLD_REPORT_GLOB="docs/test_reports/HOLD_EXPIRY_REVIEW_*.md"
LINKAGE_REPORT_GLOB="docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_*.md"
CHECKLIST_REPORT_GLOB="docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_*.md"
WEEKLY_REPORT_GLOB="docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_*.md"
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计状态看板自动汇总脚本（Draft）

用途：
  汇总 hold/linkage/checklist/weekly 报告，输出状态看板与阻断原因聚合结果。

用法：
  scripts/generate_archive_audit_status_dashboard_draft.sh [options]

选项：
  --dashboard-id ID         看板 ID（默认: yyyyMMdd_HHmmss）
  --hold-report-glob GLOB   hold 报告匹配（默认: docs/test_reports/HOLD_EXPIRY_REVIEW_*.md）
  --linkage-report-glob GLOB
                            linkage 报告匹配（默认: docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_*.md）
  --checklist-report-glob GLOB
                            checklist 报告匹配（默认: docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_*.md）
  --weekly-report-glob GLOB weekly 报告匹配（默认: docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_*.md）
  --operator NAME           操作人/作业名（默认: codex）
  --output FILE             输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_<id>.md）
  --strict                  dashboard_status 非 pass 时返回非 0
  --dry-run                 仅打印计划，不写文件
  --help                    显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --dashboard-id)
      DASHBOARD_ID="$2"
      shift 2
      ;;
    --hold-report-glob)
      HOLD_REPORT_GLOB="$2"
      shift 2
      ;;
    --linkage-report-glob)
      LINKAGE_REPORT_GLOB="$2"
      shift 2
      ;;
    --checklist-report-glob)
      CHECKLIST_REPORT_GLOB="$2"
      shift 2
      ;;
    --weekly-report-glob)
      WEEKLY_REPORT_GLOB="$2"
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

if [[ -z "$DASHBOARD_ID" ]]; then
  DASHBOARD_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_${DASHBOARD_ID}.md"
elif [[ "$OUTPUT_FILE" != /* ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/$OUTPUT_FILE"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] dashboard_id=$DASHBOARD_ID"
  echo "[DRY-RUN] hold_glob=$HOLD_REPORT_GLOB"
  echo "[DRY-RUN] linkage_glob=$LINKAGE_REPORT_GLOB"
  echo "[DRY-RUN] checklist_glob=$CHECKLIST_REPORT_GLOB"
  echo "[DRY-RUN] weekly_glob=$WEEKLY_REPORT_GLOB"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

collect_files() {
  local glob="$1"

  (
    cd "$PROJECT_ROOT"
    shopt -s nullglob
    for file in $glob; do
      [[ -f "$file" ]] || continue
      [[ "$file" == *_TEMPLATE.md ]] && continue
      printf '%s\n' "$file"
    done | sort
  )
}

resolve_report_abs_path() {
  local file="$1"
  if [[ "$file" == /* ]]; then
    echo "$file"
  else
    echo "$PROJECT_ROOT/$file"
  fi
}

mapfile -t HOLD_FILES < <(collect_files "$HOLD_REPORT_GLOB")
mapfile -t LINKAGE_FILES < <(collect_files "$LINKAGE_REPORT_GLOB")
mapfile -t CHECKLIST_FILES < <(collect_files "$CHECKLIST_REPORT_GLOB")
mapfile -t WEEKLY_FILES < <(collect_files "$WEEKLY_REPORT_GLOB")

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

trim() {
  echo "$1" | sed -E 's/^[[:space:]]+//; s/[[:space:]]+$//'
}

status_from_counts() {
  local fail_count="$1"
  local warn_count="$2"
  local pass_count="$3"

  if (( fail_count > 0 )); then
    echo "fail"
  elif (( warn_count > 0 )); then
    echo "warn"
  elif (( pass_count > 0 )); then
    echo "pass"
  else
    echo "n/a"
  fi
}

hold_rows_file="$(mktemp)"
linkage_rows_file="$(mktemp)"
checklist_rows_file="$(mktemp)"
weekly_rows_file="$(mktemp)"
reason_rows_file="$(mktemp)"
trap 'rm -f "$hold_rows_file" "$linkage_rows_file" "$checklist_rows_file" "$weekly_rows_file" "$reason_rows_file"' EXIT

declare -A REASON_COUNT

hold_overdue_total=0
hold_due_soon_total=0
hold_missing_total=0
hold_invalid_total=0
hold_pass_count=0
hold_warn_count=0
hold_fail_count=0

for file in "${HOLD_FILES[@]}"; do
  abs="$(resolve_report_abs_path "$file")"
  overdue="$(to_int_or_zero "$(extract_metric "$abs" "overdue")")"
  due_soon="$(to_int_or_zero "$(extract_metric "$abs" "due_soon")")"
  missing="$(to_int_or_zero "$(extract_metric "$abs" "missing_expiry")")"
  invalid="$(to_int_or_zero "$(extract_metric "$abs" "invalid_expiry")")"

  hold_overdue_total=$((hold_overdue_total + overdue))
  hold_due_soon_total=$((hold_due_soon_total + due_soon))
  hold_missing_total=$((hold_missing_total + missing))
  hold_invalid_total=$((hold_invalid_total + invalid))

  row_status="pass"
  if (( overdue > 0 || missing + invalid > 0 )); then
    row_status="fail"
    hold_fail_count=$((hold_fail_count + 1))
  elif (( due_soon > 0 )); then
    row_status="warn"
    hold_warn_count=$((hold_warn_count + 1))
  else
    hold_pass_count=$((hold_pass_count + 1))
  fi

  echo "$file|$overdue|$due_soon|$missing|$invalid|$row_status" >> "$hold_rows_file"
done

linkage_risk_total=0
linkage_pass_count=0
linkage_warn_count=0
linkage_fail_count=0

for file in "${LINKAGE_FILES[@]}"; do
  abs="$(resolve_report_abs_path "$file")"
  risk="$(to_int_or_zero "$(extract_metric "$abs" "sampled_runs_risk")")"
  status_raw="$(extract_metric "$abs" "status")"
  status_raw="$(trim "$status_raw")"
  [[ -z "$status_raw" ]] && status_raw="unknown"

  row_status="pass"
  if (( risk > 0 )) || [[ "$status_raw" == "fail" ]]; then
    row_status="fail"
    linkage_fail_count=$((linkage_fail_count + 1))
  elif [[ "$status_raw" == "warn" || "$status_raw" == "unknown" ]]; then
    row_status="warn"
    linkage_warn_count=$((linkage_warn_count + 1))
  else
    linkage_pass_count=$((linkage_pass_count + 1))
  fi

  linkage_risk_total=$((linkage_risk_total + risk))
  echo "$file|$risk|$status_raw|$row_status" >> "$linkage_rows_file"
done

checklist_pass_count=0
checklist_warn_count=0
checklist_fail_count=0

for file in "${CHECKLIST_FILES[@]}"; do
  abs="$(resolve_report_abs_path "$file")"
  readiness="$(extract_metric "$abs" "readiness")"
  blocking="$(extract_metric "$abs" "blocking_reasons")"
  readiness="$(trim "$readiness")"
  blocking="$(trim "$blocking")"

  [[ -z "$readiness" ]] && readiness="unknown"
  [[ -z "$blocking" ]] && blocking="none"

  case "$readiness" in
    pass)
      checklist_pass_count=$((checklist_pass_count + 1))
      ;;
    fail)
      checklist_fail_count=$((checklist_fail_count + 1))
      ;;
    warn|unknown|*)
      checklist_warn_count=$((checklist_warn_count + 1))
      ;;
  esac

  if [[ "$blocking" != "none" ]]; then
    IFS=',' read -r -a reasons <<< "$blocking"
    for reason in "${reasons[@]}"; do
      reason="$(trim "$reason")"
      [[ -z "$reason" ]] && continue
      REASON_COUNT["$reason"]=$(( ${REASON_COUNT["$reason"]:-0} + 1 ))
    done
  fi

  echo "$file|$readiness|$blocking" >> "$checklist_rows_file"
done

weekly_pass_count=0
weekly_warn_count=0
weekly_fail_count=0

for file in "${WEEKLY_FILES[@]}"; do
  abs="$(resolve_report_abs_path "$file")"
  weekly_status="$(extract_metric "$abs" "weekly_status")"
  overdue_total="$(to_int_or_zero "$(extract_metric "$abs" "hold_overdue_total")")"
  checklist_fail_total="$(to_int_or_zero "$(extract_metric "$abs" "checklist_readiness_fail")")"
  weekly_status="$(trim "$weekly_status")"
  [[ -z "$weekly_status" ]] && weekly_status="unknown"

  case "$weekly_status" in
    pass)
      weekly_pass_count=$((weekly_pass_count + 1))
      ;;
    fail)
      weekly_fail_count=$((weekly_fail_count + 1))
      ;;
    warn|unknown|*)
      weekly_warn_count=$((weekly_warn_count + 1))
      ;;
  esac

  echo "$file|$weekly_status|$overdue_total|$checklist_fail_total" >> "$weekly_rows_file"
done

for reason in "${!REASON_COUNT[@]}"; do
  echo "$reason|${REASON_COUNT[$reason]}" >> "$reason_rows_file"
done

total_input_count=$(( ${#HOLD_FILES[@]} + ${#LINKAGE_FILES[@]} + ${#CHECKLIST_FILES[@]} + ${#WEEKLY_FILES[@]} ))

hold_status="pass"
if (( hold_overdue_total > 0 || hold_missing_total + hold_invalid_total > 0 )); then
  hold_status="fail"
elif (( hold_due_soon_total > 0 )); then
  hold_status="warn"
elif (( ${#HOLD_FILES[@]} == 0 )); then
  hold_status="n/a"
fi

linkage_status="$(status_from_counts "$linkage_fail_count" "$linkage_warn_count" "$linkage_pass_count")"
checklist_status="$(status_from_counts "$checklist_fail_count" "$checklist_warn_count" "$checklist_pass_count")"
weekly_status="$(status_from_counts "$weekly_fail_count" "$weekly_warn_count" "$weekly_pass_count")"

dashboard_status="pass"
if (( total_input_count == 0 )); then
  dashboard_status="warn"
elif (( weekly_fail_count > 0 || checklist_fail_count > 0 || hold_overdue_total > 0 || hold_missing_total + hold_invalid_total > 0 || linkage_risk_total > 0 )); then
  dashboard_status="fail"
elif (( weekly_warn_count > 0 || checklist_warn_count > 0 || hold_due_soon_total > 0 || linkage_warn_count > 0 )); then
  dashboard_status="warn"
fi

blocking_reason_total=0
for count in "${REASON_COUNT[@]}"; do
  blocking_reason_total=$((blocking_reason_total + count))
done

recommend_blocking_action="none"
recommend_followup_action="maintain-weekly-review"
if [[ "$dashboard_status" == "fail" ]]; then
  recommend_blocking_action="pause-release-and-clear-blockers"
  recommend_followup_action="review-hold-and-checklist-within-24h"
elif [[ "$dashboard_status" == "warn" ]]; then
  recommend_blocking_action="no-hard-block-but-track-risk"
  recommend_followup_action="resolve-due-soon-or-unknown-status-before-next-cut"
fi

latest_hold="n/a"
latest_linkage="n/a"
latest_checklist="n/a"
latest_weekly="n/a"

if (( ${#HOLD_FILES[@]} > 0 )); then
  latest_hold="${HOLD_FILES[$(( ${#HOLD_FILES[@]} - 1 ))]}"
fi

if (( ${#LINKAGE_FILES[@]} > 0 )); then
  latest_linkage="${LINKAGE_FILES[$(( ${#LINKAGE_FILES[@]} - 1 ))]}"
fi

if (( ${#CHECKLIST_FILES[@]} > 0 )); then
  latest_checklist="${CHECKLIST_FILES[$(( ${#CHECKLIST_FILES[@]} - 1 ))]}"
fi

if (( ${#WEEKLY_FILES[@]} > 0 )); then
  latest_weekly="${WEEKLY_FILES[$(( ${#WEEKLY_FILES[@]} - 1 ))]}"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Status Dashboard（Draft）

## 1) Metadata

| field | value |
|------|-------|
| dashboard_id | $DASHBOARD_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| hold_report_inputs | ${#HOLD_FILES[@]} |
| linkage_report_inputs | ${#LINKAGE_FILES[@]} |
| checklist_report_inputs | ${#CHECKLIST_FILES[@]} |
| weekly_report_inputs | ${#WEEKLY_FILES[@]} |
| operator | $OPERATOR |

## 2) Dashboard Snapshot

| metric | value |
|--------|-------|
| dashboard_status | $dashboard_status |
| hold_status | $hold_status |
| linkage_status | $linkage_status |
| checklist_status | $checklist_status |
| weekly_status | $weekly_status |
| hold_overdue_total | $hold_overdue_total |
| hold_due_soon_total | $hold_due_soon_total |
| hold_missing_or_invalid_expiry_total | $((hold_missing_total + hold_invalid_total)) |
| linkage_risk_total | $linkage_risk_total |
| checklist_readiness_fail | $checklist_fail_count |
| checklist_readiness_warn_or_unknown | $checklist_warn_count |
| weekly_fail_count | $weekly_fail_count |
| weekly_warn_or_unknown_count | $weekly_warn_count |
| blocking_reason_total | $blocking_reason_total |

## 3) Signal Board

| dimension | status | key_metrics | evidence |
|-----------|--------|-------------|----------|
| hold_expiry | $hold_status | overdue=$hold_overdue_total; due_soon=$hold_due_soon_total; missing_or_invalid=$((hold_missing_total + hold_invalid_total)) | $latest_hold |
| audit_linkage | $linkage_status | sampled_runs_risk_total=$linkage_risk_total | $latest_linkage |
| release_checklist | $checklist_status | fail=$checklist_fail_count; warn_or_unknown=$checklist_warn_count | $latest_checklist |
| weekly_execution | $weekly_status | fail=$weekly_fail_count; warn_or_unknown=$weekly_warn_count | $latest_weekly |
| overall_dashboard | $dashboard_status | inputs=$total_input_count; blocking_reasons=$blocking_reason_total | $OUTPUT_FILE |

## 4) Hold Aggregate Detail

| source | overdue | due_soon | missing_expiry | invalid_expiry | row_status |
|--------|---------|----------|----------------|----------------|------------|
EOF_REPORT

if [[ -s "$hold_rows_file" ]]; then
  while IFS='|' read -r source overdue due_soon missing invalid row_status; do
    echo "| $source | $overdue | $due_soon | $missing | $invalid | $row_status |" >> "$OUTPUT_FILE"
  done < "$hold_rows_file"
else
  echo "| n/a | 0 | 0 | 0 | 0 | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Linkage Aggregate Detail

| source | sampled_runs_risk | source_status | row_status |
|--------|-------------------|---------------|------------|
EOF_APPEND

if [[ -s "$linkage_rows_file" ]]; then
  while IFS='|' read -r source risk source_status row_status; do
    echo "| $source | $risk | $source_status | $row_status |" >> "$OUTPUT_FILE"
  done < "$linkage_rows_file"
else
  echo "| n/a | 0 | n/a | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 6) Checklist Aggregate Detail

| source | readiness | blocking_reasons |
|--------|-----------|------------------|
EOF_APPEND

if [[ -s "$checklist_rows_file" ]]; then
  while IFS='|' read -r source readiness blocking; do
    echo "| $source | $readiness | $blocking |" >> "$OUTPUT_FILE"
  done < "$checklist_rows_file"
else
  echo "| n/a | n/a | none |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 7) Weekly Trend

| source | weekly_status | hold_overdue_total | checklist_readiness_fail |
|--------|---------------|--------------------|--------------------------|
EOF_APPEND

if [[ -s "$weekly_rows_file" ]]; then
  while IFS='|' read -r source row_weekly_status overdue_total checklist_fail_total; do
    echo "| $source | $row_weekly_status | $overdue_total | $checklist_fail_total |" >> "$OUTPUT_FILE"
  done < "$weekly_rows_file"
else
  echo "| n/a | n/a | 0 | 0 |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 8) Blocking Reason Aggregate

| reason | count |
|--------|-------|
EOF_APPEND

if [[ -s "$reason_rows_file" ]]; then
  sort -t'|' -k2,2nr -k1,1 "$reason_rows_file" | while IFS='|' read -r reason count; do
    echo "| $reason | $count |" >> "$OUTPUT_FILE"
  done
else
  echo "| none | 0 |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 9) Suggested Actions

- blocking:
  - $recommend_blocking_action
- followup:
  - $recommend_followup_action
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$dashboard_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass dashboard status: $dashboard_status" >&2
  exit 1
fi

echo "[PASS] archive audit status dashboard generated"
