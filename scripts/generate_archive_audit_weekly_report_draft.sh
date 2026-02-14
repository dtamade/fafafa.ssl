#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

WEEK_ID=""
HOLD_REPORT_GLOB="docs/test_reports/HOLD_EXPIRY_REVIEW_*.md"
LINKAGE_REPORT_GLOB="docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_*.md"
CHECKLIST_REPORT_GLOB="docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_*.md"
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计执行周报生成脚本（Draft）

用途：
  汇总 hold/linkage/checklist 报告，生成周级审计摘要。

用法：
  scripts/generate_archive_audit_weekly_report_draft.sh [options]

选项：
  --week-id ID               周报 ID（默认: YYYY_wWW）
  --hold-report-glob GLOB    hold 报告匹配（默认: docs/test_reports/HOLD_EXPIRY_REVIEW_*.md）
  --linkage-report-glob GLOB linkage 报告匹配（默认: docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_*.md）
  --checklist-report-glob GLOB checklist 报告匹配（默认: docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_*.md）
  --operator NAME            操作人/作业名（默认: codex）
  --output FILE              输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_<id>.md）
  --strict                   weekly_status 非 pass 时返回非 0
  --dry-run                  仅打印计划，不写文件
  --help                     显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --week-id)
      WEEK_ID="$2"
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

if [[ -z "$WEEK_ID" ]]; then
  WEEK_ID="$(date +"%Y_w%W")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_${WEEK_ID}.md"
elif [[ "$OUTPUT_FILE" != /* ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/$OUTPUT_FILE"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] week_id=$WEEK_ID"
  echo "[DRY-RUN] hold_glob=$HOLD_REPORT_GLOB"
  echo "[DRY-RUN] linkage_glob=$LINKAGE_REPORT_GLOB"
  echo "[DRY-RUN] checklist_glob=$CHECKLIST_REPORT_GLOB"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

mapfile -t HOLD_FILES < <(
  cd "$PROJECT_ROOT"
  shopt -s nullglob
  for file in $HOLD_REPORT_GLOB; do
    [[ -f "$file" ]] && printf '%s\n' "$file"
  done
)

mapfile -t LINKAGE_FILES < <(
  cd "$PROJECT_ROOT"
  shopt -s nullglob
  for file in $LINKAGE_REPORT_GLOB; do
    [[ -f "$file" ]] && printf '%s\n' "$file"
  done
)

mapfile -t CHECKLIST_FILES < <(
  cd "$PROJECT_ROOT"
  shopt -s nullglob
  for file in $CHECKLIST_REPORT_GLOB; do
    [[ -f "$file" ]] && printf '%s\n' "$file"
  done
)

resolve_report_abs_path() {
  local file="$1"
  if [[ "$file" == /* ]]; then
    echo "$file"
  else
    echo "$PROJECT_ROOT/$file"
  fi
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

hold_rows_file="$(mktemp)"
linkage_rows_file="$(mktemp)"
checklist_rows_file="$(mktemp)"
trap 'rm -f "$hold_rows_file" "$linkage_rows_file" "$checklist_rows_file"' EXIT

hold_overdue_total=0
hold_due_soon_total=0
hold_missing_total=0
hold_invalid_total=0
linkage_risk_total=0
checklist_pass=0
checklist_warn=0
checklist_fail=0
blocking_count=0

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

  echo "$file|$overdue|$due_soon|$missing|$invalid" >> "$hold_rows_file"
done

for file in "${LINKAGE_FILES[@]}"; do
  abs="$(resolve_report_abs_path "$file")"
  risk="$(to_int_or_zero "$(extract_metric "$abs" "sampled_runs_risk")")"
  status="$(extract_metric "$abs" "status")"
  [[ -z "$status" ]] && status="unknown"

  linkage_risk_total=$((linkage_risk_total + risk))
  echo "$file|$risk|$status" >> "$linkage_rows_file"
done

for file in "${CHECKLIST_FILES[@]}"; do
  abs="$(resolve_report_abs_path "$file")"
  readiness="$(extract_metric "$abs" "readiness")"
  blocking="$(extract_metric "$abs" "blocking_reasons")"
  [[ -z "$readiness" ]] && readiness="unknown"
  [[ -z "$blocking" ]] && blocking="none"

  case "$readiness" in
    pass) checklist_pass=$((checklist_pass + 1)) ;;
    warn) checklist_warn=$((checklist_warn + 1)) ;;
    fail) checklist_fail=$((checklist_fail + 1)) ;;
    *) ;;
  esac

  if [[ "$blocking" != "none" ]]; then
    # count comma-separated reasons
    reason_num="$(echo "$blocking" | awk -F',' '{print NF}')"
    reason_num="$(to_int_or_zero "$reason_num")"
    blocking_count=$((blocking_count + reason_num))
  fi

  echo "$file|$readiness|$blocking" >> "$checklist_rows_file"
done

weekly_status="pass"
if (( hold_overdue_total > 0 || linkage_risk_total > 0 || checklist_fail > 0 )); then
  weekly_status="fail"
elif (( hold_due_soon_total > 0 || hold_missing_total + hold_invalid_total > 0 || checklist_warn > 0 )); then
  weekly_status="warn"
fi

if (( ${#HOLD_FILES[@]} + ${#LINKAGE_FILES[@]} + ${#CHECKLIST_FILES[@]} == 0 )); then
  weekly_status="warn"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Weekly Report（Draft）

## 1) Metadata

| field | value |
|------|-------|
| week_id | $WEEK_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| hold_report_inputs | ${#HOLD_FILES[@]} |
| linkage_report_inputs | ${#LINKAGE_FILES[@]} |
| checklist_report_inputs | ${#CHECKLIST_FILES[@]} |
| operator | $OPERATOR |

## 2) Weekly Snapshot

| metric | value |
|--------|-------|
| hold_overdue_total | $hold_overdue_total |
| hold_due_soon_total | $hold_due_soon_total |
| hold_missing_or_invalid_expiry_total | $((hold_missing_total + hold_invalid_total)) |
| linkage_risk_total | $linkage_risk_total |
| checklist_readiness_fail | $checklist_fail |
| checklist_readiness_warn | $checklist_warn |
| weekly_status | $weekly_status |

## 3) Hold Aggregate

| source | overdue | due_soon | missing_expiry | invalid_expiry |
|--------|---------|----------|----------------|----------------|
EOF_REPORT

if [[ -s "$hold_rows_file" ]]; then
  while IFS='|' read -r source overdue due_soon missing invalid; do
    echo "| $source | $overdue | $due_soon | $missing | $invalid |" >> "$OUTPUT_FILE"
  done < "$hold_rows_file"
else
  echo "| n/a | 0 | 0 | 0 | 0 |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 4) Linkage Aggregate

| source | sampled_runs_risk | status |
|--------|-------------------|--------|
EOF_APPEND

if [[ -s "$linkage_rows_file" ]]; then
  while IFS='|' read -r source risk status; do
    echo "| $source | $risk | $status |" >> "$OUTPUT_FILE"
  done < "$linkage_rows_file"
else
  echo "| n/a | 0 | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Checklist Aggregate

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

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 6) Weekly Actions

- blocking:
  - <blocking_action_1>
- followup:
  - <followup_action_1>

> blocking_reason_count: $blocking_count
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$weekly_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass weekly status: $weekly_status" >&2
  exit 1
fi

echo "[PASS] archive audit weekly report generated"
