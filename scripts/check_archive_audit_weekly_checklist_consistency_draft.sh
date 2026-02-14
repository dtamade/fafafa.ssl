#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

CONSISTENCY_ID=""
WEEKLY_REPORT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md"
CHECKLIST_REPORT_FILE="$PROJECT_ROOT/docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md"
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
周报与发布清单一致性核查脚本（Draft）

用途：
  校验 weekly 报告与 pre-release checklist 在 readiness、阻断原因与关键计数上的一致性。

用法：
  scripts/check_archive_audit_weekly_checklist_consistency_draft.sh [options]

选项：
  --consistency-id ID      一致性核查 ID（默认: yyyyMMdd_HHmmss）
  --weekly-report FILE     周报文件（默认: docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md）
  --checklist-report FILE  发布前清单文件（默认: docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md）
  --operator NAME          操作人/作业名（默认: codex）
  --output FILE            输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_WEEKLY_CHECKLIST_CONSISTENCY_<id>.md）
  --strict                 consistency_status 非 pass 时返回非 0
  --dry-run                仅打印计划，不写文件
  --help                   显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --consistency-id)
      CONSISTENCY_ID="$2"
      shift 2
      ;;
    --weekly-report)
      WEEKLY_REPORT_FILE="$2"
      shift 2
      ;;
    --checklist-report)
      CHECKLIST_REPORT_FILE="$2"
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

if [[ -z "$CONSISTENCY_ID" ]]; then
  CONSISTENCY_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_WEEKLY_CHECKLIST_CONSISTENCY_${CONSISTENCY_ID}.md"
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

WEEKLY_REPORT_FILE="$(resolve_input_path "$WEEKLY_REPORT_FILE")"
CHECKLIST_REPORT_FILE="$(resolve_input_path "$CHECKLIST_REPORT_FILE")"
OUTPUT_FILE="$(resolve_output_path "$OUTPUT_FILE")"

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] consistency_id=$CONSISTENCY_ID"
  echo "[DRY-RUN] weekly_report=$WEEKLY_REPORT_FILE"
  echo "[DRY-RUN] checklist_report=$CHECKLIST_REPORT_FILE"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$WEEKLY_REPORT_FILE" "$CHECKLIST_REPORT_FILE"; do
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

normalize_path_like() {
  local value="$1"
  value="${value//$PROJECT_ROOT\//}"
  echo "$value"
}

checklist_readiness="$(trim "$(extract_metric "$CHECKLIST_REPORT_FILE" "readiness")")"
checklist_blocking="$(trim "$(extract_metric "$CHECKLIST_REPORT_FILE" "blocking_reasons")")"
checklist_hold_overdue="$(to_int_or_zero "$(extract_metric "$CHECKLIST_REPORT_FILE" "hold_overdue")")"

weekly_status="$(trim "$(extract_metric "$WEEKLY_REPORT_FILE" "weekly_status")")"
weekly_hold_overdue_total="$(to_int_or_zero "$(extract_metric "$WEEKLY_REPORT_FILE" "hold_overdue_total")")"
weekly_checklist_fail="$(to_int_or_zero "$(extract_metric "$WEEKLY_REPORT_FILE" "checklist_readiness_fail")")"
weekly_checklist_warn="$(to_int_or_zero "$(extract_metric "$WEEKLY_REPORT_FILE" "checklist_readiness_warn")")"
weekly_linkage_risk_total="$(to_int_or_zero "$(extract_metric "$WEEKLY_REPORT_FILE" "linkage_risk_total")")"
weekly_checklist_inputs="$(to_int_or_zero "$(extract_metric "$WEEKLY_REPORT_FILE" "checklist_report_inputs")")"

[[ -z "$checklist_readiness" ]] && checklist_readiness="unknown"
[[ -z "$checklist_blocking" ]] && checklist_blocking="none"
[[ -z "$weekly_status" ]] && weekly_status="unknown"

checklist_rel="$(normalize_path_like "$CHECKLIST_REPORT_FILE")"
checklist_base="$(basename "$CHECKLIST_REPORT_FILE")"

weekly_row_found=false
weekly_row_source="n/a"
weekly_row_readiness="n/a"
weekly_row_blocking="none"

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 _ <<< "$row"

  source_raw="$(trim "$c1")"
  source_norm="$(normalize_path_like "$source_raw")"
  source_base="$(basename "$source_raw")"
  readiness_raw="$(trim "$c2")"
  blocking_raw="$(trim "$c3")"

  if [[ "$source_norm" == "$checklist_rel" || "$source_raw" == "$CHECKLIST_REPORT_FILE" || "$source_base" == "$checklist_base" ]]; then
    weekly_row_found=true
    weekly_row_source="$source_raw"
    weekly_row_readiness="$readiness_raw"
    weekly_row_blocking="$blocking_raw"
    break
  fi
done < <(extract_section_rows "$WEEKLY_REPORT_FILE" "5) Checklist Aggregate")

[[ -z "$weekly_row_readiness" ]] && weekly_row_readiness="n/a"
[[ -z "$weekly_row_blocking" ]] && weekly_row_blocking="none"

check_rows_file="$(mktemp)"
trap 'rm -f "$check_rows_file"' EXIT

critical_fail_count=0
warning_count=0

add_check() {
  local check_id="$1"
  local level="$2"
  local result="$3"
  local expected="$4"
  local actual="$5"
  local note="$6"

  echo "$check_id|$level|$result|$expected|$actual|$note" >> "$check_rows_file"

  if [[ "$result" != "pass" ]]; then
    if [[ "$level" == "critical" ]]; then
      critical_fail_count=$((critical_fail_count + 1))
    else
      warning_count=$((warning_count + 1))
    fi
  fi
}

if [[ "$weekly_row_found" == "true" ]]; then
  add_check "checklist_row_present" "critical" "pass" "weekly checklist aggregate contains checklist source" "$weekly_row_source" "source row found"
else
  add_check "checklist_row_present" "critical" "fail" "weekly checklist aggregate contains checklist source" "not found" "source row missing"
fi

if [[ "$weekly_row_found" == "true" && "$weekly_row_readiness" == "$checklist_readiness" ]]; then
  add_check "readiness_echo" "critical" "pass" "weekly row readiness == checklist readiness" "$weekly_row_readiness vs $checklist_readiness" "aligned"
else
  add_check "readiness_echo" "critical" "fail" "weekly row readiness == checklist readiness" "$weekly_row_readiness vs $checklist_readiness" "readiness mismatch"
fi

readiness_count_result="pass"
readiness_count_note="aligned"
readiness_count_expected=""
readiness_count_actual="fail=$weekly_checklist_fail; warn=$weekly_checklist_warn; checklist_inputs=$weekly_checklist_inputs"

case "$checklist_readiness" in
  fail)
    readiness_count_expected="weekly checklist_readiness_fail > 0"
    if (( weekly_checklist_fail <= 0 )); then
      readiness_count_result="fail"
      readiness_count_note="checklist fail not reflected in weekly count"
    fi
    ;;
  warn)
    readiness_count_expected="weekly checklist_readiness_warn > 0"
    if (( weekly_checklist_warn <= 0 )); then
      readiness_count_result="fail"
      readiness_count_note="checklist warn not reflected in weekly count"
    fi
    ;;
  pass)
    if (( weekly_checklist_inputs <= 1 )); then
      readiness_count_expected="weekly checklist_readiness_fail == 0 and warn == 0"
      if (( weekly_checklist_fail > 0 || weekly_checklist_warn > 0 )); then
        readiness_count_result="fail"
        readiness_count_note="single-checklist scope should not have fail/warn counts"
      fi
    else
      readiness_count_expected="weekly checklist_readiness_fail == 0 (multi-checklist relaxed)"
      if (( weekly_checklist_fail > 0 )); then
        readiness_count_result="fail"
        readiness_count_note="fail count exists while current checklist is pass"
      else
        readiness_count_note="multi-checklist mode: warn count may come from other checklist rows"
      fi
    fi
    ;;
  *)
    readiness_count_expected="checklist readiness should be pass/warn/fail"
    readiness_count_result="fail"
    readiness_count_note="unknown checklist readiness"
    ;;
esac

add_check "readiness_count_alignment" "critical" "$readiness_count_result" "$readiness_count_expected" "$readiness_count_actual" "$readiness_count_note"

blocking_result="pass"
blocking_expected="weekly row blocking_reasons aligns with checklist blocking_reasons"
blocking_actual="$weekly_row_blocking vs $checklist_blocking"
blocking_note="aligned"

if [[ "$weekly_row_found" != "true" ]]; then
  blocking_result="fail"
  blocking_note="cannot compare without checklist row"
else
  if [[ "$checklist_blocking" == "none" ]]; then
    if [[ "$weekly_row_blocking" != "none" && -n "$weekly_row_blocking" ]]; then
      blocking_result="fail"
      blocking_note="weekly has blocking reasons but checklist reports none"
    fi
  else
    IFS=',' read -r -a reasons <<< "$checklist_blocking"
    for reason in "${reasons[@]}"; do
      reason="$(trim "$reason")"
      [[ -z "$reason" ]] && continue
      if ! echo "$weekly_row_blocking" | tr ',' '\n' | sed -E 's/^[[:space:]]+//; s/[[:space:]]+$//' | grep -Fxq "$reason"; then
        blocking_result="fail"
        blocking_note="reason '$reason' missing in weekly row"
        break
      fi
    done
  fi
fi

add_check "blocking_reason_alignment" "critical" "$blocking_result" "$blocking_expected" "$blocking_actual" "$blocking_note"

hold_signal_result="pass"
hold_signal_expected="if checklist hold_overdue > 0 then weekly hold_overdue_total > 0"
hold_signal_actual="checklist_hold_overdue=$checklist_hold_overdue; weekly_hold_overdue_total=$weekly_hold_overdue_total"
hold_signal_note="aligned"

if (( checklist_hold_overdue > 0 && weekly_hold_overdue_total <= 0 )); then
  hold_signal_result="fail"
  hold_signal_note="weekly missing overdue signal from checklist"
fi

add_check "hold_overdue_signal_alignment" "warning" "$hold_signal_result" "$hold_signal_expected" "$hold_signal_actual" "$hold_signal_note"

status_guard_result="pass"
status_guard_expected="if checklist readiness is fail/warn then weekly_status should not be pass"
status_guard_actual="checklist_readiness=$checklist_readiness; weekly_status=$weekly_status"
status_guard_note="aligned"

if [[ "$checklist_readiness" == "fail" || "$checklist_readiness" == "warn" ]]; then
  if [[ "$weekly_status" == "pass" ]]; then
    status_guard_result="fail"
    status_guard_note="weekly status pass conflicts with non-pass checklist readiness"
  fi
fi

add_check "weekly_status_guard" "critical" "$status_guard_result" "$status_guard_expected" "$status_guard_actual" "$status_guard_note"

status_rule_result="pass"
status_rule_expected="weekly_status should match weekly fail/warn triggering rules"
status_rule_actual="weekly_status=$weekly_status; fail=$weekly_checklist_fail; hold_overdue=$weekly_hold_overdue_total; linkage_risk=$weekly_linkage_risk_total; warn=$weekly_checklist_warn"
status_rule_note="aligned"

if [[ "$weekly_status" == "pass" ]]; then
  if (( weekly_checklist_fail > 0 || weekly_hold_overdue_total > 0 || weekly_linkage_risk_total > 0 || weekly_checklist_warn > 0 )); then
    status_rule_result="fail"
    status_rule_note="weekly status pass conflicts with fail/warn trigger metrics"
  fi
elif [[ "$weekly_status" == "warn" ]]; then
  if (( weekly_checklist_fail > 0 || weekly_hold_overdue_total > 0 || weekly_linkage_risk_total > 0 )); then
    status_rule_result="fail"
    status_rule_note="weekly status warn conflicts with fail trigger metrics"
  fi
elif [[ "$weekly_status" == "fail" ]]; then
  if (( weekly_checklist_fail == 0 && weekly_hold_overdue_total == 0 && weekly_linkage_risk_total == 0 )); then
    status_rule_result="fail"
    status_rule_note="weekly status fail lacks supporting fail trigger metrics"
  fi
else
  status_rule_result="fail"
  status_rule_note="weekly status is unknown"
fi

add_check "weekly_status_rule_consistency" "critical" "$status_rule_result" "$status_rule_expected" "$status_rule_actual" "$status_rule_note"

consistency_status="pass"
if (( critical_fail_count > 0 )); then
  consistency_status="fail"
elif (( warning_count > 0 )); then
  consistency_status="warn"
fi

release_recommendation="consistent-can-proceed"
if [[ "$consistency_status" == "fail" ]]; then
  release_recommendation="block-until-weekly-checklist-aligned"
elif [[ "$consistency_status" == "warn" ]]; then
  release_recommendation="review-warnings-before-release"
fi

total_checks="$(wc -l < "$check_rows_file" | tr -d ' ')"
passed_checks=$((total_checks - critical_fail_count - warning_count))

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Weekly vs Checklist Consistency Report（Draft）

## 1) Metadata

| field | value |
|------|-------|
| consistency_id | $CONSISTENCY_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| weekly_report | $WEEKLY_REPORT_FILE |
| checklist_report | $CHECKLIST_REPORT_FILE |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| checklist_readiness | $checklist_readiness |
| checklist_blocking_reasons | $checklist_blocking |
| checklist_hold_overdue | $checklist_hold_overdue |
| weekly_status | $weekly_status |
| weekly_checklist_fail | $weekly_checklist_fail |
| weekly_checklist_warn | $weekly_checklist_warn |
| weekly_hold_overdue_total | $weekly_hold_overdue_total |
| weekly_linkage_risk_total | $weekly_linkage_risk_total |
| weekly_checklist_inputs | $weekly_checklist_inputs |

## 3) Consistency Summary

| metric | value |
|--------|-------|
| total_checks | $total_checks |
| passed_checks | $passed_checks |
| critical_fail_count | $critical_fail_count |
| warning_count | $warning_count |
| consistency_status | $consistency_status |
| release_recommendation | $release_recommendation |

## 4) Consistency Checks

| check_id | level | result | expected | actual | note |
|----------|-------|--------|----------|--------|------|
EOF_REPORT

while IFS='|' read -r check_id level result expected actual note; do
  echo "| $check_id | $level | $result | $expected | $actual | $note |" >> "$OUTPUT_FILE"
done < "$check_rows_file"

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Source Row Match

| item | value |
|------|-------|
EOF_APPEND

echo "| weekly_row_found | $weekly_row_found |" >> "$OUTPUT_FILE"
echo "| weekly_row_source | $weekly_row_source |" >> "$OUTPUT_FILE"
echo "| weekly_row_readiness | $weekly_row_readiness |" >> "$OUTPUT_FILE"
echo "| weekly_row_blocking_reasons | $weekly_row_blocking |" >> "$OUTPUT_FILE"

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 6) Suggested Actions

- blocking:
  - $release_recommendation
- followup:
  - sync-weekly-and-checklist-before-next-cut
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$consistency_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass consistency status: $consistency_status" >&2
  exit 1
fi

echo "[PASS] weekly-checklist consistency check completed"
