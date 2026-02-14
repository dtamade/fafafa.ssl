#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

MATRIX_ID=""
DASHBOARD_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md"
CHECKLIST_FILE="$PROJECT_ROOT/docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md"
HOLD_REVIEW_FILE="$PROJECT_ROOT/docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md"
WEEKLY_REPORT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md"
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计风险分级与响应矩阵生成脚本（Draft）

用途：
  基于 dashboard/checklist/hold/weekly 报告输出统一风险分级与响应矩阵。

用法：
  scripts/generate_archive_audit_risk_response_draft.sh [options]

选项：
  --matrix-id ID        风险矩阵 ID（默认: yyyyMMdd_HHmmss）
  --dashboard FILE      状态看板报告（默认: docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md）
  --checklist FILE      发布前核查清单（默认: docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md）
  --hold-review FILE    hold 到期复核报告（默认: docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md）
  --weekly-report FILE  周报（默认: docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md）
  --operator NAME       操作人/作业名（默认: codex）
  --output FILE         输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_RISK_RESPONSE_<id>.md）
  --strict              风险级别非 low 时返回非 0
  --dry-run             仅打印计划，不写文件
  --help                显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --matrix-id)
      MATRIX_ID="$2"
      shift 2
      ;;
    --dashboard)
      DASHBOARD_FILE="$2"
      shift 2
      ;;
    --checklist)
      CHECKLIST_FILE="$2"
      shift 2
      ;;
    --hold-review)
      HOLD_REVIEW_FILE="$2"
      shift 2
      ;;
    --weekly-report)
      WEEKLY_REPORT_FILE="$2"
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

if [[ -z "$MATRIX_ID" ]]; then
  MATRIX_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_RISK_RESPONSE_${MATRIX_ID}.md"
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

DASHBOARD_FILE="$(resolve_input_path "$DASHBOARD_FILE")"
CHECKLIST_FILE="$(resolve_input_path "$CHECKLIST_FILE")"
HOLD_REVIEW_FILE="$(resolve_input_path "$HOLD_REVIEW_FILE")"
WEEKLY_REPORT_FILE="$(resolve_input_path "$WEEKLY_REPORT_FILE")"
OUTPUT_FILE="$(resolve_output_path "$OUTPUT_FILE")"

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] matrix_id=$MATRIX_ID"
  echo "[DRY-RUN] dashboard=$DASHBOARD_FILE"
  echo "[DRY-RUN] checklist=$CHECKLIST_FILE"
  echo "[DRY-RUN] hold_review=$HOLD_REVIEW_FILE"
  echo "[DRY-RUN] weekly_report=$WEEKLY_REPORT_FILE"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$DASHBOARD_FILE" "$CHECKLIST_FILE" "$HOLD_REVIEW_FILE" "$WEEKLY_REPORT_FILE"; do
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

severity_to_sla() {
  case "$1" in
    critical) echo "<1h" ;;
    high) echo "4h" ;;
    medium) echo "1bd" ;;
    *) echo "next-weekly" ;;
  esac
}

severity_to_action() {
  case "$1" in
    critical) echo "block-release-and-escalate" ;;
    high) echo "open-incident-and-fix-before-cut" ;;
    medium) echo "track-mitigation-and-verify" ;;
    *) echo "monitor-in-routine" ;;
  esac
}

risk_from_score() {
  local score="$1"
  if (( score >= 9 )); then
    echo "critical"
  elif (( score >= 6 )); then
    echo "high"
  elif (( score >= 3 )); then
    echo "medium"
  else
    echo "low"
  fi
}

dashboard_status="$(trim "$(extract_metric "$DASHBOARD_FILE" "dashboard_status")")"
hold_status="$(trim "$(extract_metric "$DASHBOARD_FILE" "hold_status")")"
linkage_status="$(trim "$(extract_metric "$DASHBOARD_FILE" "linkage_status")")"
checklist_status="$(trim "$(extract_metric "$DASHBOARD_FILE" "checklist_status")")"
weekly_status_dash="$(trim "$(extract_metric "$DASHBOARD_FILE" "weekly_status")")"
hold_overdue_total="$(to_int_or_zero "$(extract_metric "$DASHBOARD_FILE" "hold_overdue_total")")"
hold_due_soon_total="$(to_int_or_zero "$(extract_metric "$DASHBOARD_FILE" "hold_due_soon_total")")"
hold_missing_invalid_total="$(to_int_or_zero "$(extract_metric "$DASHBOARD_FILE" "hold_missing_or_invalid_expiry_total")")"
linkage_risk_total="$(to_int_or_zero "$(extract_metric "$DASHBOARD_FILE" "linkage_risk_total")")"
checklist_fail_total="$(to_int_or_zero "$(extract_metric "$DASHBOARD_FILE" "checklist_readiness_fail")")"
checklist_warn_total="$(to_int_or_zero "$(extract_metric "$DASHBOARD_FILE" "checklist_readiness_warn_or_unknown")")"
weekly_fail_count="$(to_int_or_zero "$(extract_metric "$DASHBOARD_FILE" "weekly_fail_count")")"
blocking_reason_total="$(to_int_or_zero "$(extract_metric "$DASHBOARD_FILE" "blocking_reason_total")")"

readiness="$(trim "$(extract_metric "$CHECKLIST_FILE" "readiness")")"
blocking_reasons="$(trim "$(extract_metric "$CHECKLIST_FILE" "blocking_reasons")")"

hold_overdue="$(to_int_or_zero "$(extract_metric "$HOLD_REVIEW_FILE" "overdue")")"
hold_due_soon="$(to_int_or_zero "$(extract_metric "$HOLD_REVIEW_FILE" "due_soon")")"
hold_missing="$(to_int_or_zero "$(extract_metric "$HOLD_REVIEW_FILE" "missing_expiry")")"
hold_invalid="$(to_int_or_zero "$(extract_metric "$HOLD_REVIEW_FILE" "invalid_expiry")")"

weekly_status_source="$(trim "$(extract_metric "$WEEKLY_REPORT_FILE" "weekly_status")")"

[[ -z "$dashboard_status" ]] && dashboard_status="unknown"
[[ -z "$hold_status" ]] && hold_status="unknown"
[[ -z "$linkage_status" ]] && linkage_status="unknown"
[[ -z "$checklist_status" ]] && checklist_status="unknown"
[[ -z "$weekly_status_dash" ]] && weekly_status_dash="unknown"
[[ -z "$readiness" ]] && readiness="unknown"
[[ -z "$blocking_reasons" ]] && blocking_reasons="none"
[[ -z "$weekly_status_source" ]] && weekly_status_source="unknown"

risk_score=0

case "$dashboard_status" in
  fail) risk_score=$((risk_score + 4)) ;;
  warn|unknown) risk_score=$((risk_score + 2)) ;;
  *) ;;
esac

case "$readiness" in
  fail) risk_score=$((risk_score + 4)) ;;
  warn|unknown) risk_score=$((risk_score + 2)) ;;
  *) ;;
esac

if (( hold_overdue_total > 0 || hold_overdue > 0 )); then
  risk_score=$((risk_score + 4))
fi

if (( hold_missing_invalid_total > 0 || hold_missing + hold_invalid > 0 )); then
  risk_score=$((risk_score + 3))
fi

if (( linkage_risk_total > 0 )); then
  risk_score=$((risk_score + 3))
fi

if (( hold_due_soon_total > 0 || hold_due_soon > 0 )); then
  risk_score=$((risk_score + 1))
fi

if (( weekly_fail_count > 0 )) || [[ "$weekly_status_dash" == "fail" || "$weekly_status_source" == "fail" ]]; then
  risk_score=$((risk_score + 3))
elif [[ "$weekly_status_dash" == "warn" || "$weekly_status_source" == "warn" ]]; then
  risk_score=$((risk_score + 1))
fi

if (( checklist_warn_total > 0 )); then
  risk_score=$((risk_score + 1))
fi

if (( blocking_reason_total >= 3 )); then
  risk_score=$((risk_score + 2))
elif (( blocking_reason_total > 0 )); then
  risk_score=$((risk_score + 1))
fi

overall_risk="$(risk_from_score "$risk_score")"

decision_status="pass"
release_advice="proceed"

case "$overall_risk" in
  critical)
    decision_status="fail"
    release_advice="block-release"
    ;;
  high)
    decision_status="fail"
    release_advice="hold-until-mitigated"
    ;;
  medium)
    decision_status="warn"
    release_advice="proceed-with-mitigation"
    ;;
  low)
    decision_status="pass"
    release_advice="proceed"
    ;;
esac

risk_rows_file="$(mktemp)"
reason_rows_file="$(mktemp)"
trap 'rm -f "$risk_rows_file" "$reason_rows_file"' EXIT

severity_release="low"
if [[ "$readiness" == "fail" || "$checklist_status" == "fail" ]]; then
  severity_release="critical"
elif [[ "$readiness" == "warn" || "$checklist_status" == "warn" || "$checklist_status" == "unknown" ]]; then
  severity_release="medium"
fi

echo "release_checklist|$severity_release|release-manager|$(severity_to_sla "$severity_release")|$(severity_to_action "$severity_release")|readiness=$readiness; checklist_status=$checklist_status" >> "$risk_rows_file"

severity_hold="low"
if (( hold_overdue_total > 0 || hold_overdue > 0 || hold_missing + hold_invalid > 0 || hold_missing_invalid_total > 0 )); then
  severity_hold="critical"
elif (( hold_due_soon_total > 0 || hold_due_soon > 0 )); then
  severity_hold="high"
fi

echo "hold_expiry_control|$severity_hold|qa-secops|$(severity_to_sla "$severity_hold")|$(severity_to_action "$severity_hold")|overdue=$hold_overdue_total/$hold_overdue; due_soon=$hold_due_soon_total/$hold_due_soon" >> "$risk_rows_file"

severity_linkage="low"
if (( linkage_risk_total > 0 )) || [[ "$linkage_status" == "fail" ]]; then
  severity_linkage="high"
elif [[ "$linkage_status" == "warn" || "$linkage_status" == "unknown" ]]; then
  severity_linkage="medium"
fi

echo "audit_linkage_signal|$severity_linkage|audit-owner|$(severity_to_sla "$severity_linkage")|$(severity_to_action "$severity_linkage")|linkage_risk_total=$linkage_risk_total; linkage_status=$linkage_status" >> "$risk_rows_file"

severity_weekly="low"
if (( weekly_fail_count > 0 )) || [[ "$weekly_status_dash" == "fail" || "$weekly_status_source" == "fail" ]]; then
  severity_weekly="high"
elif [[ "$weekly_status_dash" == "warn" || "$weekly_status_source" == "warn" || "$weekly_status_dash" == "unknown" ]]; then
  severity_weekly="medium"
fi

echo "weekly_execution_signal|$severity_weekly|release-ops|$(severity_to_sla "$severity_weekly")|$(severity_to_action "$severity_weekly")|weekly_status=$weekly_status_dash/$weekly_status_source; weekly_fail_count=$weekly_fail_count" >> "$risk_rows_file"

severity_blocking="low"
if (( blocking_reason_total >= 3 )); then
  severity_blocking="high"
elif (( blocking_reason_total > 0 )); then
  severity_blocking="medium"
fi

echo "blocking_reason_density|$severity_blocking|release-manager|$(severity_to_sla "$severity_blocking")|$(severity_to_action "$severity_blocking")|blocking_reason_total=$blocking_reason_total" >> "$risk_rows_file"

if [[ "$blocking_reasons" != "none" ]]; then
  IFS=',' read -r -a reasons <<< "$blocking_reasons"
  for reason in "${reasons[@]}"; do
    reason="$(trim "$reason")"
    [[ -z "$reason" ]] && continue
    echo "$reason" >> "$reason_rows_file"
  done
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Risk Grading & Response Matrix（Draft）

## 1) Metadata

| field | value |
|------|-------|
| matrix_id | $MATRIX_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| dashboard_report | $DASHBOARD_FILE |
| checklist_report | $CHECKLIST_FILE |
| hold_review_report | $HOLD_REVIEW_FILE |
| weekly_report | $WEEKLY_REPORT_FILE |
| operator | $OPERATOR |

## 2) Risk Snapshot

| metric | value |
|--------|-------|
| risk_score | $risk_score |
| overall_risk | $overall_risk |
| decision_status | $decision_status |
| release_advice | $release_advice |
| dashboard_status | $dashboard_status |
| checklist_readiness | $readiness |
| hold_overdue_total | $hold_overdue_total |
| hold_due_soon_total | $hold_due_soon_total |
| hold_missing_or_invalid_expiry_total | $hold_missing_invalid_total |
| linkage_risk_total | $linkage_risk_total |
| weekly_fail_count | $weekly_fail_count |
| blocking_reason_total | $blocking_reason_total |

## 3) Risk Grading Rules

| grade | trigger_example | expected_decision |
|-------|-----------------|-------------------|
| critical | overdue/missing-expiry + checklist fail + weekly fail | block-release |
| high | 关键维度 fail 或阻断原因密集（>=3） | hold-until-mitigated |
| medium | 无 fail 但存在 warn/due-soon | proceed-with-mitigation |
| low | 关键输入全 pass 且无阻断原因 | proceed |

## 4) Response Matrix

| risk_item | severity | owner | sla | response_action | evidence |
|-----------|----------|-------|-----|-----------------|----------|
EOF_REPORT

while IFS='|' read -r risk_item severity owner sla response_action evidence; do
  echo "| $risk_item | $severity | $owner | $sla | $response_action | $evidence |" >> "$OUTPUT_FILE"
done < "$risk_rows_file"

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Blocking Reasons

| reason | status |
|--------|--------|
EOF_APPEND

if [[ -s "$reason_rows_file" ]]; then
  while IFS= read -r reason; do
    echo "| $reason | open |" >> "$OUTPUT_FILE"
  done < "$reason_rows_file"
else
  echo "| none | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 6) Suggested Coordination

- immediate:
  - $(severity_to_action "$overall_risk")
- followup:
  - align-risk-owner-and-update-dashboard
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$overall_risk" != "low" ]]; then
  echo "[FAIL] strict mode detected non-low risk: $overall_risk" >&2
  exit 1
fi

echo "[PASS] archive audit risk response matrix generated"
