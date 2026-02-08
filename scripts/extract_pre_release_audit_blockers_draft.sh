#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

BLOCKER_ID=""
CHECKLIST_FILE="$PROJECT_ROOT/docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md"
WEEKLY_REPORT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md"
RISK_MATRIX_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_RISK_RESPONSE_SAMPLE_B31.md"
DASHBOARD_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md"
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
发布前审计阻断项提取脚本（Draft）

用途：
  从 checklist/weekly/risk-matrix/dashboard 自动提取发布前阻断项清单。

用法：
  scripts/extract_pre_release_audit_blockers_draft.sh [options]

选项：
  --blocker-id ID         阻断清单 ID（默认: yyyyMMdd_HHmmss）
  --checklist FILE        发布前核查清单（默认: docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md）
  --weekly-report FILE    周报（默认: docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md）
  --risk-matrix FILE      风险分级矩阵（默认: docs/test_reports/ARCHIVE_AUDIT_RISK_RESPONSE_SAMPLE_B31.md）
  --dashboard FILE        状态看板（默认: docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md）
  --operator NAME         操作人/作业名（默认: codex）
  --output FILE           输出文件（默认: docs/test_reports/PRE_RELEASE_AUDIT_BLOCKERS_<id>.md）
  --strict                blockers_status 非 pass 时返回非 0
  --dry-run               仅打印计划，不写文件
  --help                  显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --blocker-id)
      BLOCKER_ID="$2"
      shift 2
      ;;
    --checklist)
      CHECKLIST_FILE="$2"
      shift 2
      ;;
    --weekly-report)
      WEEKLY_REPORT_FILE="$2"
      shift 2
      ;;
    --risk-matrix)
      RISK_MATRIX_FILE="$2"
      shift 2
      ;;
    --dashboard)
      DASHBOARD_FILE="$2"
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

if [[ -z "$BLOCKER_ID" ]]; then
  BLOCKER_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/PRE_RELEASE_AUDIT_BLOCKERS_${BLOCKER_ID}.md"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] blocker_id=$BLOCKER_ID"
  echo "[DRY-RUN] checklist=$CHECKLIST_FILE"
  echo "[DRY-RUN] weekly_report=$WEEKLY_REPORT_FILE"
  echo "[DRY-RUN] risk_matrix=$RISK_MATRIX_FILE"
  echo "[DRY-RUN] dashboard=$DASHBOARD_FILE"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$CHECKLIST_FILE" "$WEEKLY_REPORT_FILE" "$RISK_MATRIX_FILE" "$DASHBOARD_FILE"; do
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

checklist_readiness="$(trim "$(extract_metric "$CHECKLIST_FILE" "readiness")")"
checklist_blocking="$(trim "$(extract_metric "$CHECKLIST_FILE" "blocking_reasons")")"

weekly_status="$(trim "$(extract_metric "$WEEKLY_REPORT_FILE" "weekly_status")")"
weekly_hold_overdue="$(to_int_or_zero "$(extract_metric "$WEEKLY_REPORT_FILE" "hold_overdue_total")")"
weekly_checklist_fail="$(to_int_or_zero "$(extract_metric "$WEEKLY_REPORT_FILE" "checklist_readiness_fail")")"

risk_overall="$(trim "$(extract_metric "$RISK_MATRIX_FILE" "overall_risk")")"
risk_decision="$(trim "$(extract_metric "$RISK_MATRIX_FILE" "decision_status")")"
risk_advice="$(trim "$(extract_metric "$RISK_MATRIX_FILE" "release_advice")")"

dashboard_status="$(trim "$(extract_metric "$DASHBOARD_FILE" "dashboard_status")")"
dashboard_blocking_total="$(to_int_or_zero "$(extract_metric "$DASHBOARD_FILE" "blocking_reason_total")")"

[[ -z "$checklist_readiness" ]] && checklist_readiness="unknown"
[[ -z "$checklist_blocking" ]] && checklist_blocking="none"
[[ -z "$weekly_status" ]] && weekly_status="unknown"
[[ -z "$risk_overall" ]] && risk_overall="unknown"
[[ -z "$risk_decision" ]] && risk_decision="unknown"
[[ -z "$risk_advice" ]] && risk_advice="unknown"
[[ -z "$dashboard_status" ]] && dashboard_status="unknown"

blocker_rows_file="$(mktemp)"
trap 'rm -f "$blocker_rows_file"' EXIT

declare -A seen
blocker_index=0

add_blocker() {
  local source="$1"
  local blocker_key="$2"
  local severity="$3"
  local owner="$4"
  local action="$5"
  local evidence="$6"

  local dedup_key="${source}:${blocker_key}"
  if [[ -n "${seen[$dedup_key]:-}" ]]; then
    return
  fi

  seen[$dedup_key]=1
  blocker_index=$((blocker_index + 1))
  local blocker_code
  blocker_code="BLK-$(printf '%03d' "$blocker_index")"
  echo "$blocker_code|$source|$blocker_key|$severity|$owner|$action|$evidence" >> "$blocker_rows_file"
}

if [[ "$checklist_readiness" == "fail" ]]; then
  add_blocker "checklist" "checklist_readiness_fail" "critical" "release-manager" "block-release-and-close-checklist-gaps" "readiness=$checklist_readiness"
elif [[ "$checklist_readiness" == "warn" || "$checklist_readiness" == "unknown" ]]; then
  add_blocker "checklist" "checklist_readiness_warn_or_unknown" "high" "release-manager" "mitigate-checklist-risks-before-cut" "readiness=$checklist_readiness"
fi

if [[ "$checklist_blocking" != "none" ]]; then
  IFS=',' read -r -a reasons <<< "$checklist_blocking"
  for reason in "${reasons[@]}"; do
    reason="$(trim "$reason")"
    [[ -z "$reason" ]] && continue
    add_blocker "checklist" "$reason" "high" "qa-secops" "resolve-blocking-reason" "blocking_reason=$reason"
  done
fi

if [[ "$weekly_status" == "fail" ]]; then
  add_blocker "weekly" "weekly_status_fail" "high" "release-ops" "stabilize-weekly-signals" "weekly_status=$weekly_status"
elif [[ "$weekly_status" == "warn" || "$weekly_status" == "unknown" ]]; then
  add_blocker "weekly" "weekly_status_warn_or_unknown" "medium" "release-ops" "review-weekly-signals" "weekly_status=$weekly_status"
fi

if (( weekly_hold_overdue > 0 )); then
  add_blocker "weekly" "hold_overdue_present" "critical" "qa-secops" "clear-overdue-hold-items" "hold_overdue_total=$weekly_hold_overdue"
fi

if (( weekly_checklist_fail > 0 )); then
  add_blocker "weekly" "checklist_fail_present" "high" "release-manager" "close-checklist-failures" "checklist_readiness_fail=$weekly_checklist_fail"
fi

if [[ "$risk_decision" == "fail" || "$risk_overall" == "critical" || "$risk_overall" == "high" ]]; then
  add_blocker "risk_matrix" "risk_decision_fail" "critical" "risk-owner" "execute-risk-response-before-release" "overall_risk=$risk_overall; decision_status=$risk_decision"
elif [[ "$risk_decision" == "warn" || "$risk_overall" == "medium" ]]; then
  add_blocker "risk_matrix" "risk_decision_warn" "medium" "risk-owner" "track-risk-mitigation" "overall_risk=$risk_overall; decision_status=$risk_decision"
fi

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 _ <<< "$row"

  risk_item="$(trim "$c1")"
  severity="$(trim "$c2")"
  owner="$(trim "$c3")"
  action="$(trim "$c5")"
  evidence="$(trim "$c6")"

  [[ -z "$risk_item" ]] && continue

  if [[ "$severity" == "critical" || "$severity" == "high" ]]; then
    add_blocker "risk_matrix_response" "$risk_item" "$severity" "${owner:-risk-owner}" "${action:-execute-risk-response}" "$evidence"
  fi
done < <(extract_section_rows "$RISK_MATRIX_FILE" "4) Response Matrix")

if [[ "$dashboard_status" == "fail" ]]; then
  add_blocker "dashboard" "dashboard_status_fail" "high" "release-manager" "clear-dashboard-fail-signals" "dashboard_status=$dashboard_status"
elif [[ "$dashboard_status" == "warn" || "$dashboard_status" == "unknown" ]]; then
  add_blocker "dashboard" "dashboard_status_warn_or_unknown" "medium" "release-manager" "review-dashboard-signals" "dashboard_status=$dashboard_status"
fi

if (( dashboard_blocking_total > 0 )); then
  add_blocker "dashboard" "dashboard_blocking_reasons_present" "high" "release-manager" "reduce-dashboard-blocking-reasons" "blocking_reason_total=$dashboard_blocking_total"
fi

critical_count=0
high_count=0
medium_count=0

if [[ -s "$blocker_rows_file" ]]; then
  while IFS='|' read -r _ _ _ severity _ _ _; do
    case "$severity" in
      critical) critical_count=$((critical_count + 1)) ;;
      high) high_count=$((high_count + 1)) ;;
      medium) medium_count=$((medium_count + 1)) ;;
      *) ;;
    esac
  done < "$blocker_rows_file"
fi

blockers_total=$((critical_count + high_count + medium_count))

blockers_status="pass"
release_gate_decision="proceed"
if (( critical_count > 0 )); then
  blockers_status="fail"
  release_gate_decision="block-release"
elif (( high_count > 0 )); then
  blockers_status="fail"
  release_gate_decision="hold-until-high-cleared"
elif (( medium_count > 0 )); then
  blockers_status="warn"
  release_gate_decision="proceed-with-mitigation"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Pre-Release Audit Blockers（Draft）

## 1) Metadata

| field | value |
|------|-------|
| blocker_id | $BLOCKER_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| checklist_report | $CHECKLIST_FILE |
| weekly_report | $WEEKLY_REPORT_FILE |
| risk_matrix_report | $RISK_MATRIX_FILE |
| dashboard_report | $DASHBOARD_FILE |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| checklist_readiness | $checklist_readiness |
| checklist_blocking_reasons | $checklist_blocking |
| weekly_status | $weekly_status |
| weekly_hold_overdue_total | $weekly_hold_overdue |
| weekly_checklist_fail | $weekly_checklist_fail |
| risk_overall | $risk_overall |
| risk_decision_status | $risk_decision |
| risk_release_advice | $risk_advice |
| dashboard_status | $dashboard_status |
| dashboard_blocking_reason_total | $dashboard_blocking_total |

## 3) Blocker Summary

| metric | value |
|--------|-------|
| blockers_total | $blockers_total |
| blockers_critical | $critical_count |
| blockers_high | $high_count |
| blockers_medium | $medium_count |
| blockers_status | $blockers_status |
| release_gate_decision | $release_gate_decision |

## 4) Blocker Items

| blocker_code | source | blocker_key | severity | owner | action | evidence |
|--------------|--------|-------------|----------|-------|--------|----------|
EOF_REPORT

if [[ -s "$blocker_rows_file" ]]; then
  while IFS='|' read -r blocker_code source blocker_key severity owner action evidence; do
    echo "| $blocker_code | $source | $blocker_key | $severity | $owner | $action | $evidence |" >> "$OUTPUT_FILE"
  done < "$blocker_rows_file"
else
  echo "| n/a | n/a | none | low | n/a | none | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Extraction Evidence

| check | result |
|-------|--------|
EOF_APPEND

echo "| checklist_report_readable | pass |" >> "$OUTPUT_FILE"
echo "| weekly_report_readable | pass |" >> "$OUTPUT_FILE"
echo "| risk_matrix_report_readable | pass |" >> "$OUTPUT_FILE"
echo "| dashboard_report_readable | pass |" >> "$OUTPUT_FILE"

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 6) Release Decision

- immediate:
  - $release_gate_decision
- followup:
  - sync-blockers-with-release-owner
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$blockers_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass blockers status: $blockers_status" >&2
  exit 1
fi

echo "[PASS] pre-release audit blockers extracted"
