#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

TRACKER_ID=""
WRITEBACK_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md"
LINKAGE_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_RETEST_APPROVAL_WRITEBACK_LINKAGE_SAMPLE_B44.md"
ADAPTIVE_POLICY_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_CONVERGENCE_ADAPTIVE_THRESHOLD_POLICY_SAMPLE_B45.md"
VERSIONING_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md"
ANOMALY_RESPONSE_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_EVIDENCE_ANOMALY_GRADING_RESPONSE_SAMPLE_B47.md"
SLA_ALERT_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md"
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计回写变更覆盖率修复追踪脚本（Draft）

用途：
  聚合 B42/B44/B45/B46/B47/B48 结果，输出回写变更覆盖率修复追踪与行动队列。

用法：
  scripts/track_archive_audit_writeback_change_coverage_remediation_draft.sh [options]

选项：
  --tracker-id ID              追踪 ID（默认: yyyyMMdd_HHmmss）
  --writeback-report FILE      回写报告（默认: B42 样例）
  --linkage-report FILE        联动一致性报告（默认: B44 样例）
  --adaptive-policy FILE       阈值策略报告（默认: B45 样例）
  --versioning-report FILE     版本化回滚报告（默认: B46 样例）
  --anomaly-response FILE      异常处置报告（默认: B47 样例）
  --sla-alert-report FILE      SLA 违约预警报告（默认: B48 样例）
  --operator NAME              操作人/作业名（默认: codex）
  --output FILE                输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CHANGE_COVERAGE_REMEDIATION_TRACKER_<id>.md）
  --strict                     tracker_status 非 pass 时返回非 0
  --dry-run                    仅打印计划，不写文件
  --help                       显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --tracker-id)
      TRACKER_ID="$2"
      shift 2
      ;;
    --writeback-report)
      WRITEBACK_REPORT="$2"
      shift 2
      ;;
    --linkage-report)
      LINKAGE_REPORT="$2"
      shift 2
      ;;
    --adaptive-policy)
      ADAPTIVE_POLICY_REPORT="$2"
      shift 2
      ;;
    --versioning-report)
      VERSIONING_REPORT="$2"
      shift 2
      ;;
    --anomaly-response)
      ANOMALY_RESPONSE_REPORT="$2"
      shift 2
      ;;
    --sla-alert-report)
      SLA_ALERT_REPORT="$2"
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

if [[ -z "$TRACKER_ID" ]]; then
  TRACKER_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CHANGE_COVERAGE_REMEDIATION_TRACKER_${TRACKER_ID}.md"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] tracker_id=$TRACKER_ID"
  echo "[DRY-RUN] writeback_report=$WRITEBACK_REPORT"
  echo "[DRY-RUN] linkage_report=$LINKAGE_REPORT"
  echo "[DRY-RUN] adaptive_policy_report=$ADAPTIVE_POLICY_REPORT"
  echo "[DRY-RUN] versioning_report=$VERSIONING_REPORT"
  echo "[DRY-RUN] anomaly_response_report=$ANOMALY_RESPONSE_REPORT"
  echo "[DRY-RUN] sla_alert_report=$SLA_ALERT_REPORT"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$WRITEBACK_REPORT" "$LINKAGE_REPORT" "$ADAPTIVE_POLICY_REPORT" "$VERSIONING_REPORT" "$ANOMALY_RESPONSE_REPORT" "$SLA_ALERT_REPORT"; do
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
  value="${value//%/}"
  if [[ "$value" =~ ^[0-9]+$ ]]; then
    echo "$value"
  else
    echo 0
  fi
}

normalize_status() {
  case "$1" in
    pass|warn|fail|unknown|open|queued|closed|pending|in-progress|done|waived|review|stable|reinforce|ok|breach-risk-high|breach-risk-medium|watch)
      echo "$1"
      ;;
    inprogress)
      echo "in-progress"
      ;;
    *)
      echo "unknown"
      ;;
  esac
}

severity_to_priority() {
  case "$1" in
    critical) echo "critical" ;;
    high) echo "high" ;;
    medium) echo "medium" ;;
    low) echo "medium" ;;
    *) echo "medium" ;;
  esac
}

priority_to_sla() {
  case "$1" in
    critical) echo "<1h" ;;
    high) echo "4h" ;;
    medium) echo "1bd" ;;
    *) echo "1bd" ;;
  esac
}

alert_to_priority() {
  case "$1" in
    breach-risk-high) echo "critical" ;;
    breach-risk-medium) echo "high" ;;
    watch) echo "medium" ;;
    *) echo "medium" ;;
  esac
}

writeback_status="$(normalize_status "$(trim "$(extract_metric "$WRITEBACK_REPORT" "writeback_status")")")"
writeback_signaled_items="$(to_int_or_zero "$(extract_metric "$WRITEBACK_REPORT" "retest_signaled_items")")"
writeback_changed_items="$(to_int_or_zero "$(extract_metric "$WRITEBACK_REPORT" "writeback_changed_items")")"
writeback_pending_items="$(to_int_or_zero "$(extract_metric "$WRITEBACK_REPORT" "writeback_pending_items")")"
writeback_inprogress_items="$(to_int_or_zero "$(extract_metric "$WRITEBACK_REPORT" "writeback_inprogress_items")")"

linkage_status="$(normalize_status "$(trim "$(extract_metric "$LINKAGE_REPORT" "linkage_status")")")"
mismatch_rows="$(to_int_or_zero "$(extract_metric "$LINKAGE_REPORT" "mismatch_rows")")"
missing_payload_rows="$(to_int_or_zero "$(extract_metric "$LINKAGE_REPORT" "missing_payload_rows")")"

adaptive_status="$(normalize_status "$(trim "$(extract_metric "$ADAPTIVE_POLICY_REPORT" "adaptive_status")")")"
pressure_score="$(to_int_or_zero "$(extract_metric "$ADAPTIVE_POLICY_REPORT" "pressure_score")")"

versioning_status="$(normalize_status "$(trim "$(extract_metric "$VERSIONING_REPORT" "versioning_status")")")"
rollback_candidates="$(to_int_or_zero "$(extract_metric "$VERSIONING_REPORT" "rollback_candidates")")"

response_status="$(normalize_status "$(trim "$(extract_metric "$ANOMALY_RESPONSE_REPORT" "response_status")")")"
critical_high_open="$(to_int_or_zero "$(extract_metric "$ANOMALY_RESPONSE_REPORT" "critical_high_open")")"

sla_breach_status="$(normalize_status "$(trim "$(extract_metric "$SLA_ALERT_REPORT" "sla_breach_status")")")"
critical_alert_items="$(to_int_or_zero "$(extract_metric "$SLA_ALERT_REPORT" "critical_alert_items")")"
high_alert_items="$(to_int_or_zero "$(extract_metric "$SLA_ALERT_REPORT" "high_alert_items")")"

policy_writeback_result="unknown"
policy_writeback_observed="n/a"
while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 _ <<< "$row"

  check_id="$(trim "$c1")"
  observed="$(trim "$c2")"
  result="$(normalize_status "$(trim "$c4")")"

  if [[ "$check_id" == "writeback-change-coverage" ]]; then
    policy_writeback_result="$result"
    policy_writeback_observed="$observed"
    break
  fi
done < <(extract_section_rows "$ADAPTIVE_POLICY_REPORT" "5) Decision Queue")

rows_file="$(mktemp)"
queue_file="$(mktemp)"
owner_file="$(mktemp)"
trap 'rm -f "$rows_file" "$queue_file" "$owner_file"' EXIT

total_gap_items=0
critical_gap_items=0
high_gap_items=0
medium_gap_items=0
remediation_queue_items=0
unresolved_payload_items=0

coverage_percent=0
if (( writeback_signaled_items > 0 )); then
  coverage_percent=$(( writeback_changed_items * 100 / writeback_signaled_items ))
fi

declare -A OWNER_CRITICAL
declare -A OWNER_HIGH
declare -A OWNER_MEDIUM
declare -A OWNER_TOTAL

track_owner_bump() {
  local owner="$1"
  local priority="$2"

  OWNER_TOTAL["$owner"]=$(( ${OWNER_TOTAL["$owner"]:-0} + 1 ))
  case "$priority" in
    critical)
      OWNER_CRITICAL["$owner"]=$(( ${OWNER_CRITICAL["$owner"]:-0} + 1 ))
      ;;
    high)
      OWNER_HIGH["$owner"]=$(( ${OWNER_HIGH["$owner"]:-0} + 1 ))
      ;;
    medium)
      OWNER_MEDIUM["$owner"]=$(( ${OWNER_MEDIUM["$owner"]:-0} + 1 ))
      ;;
    *) ;;
  esac
}

add_tracking_row() {
  local item_id="$1"
  local source="$2"
  local priority="$3"
  local owner="$4"
  local sla="$5"
  local current_status="$6"
  local target_status="$7"
  local remediation_action="$8"
  local evidence="$9"

  if [[ -z "$owner" || "$owner" == "n/a" ]]; then
    owner="unassigned"
  fi

  total_gap_items=$((total_gap_items + 1))
  case "$priority" in
    critical) critical_gap_items=$((critical_gap_items + 1)) ;;
    high) high_gap_items=$((high_gap_items + 1)) ;;
    *) medium_gap_items=$((medium_gap_items + 1)) ;;
  esac

  track_owner_bump "$owner" "$priority"

  echo "$item_id|$source|$priority|$owner|$sla|$current_status|$target_status|$remediation_action|$evidence" >> "$rows_file"

  if [[ "$current_status" != "closed" && "$current_status" != "done" && "$current_status" != "waived" && "$current_status" != "pass" && "$current_status" != "ok" ]]; then
    remediation_queue_items=$((remediation_queue_items + 1))
    echo "$item_id|$priority|$owner|$sla|$remediation_action|$current_status" >> "$queue_file"
  fi
}

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 _ <<< "$row"

  blocker_code="$(trim "$c1")"
  severity="$(trim "$c4")"
  owner="$(trim "$c5")"
  current_status="$(normalize_status "$(trim "$c8")")"
  writeback_note="$(trim "$c9")"
  writeback_evidence="$(trim "$c11")"

  if [[ -z "$blocker_code" || "$blocker_code" == "none" || "$blocker_code" == "<BLK-001>" ]]; then
    continue
  fi

  if [[ "$current_status" == "done" || "$current_status" == "waived" || "$current_status" == "closed" ]]; then
    continue
  fi

  unresolved_payload_items=$((unresolved_payload_items + 1))
  priority="$(severity_to_priority "$severity")"
  sla="$(priority_to_sla "$priority")"
  add_tracking_row "WB-${blocker_code}" "writeback_payload" "$priority" "$owner" "$sla" "$current_status" "done" "execute-writeback-change-for-${blocker_code}" "${writeback_note}; ${writeback_evidence}"
done < <(extract_section_rows "$WRITEBACK_REPORT" "4) Writeback Rows")

if [[ "$policy_writeback_result" != "pass" && "$policy_writeback_result" != "stable" && "$policy_writeback_result" != "unknown" ]]; then
  policy_priority="high"
  if [[ "$policy_writeback_result" == "fail" ]]; then
    policy_priority="critical"
  fi
  policy_sla="$(priority_to_sla "$policy_priority")"
  add_tracking_row "POL-writeback-change-coverage" "adaptive_policy" "$policy_priority" "release-manager" "$policy_sla" "$policy_writeback_result" "pass" "resolve-policy-check-writeback-change-coverage" "$policy_writeback_observed"
fi

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 _ <<< "$row"

  anomaly_id="$(trim "$c1")"
  severity="$(trim "$c2")"
  owner="$(trim "$c3")"
  sla="$(trim "$c4")"
  immediate_action="$(trim "$c5")"
  status="$(normalize_status "$(trim "$c6")")"

  if [[ -z "$anomaly_id" || "$anomaly_id" == "none" || "$anomaly_id" == "<A-001>" ]]; then
    continue
  fi

  if [[ "$status" == "closed" || "$status" == "pass" || "$status" == "done" ]]; then
    continue
  fi

  if [[ "$severity" != "critical" && "$severity" != "high" && "$immediate_action" != *"writeback"* && "$immediate_action" != *"rollback"* ]]; then
    continue
  fi

  priority="$(severity_to_priority "$severity")"
  if [[ -z "$sla" || "$sla" == "n/a" ]]; then
    sla="$(priority_to_sla "$priority")"
  fi
  add_tracking_row "$anomaly_id" "anomaly_response" "$priority" "$owner" "$sla" "$status" "closed" "$immediate_action" "from-response-queue"
done < <(extract_section_rows "$ANOMALY_RESPONSE_REPORT" "5) Response Queue")

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 c7 c8 _ <<< "$row"

  alert_id="$(trim "$c1")"
  owner="$(trim "$c3")"
  target_sla="$(trim "$c4")"
  observed="$(trim "$c6")"
  alert_level="$(normalize_status "$(trim "$c7")")"
  escalation_action="$(trim "$c8")"

  if [[ -z "$alert_id" || "$alert_id" == "none" || "$alert_id" == "<ALERT-001>" ]]; then
    continue
  fi

  if [[ "$alert_level" == "ok" || "$alert_level" == "pass" ]]; then
    continue
  fi

  if [[ "$escalation_action" != *"writeback"* && "$escalation_action" != *"rollback"* ]]; then
    continue
  fi

  priority="$(alert_to_priority "$alert_level")"
  if [[ -z "$target_sla" || "$target_sla" == "n/a" ]]; then
    target_sla="$(priority_to_sla "$priority")"
  fi

  add_tracking_row "$alert_id" "sla_alert" "$priority" "$owner" "$target_sla" "$alert_level" "ok" "$escalation_action" "$observed"
done < <(extract_section_rows "$SLA_ALERT_REPORT" "4) Alert Rows")

owner_hotspots=0
for owner in "${!OWNER_TOTAL[@]}"; do
  critical_items="${OWNER_CRITICAL[$owner]:-0}"
  high_items="${OWNER_HIGH[$owner]:-0}"
  medium_items="${OWNER_MEDIUM[$owner]:-0}"
  total_items="${OWNER_TOTAL[$owner]:-0}"

  recommended_window="next-weekly"
  if (( critical_items > 0 )); then
    recommended_window="<1h"
  elif (( high_items > 0 )); then
    recommended_window="4h"
  elif (( medium_items > 0 )); then
    recommended_window="1bd"
  fi

  if (( critical_items + high_items >= 3 )); then
    owner_hotspots=$((owner_hotspots + 1))
  fi

  echo "$owner|$critical_items|$high_items|$medium_items|$total_items|$recommended_window" >> "$owner_file"
done

tracker_status="pass"
release_advice="proceed-with-standard-writeback-coverage-monitoring"

if (( writeback_signaled_items == 0 )); then
  tracker_status="warn"
  release_advice="insufficient-writeback-signals-for-coverage-tracking"
fi

if (( writeback_signaled_items > 0 && writeback_changed_items == 0 )); then
  tracker_status="fail"
  release_advice="block-release-and-execute-writeback-remediation-sprint"
elif (( writeback_signaled_items > 0 && coverage_percent < 50 )); then
  tracker_status="fail"
  release_advice="block-release-until-writeback-coverage-reaches-50-percent"
elif (( writeback_signaled_items > 0 && coverage_percent < 80 )) && [[ "$tracker_status" != "fail" ]]; then
  tracker_status="warn"
  release_advice="proceed-with-daily-writeback-coverage-watchlist"
fi

if (( mismatch_rows > 0 || missing_payload_rows > 0 )); then
  tracker_status="fail"
  release_advice="block-release-and-fix-linkage-payload-inconsistency"
fi

if [[ "$policy_writeback_result" == "fail" || "$response_status" == "fail" || "$sla_breach_status" == "fail" ]]; then
  tracker_status="fail"
fi

if (( critical_alert_items > 0 )) && [[ "$tracker_status" == "pass" || "$tracker_status" == "warn" ]]; then
  tracker_status="fail"
  release_advice="block-release-and-close-critical-sla-writeback-alerts"
fi

if (( rollback_candidates > 0 )) && [[ "$tracker_status" == "pass" ]]; then
  tracker_status="warn"
  release_advice="proceed-with-rollback-watch-and-coverage-remediation"
fi

if (( total_gap_items == 0 )) && (( writeback_signaled_items > 0 )) && (( writeback_changed_items >= writeback_signaled_items )); then
  tracker_status="pass"
  release_advice="writeback-coverage-gap-closed"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Writeback Change Coverage Remediation Tracker（Draft）

## 1) Metadata

| field | value |
|------|-------|
| tracker_id | $TRACKER_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| writeback_report | $WRITEBACK_REPORT |
| linkage_report | $LINKAGE_REPORT |
| adaptive_policy_report | $ADAPTIVE_POLICY_REPORT |
| versioning_report | $VERSIONING_REPORT |
| anomaly_response_report | $ANOMALY_RESPONSE_REPORT |
| sla_alert_report | $SLA_ALERT_REPORT |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| writeback_status | $writeback_status |
| writeback_signaled_items | $writeback_signaled_items |
| writeback_changed_items | $writeback_changed_items |
| writeback_pending_items | $writeback_pending_items |
| writeback_inprogress_items | $writeback_inprogress_items |
| linkage_status | $linkage_status |
| mismatch_rows | $mismatch_rows |
| missing_payload_rows | $missing_payload_rows |
| adaptive_status | $adaptive_status |
| pressure_score | $pressure_score |
| policy_writeback_change_result | $policy_writeback_result |
| versioning_status | $versioning_status |
| rollback_candidates | $rollback_candidates |
| anomaly_response_status | $response_status |
| critical_high_open | $critical_high_open |
| sla_breach_status | $sla_breach_status |
| critical_alert_items | $critical_alert_items |
| high_alert_items | $high_alert_items |

## 3) Coverage Tracking Summary

| metric | value |
|--------|-------|
| writeback_change_coverage_percent | ${coverage_percent}% |
| unresolved_payload_items | $unresolved_payload_items |
| total_gap_items | $total_gap_items |
| remediation_queue_items | $remediation_queue_items |
| critical_gap_items | $critical_gap_items |
| high_gap_items | $high_gap_items |
| medium_gap_items | $medium_gap_items |
| owner_hotspots | $owner_hotspots |
| tracker_status | $tracker_status |
| release_advice | $release_advice |

## 4) Coverage Gap Rows

| item_id | source | priority | owner | sla | current_status | target_status | remediation_action | evidence |
|---------|--------|----------|-------|-----|----------------|---------------|--------------------|----------|
EOF_REPORT

if [[ -s "$rows_file" ]]; then
  while IFS='|' read -r item_id source priority owner sla current_status target_status remediation_action evidence; do
    echo "| $item_id | $source | $priority | $owner | $sla | $current_status | $target_status | $remediation_action | $evidence |" >> "$OUTPUT_FILE"
  done < "$rows_file"
else
  echo "| none | n/a | medium | unassigned | 1bd | closed | closed | no-action | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Remediation Queue

| item_id | priority | owner | sla | immediate_action | status |
|---------|----------|-------|-----|------------------|--------|
EOF_APPEND

if [[ -s "$queue_file" ]]; then
  while IFS='|' read -r item_id priority owner sla immediate_action status; do
    echo "| $item_id | $priority | $owner | $sla | $immediate_action | $status |" >> "$OUTPUT_FILE"
  done < "$queue_file"
else
  echo "| none | n/a | unassigned | 1bd | no-action | closed |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 6) Owner Workload

| owner | critical_items | high_items | medium_items | total_items | recommended_window |
|-------|----------------|------------|--------------|-------------|--------------------|
EOF_APPEND

if [[ -s "$owner_file" ]]; then
  while IFS='|' read -r owner critical_items high_items medium_items total_items recommended_window; do
    echo "| $owner | $critical_items | $high_items | $medium_items | $total_items | $recommended_window |" >> "$OUTPUT_FILE"
  done < "$owner_file"
else
  echo "| unassigned | 0 | 0 | 0 | 0 | next-weekly |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 7) Suggested Actions

- immediate:
  - $release_advice
- followup:
  - rerun-writeback-coverage-remediation-tracker-after-action-closure
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$tracker_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass tracker status: $tracker_status" >&2
  exit 1
fi

echo "[PASS] writeback change coverage remediation tracker generated"
