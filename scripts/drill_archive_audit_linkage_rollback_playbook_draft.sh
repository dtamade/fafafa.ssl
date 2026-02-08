#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

DRILL_ID=""
TRACKER_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CHANGE_COVERAGE_REMEDIATION_TRACKER_SAMPLE_B49.md"
VERSIONING_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md"
ANOMALY_RESPONSE_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_EVIDENCE_ANOMALY_GRADING_RESPONSE_SAMPLE_B47.md"
SLA_ALERT_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md"
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计联动与回滚演练计划脚本（Draft）

用途：
  聚合回写覆盖率追踪、版本化回滚、异常处置和 SLA 预警，生成联动与回滚演练计划。

用法：
  scripts/drill_archive_audit_linkage_rollback_playbook_draft.sh [options]

选项：
  --drill-id ID                演练 ID（默认: yyyyMMdd_HHmmss）
  --tracker-report FILE        回写覆盖率修复追踪报告（默认: B49 样例）
  --versioning-report FILE     版本化回滚报告（默认: B46 样例）
  --anomaly-response FILE      异常处置报告（默认: B47 样例）
  --sla-alert-report FILE      SLA 违约预警报告（默认: B48 样例）
  --operator NAME              操作人/作业名（默认: codex）
  --output FILE                输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_<id>.md）
  --strict                     drill_status 非 pass 时返回非 0
  --dry-run                    仅打印计划，不写文件
  --help                       显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --drill-id)
      DRILL_ID="$2"
      shift 2
      ;;
    --tracker-report)
      TRACKER_REPORT="$2"
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

if [[ -z "$DRILL_ID" ]]; then
  DRILL_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_${DRILL_ID}.md"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] drill_id=$DRILL_ID"
  echo "[DRY-RUN] tracker_report=$TRACKER_REPORT"
  echo "[DRY-RUN] versioning_report=$VERSIONING_REPORT"
  echo "[DRY-RUN] anomaly_response_report=$ANOMALY_RESPONSE_REPORT"
  echo "[DRY-RUN] sla_alert_report=$SLA_ALERT_REPORT"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$TRACKER_REPORT" "$VERSIONING_REPORT" "$ANOMALY_RESPONSE_REPORT" "$SLA_ALERT_REPORT"; do
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
    pass|warn|fail|unknown|open|queued|closed|pending|in-progress|done|waived|planned|ok|review|breach-risk-high|breach-risk-medium|watch)
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

normalize_priority() {
  case "$1" in
    critical|high|medium|low) echo "$1" ;;
    *) echo "medium" ;;
  esac
}

priority_rank() {
  case "$1" in
    critical) echo 4 ;;
    high) echo 3 ;;
    medium) echo 2 ;;
    low) echo 1 ;;
    *) echo 0 ;;
  esac
}

status_rank() {
  case "$1" in
    fail|breach-risk-high|open|pending) echo 5 ;;
    queued|breach-risk-medium|in-progress) echo 4 ;;
    watch|warn|review|planned) echo 3 ;;
    unknown) echo 2 ;;
    closed|done|waived|pass|ok) echo 1 ;;
    *) echo 0 ;;
  esac
}

priority_to_sla() {
  case "$1" in
    critical) echo "<1h" ;;
    high) echo "4h" ;;
    medium|low) echo "1bd" ;;
    *) echo "1bd" ;;
  esac
}

priority_to_minutes() {
  case "$1" in
    critical) echo 60 ;;
    high) echo 120 ;;
    medium|low) echo 240 ;;
    *) echo 180 ;;
  esac
}

phase_for_item() {
  local item_id="$1"
  local action="$2"

  if [[ "$item_id" == RB-* || "$action" == *"rollback"* ]]; then
    echo "rollback-execution"
  elif [[ "$item_id" == WB-* || "$action" == *"writeback"* || "$action" == *"coverage"* ]]; then
    echo "writeback-remediation"
  elif [[ "$item_id" == AUD-* || "$action" == *"anomaly"* ]]; then
    echo "anomaly-closure"
  else
    echo "cross-team-followup"
  fi
}

target_status_for_phase() {
  case "$1" in
    rollback-execution) echo "verified" ;;
    writeback-remediation|anomaly-closure|cross-team-followup) echo "closed" ;;
    *) echo "closed" ;;
  esac
}

tracker_status="$(normalize_status "$(trim "$(extract_metric "$TRACKER_REPORT" "tracker_status")")")"
coverage_percent="$(to_int_or_zero "$(extract_metric "$TRACKER_REPORT" "writeback_change_coverage_percent")")"
total_gap_items="$(to_int_or_zero "$(extract_metric "$TRACKER_REPORT" "total_gap_items")")"
remediation_queue_items_input="$(to_int_or_zero "$(extract_metric "$TRACKER_REPORT" "remediation_queue_items")")"
critical_gap_items="$(to_int_or_zero "$(extract_metric "$TRACKER_REPORT" "critical_gap_items")")"
high_gap_items="$(to_int_or_zero "$(extract_metric "$TRACKER_REPORT" "high_gap_items")")"

versioning_status="$(normalize_status "$(trim "$(extract_metric "$VERSIONING_REPORT" "versioning_status")")")"
rollback_candidates="$(to_int_or_zero "$(extract_metric "$VERSIONING_REPORT" "rollback_candidates")")"
target_version="$(trim "$(extract_metric "$VERSIONING_REPORT" "target_version")")"
rollback_version="$(trim "$(extract_metric "$VERSIONING_REPORT" "rollback_version")")"

response_status="$(normalize_status "$(trim "$(extract_metric "$ANOMALY_RESPONSE_REPORT" "response_status")")")"
critical_high_open="$(to_int_or_zero "$(extract_metric "$ANOMALY_RESPONSE_REPORT" "critical_high_open")")"

sla_breach_status="$(normalize_status "$(trim "$(extract_metric "$SLA_ALERT_REPORT" "sla_breach_status")")")"
total_alert_items="$(to_int_or_zero "$(extract_metric "$SLA_ALERT_REPORT" "total_alert_items")")"
critical_alert_items="$(to_int_or_zero "$(extract_metric "$SLA_ALERT_REPORT" "critical_alert_items")")"
high_alert_items="$(to_int_or_zero "$(extract_metric "$SLA_ALERT_REPORT" "high_alert_items")")"

rows_key_file="$(mktemp)"
rows_sorted_file="$(mktemp)"
rows_render_file="$(mktemp)"
rollback_queue_file="$(mktemp)"
owner_file="$(mktemp)"
trap 'rm -f "$rows_key_file" "$rows_sorted_file" "$rows_render_file" "$rollback_queue_file" "$owner_file"' EXIT

declare -A STEP_PRIORITY
declare -A STEP_OWNER
declare -A STEP_SLA
declare -A STEP_STATUS
declare -A STEP_ACTION
declare -A STEP_PHASE
declare -A STEP_SOURCE
declare -A STEP_EVIDENCE

declare -A OWNER_CRITICAL
declare -A OWNER_HIGH
declare -A OWNER_MEDIUM
declare -A OWNER_TOTAL

add_or_update_step() {
  local item_id="$1"
  local source="$2"
  local priority_raw="$3"
  local owner_raw="$4"
  local sla_raw="$5"
  local status_raw="$6"
  local action="$7"
  local evidence="$8"

  local priority owner sla status phase key
  priority="$(normalize_priority "$priority_raw")"
  owner="$(trim "$owner_raw")"
  [[ -z "$owner" || "$owner" == "n/a" ]] && owner="unassigned"

  sla="$(trim "$sla_raw")"
  [[ -z "$sla" || "$sla" == "n/a" ]] && sla="$(priority_to_sla "$priority")"

  status="$(normalize_status "$status_raw")"
  phase="$(phase_for_item "$item_id" "$action")"

  key="${item_id}|${action}"

  if [[ -z "${STEP_PRIORITY[$key]:-}" ]]; then
    STEP_PRIORITY["$key"]="$priority"
    STEP_OWNER["$key"]="$owner"
    STEP_SLA["$key"]="$sla"
    STEP_STATUS["$key"]="$status"
    STEP_ACTION["$key"]="$action"
    STEP_PHASE["$key"]="$phase"
    STEP_SOURCE["$key"]="$source"
    STEP_EVIDENCE["$key"]="$evidence"
    echo "$key" >> "$rows_key_file"
    return
  fi

  local old_priority old_status
  old_priority="${STEP_PRIORITY[$key]}"
  old_status="${STEP_STATUS[$key]}"

  if (( $(priority_rank "$priority") > $(priority_rank "$old_priority") )) || \
     (( $(priority_rank "$priority") == $(priority_rank "$old_priority") && $(status_rank "$status") > $(status_rank "$old_status") )); then
    STEP_PRIORITY["$key"]="$priority"
    STEP_OWNER["$key"]="$owner"
    STEP_SLA["$key"]="$sla"
    STEP_STATUS["$key"]="$status"
    STEP_ACTION["$key"]="$action"
    STEP_PHASE["$key"]="$phase"
    STEP_SOURCE["$key"]="$source"
    STEP_EVIDENCE["$key"]="$evidence"
  fi
}

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 _ <<< "$row"

  item_id="$(trim "$c1")"
  priority="$(trim "$c2")"
  owner="$(trim "$c3")"
  sla="$(trim "$c4")"
  action="$(trim "$c5")"
  status="$(trim "$c6")"

  if [[ -z "$item_id" || "$item_id" == "none" || "$item_id" == "<ITEM-001>" ]]; then
    continue
  fi

  add_or_update_step "$item_id" "coverage_tracker" "$priority" "$owner" "$sla" "$status" "$action" "source=tracker-remediation-queue"
done < <(extract_section_rows "$TRACKER_REPORT" "5) Remediation Queue")

drill_items_total=0
critical_steps=0
high_steps=0
medium_steps=0
rollback_drill_items=0
estimated_total_minutes=0
owner_hotspots=0

while IFS= read -r key; do
  [[ -z "$key" ]] && continue
  priority="${STEP_PRIORITY[$key]}"
  source="${STEP_SOURCE[$key]}"

  rank="$(priority_rank "$priority")"
  echo "$rank|$source|$key" >> "$rows_sorted_file"
done < "$rows_key_file"

sort -t'|' -k1,1nr -k2,2 -k3,3 "$rows_sorted_file" > "$rows_render_file"

step_no=0
while IFS='|' read -r _rank _source key; do
  [[ -z "$key" ]] && continue

  item_id="${key%%|*}"
  action="${STEP_ACTION[$key]}"
  priority="${STEP_PRIORITY[$key]}"
  owner="${STEP_OWNER[$key]}"
  sla="${STEP_SLA[$key]}"
  status="${STEP_STATUS[$key]}"
  phase="${STEP_PHASE[$key]}"
  evidence="${STEP_EVIDENCE[$key]}"

  step_no=$((step_no + 1))
  step_id="DRL-$(printf '%03d' "$step_no")"
  target_status="$(target_status_for_phase "$phase")"
  estimated_minutes="$(priority_to_minutes "$priority")"
  estimated_total_minutes=$((estimated_total_minutes + estimated_minutes))

  drill_items_total=$((drill_items_total + 1))
  case "$priority" in
    critical) critical_steps=$((critical_steps + 1)) ;;
    high) high_steps=$((high_steps + 1)) ;;
    *) medium_steps=$((medium_steps + 1)) ;;
  esac

  if [[ "$phase" == "rollback-execution" ]]; then
    rollback_drill_items=$((rollback_drill_items + 1))
    echo "$step_id|$priority|$owner|confirm-change-freeze-and-payload-snapshot|$action|rerun-versioning-and-sla-alert-checks|planned" >> "$rollback_queue_file"
  fi

  OWNER_TOTAL["$owner"]=$(( ${OWNER_TOTAL["$owner"]:-0} + 1 ))
  case "$priority" in
    critical) OWNER_CRITICAL["$owner"]=$(( ${OWNER_CRITICAL["$owner"]:-0} + 1 )) ;;
    high) OWNER_HIGH["$owner"]=$(( ${OWNER_HIGH["$owner"]:-0} + 1 )) ;;
    *) OWNER_MEDIUM["$owner"]=$(( ${OWNER_MEDIUM["$owner"]:-0} + 1 )) ;;
  esac

  echo "$step_id|$phase|$priority|$owner|$sla|$estimated_minutes|status=${status}|$action|status->${target_status}|planned|$evidence" >> "$owner_file"
done < "$rows_render_file"

workload_file="$(mktemp)"
trap 'rm -f "$rows_key_file" "$rows_sorted_file" "$rows_render_file" "$rollback_queue_file" "$owner_file" "$workload_file"' EXIT

for owner in "${!OWNER_TOTAL[@]}"; do
  critical_count="${OWNER_CRITICAL[$owner]:-0}"
  high_count="${OWNER_HIGH[$owner]:-0}"
  medium_count="${OWNER_MEDIUM[$owner]:-0}"
  total_count="${OWNER_TOTAL[$owner]:-0}"

  recommended_window="next-weekly"
  if (( critical_count > 0 )); then
    recommended_window="<1h"
  elif (( high_count > 0 )); then
    recommended_window="4h"
  elif (( medium_count > 0 )); then
    recommended_window="1bd"
  fi

  if (( critical_count + high_count >= 3 )); then
    owner_hotspots=$((owner_hotspots + 1))
  fi

  echo "$owner|$critical_count|$high_count|$medium_count|$total_count|$recommended_window" >> "$workload_file"
done

if (( drill_items_total == 0 )); then
  owner_hotspots=0
fi

drill_status="pass"
release_advice="proceed-with-standard-drill-rhythm"

if (( drill_items_total == 0 )); then
  drill_status="warn"
  release_advice="insufficient-remediation-items-for-drill"
fi

if [[ "$tracker_status" == "fail" || "$versioning_status" == "fail" || "$response_status" == "fail" || "$sla_breach_status" == "fail" ]]; then
  drill_status="fail"
  release_advice="block-release-and-run-linkage-rollback-war-room"
elif (( critical_steps > 0 || critical_alert_items > 0 )); then
  drill_status="fail"
  release_advice="block-release-until-critical-drill-items-closed"
elif (( high_steps > 5 || high_alert_items > 5 )); then
  drill_status="warn"
  release_advice="proceed-with-rollback-watch-and-daily-drill"
fi

if (( coverage_percent >= 80 && critical_steps == 0 && critical_alert_items == 0 )) && [[ "$drill_status" != "fail" ]]; then
  drill_status="pass"
  release_advice="coverage-ready-for-controlled-release-drill"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Linkage & Rollback Drill Plan（Draft）

## 1) Metadata

| field | value |
|------|-------|
| drill_id | $DRILL_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| tracker_report | $TRACKER_REPORT |
| versioning_report | $VERSIONING_REPORT |
| anomaly_response_report | $ANOMALY_RESPONSE_REPORT |
| sla_alert_report | $SLA_ALERT_REPORT |
| target_version | ${target_version:-n/a} |
| rollback_version | ${rollback_version:-n/a} |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| tracker_status | $tracker_status |
| writeback_change_coverage_percent | ${coverage_percent}% |
| total_gap_items | $total_gap_items |
| remediation_queue_items_input | $remediation_queue_items_input |
| critical_gap_items | $critical_gap_items |
| high_gap_items | $high_gap_items |
| versioning_status | $versioning_status |
| rollback_candidates | $rollback_candidates |
| anomaly_response_status | $response_status |
| critical_high_open | $critical_high_open |
| sla_breach_status | $sla_breach_status |
| total_alert_items | $total_alert_items |
| critical_alert_items | $critical_alert_items |
| high_alert_items | $high_alert_items |

## 3) Drill Summary

| metric | value |
|--------|-------|
| drill_items_total | $drill_items_total |
| rollback_drill_items | $rollback_drill_items |
| critical_steps | $critical_steps |
| high_steps | $high_steps |
| medium_steps | $medium_steps |
| owner_hotspots | $owner_hotspots |
| estimated_total_minutes | $estimated_total_minutes |
| drill_status | $drill_status |
| release_advice | $release_advice |

## 4) Drill Steps

| step_id | phase | priority | owner | target_sla | estimated_minutes | trigger | action | expected_result | status | evidence |
|---------|-------|----------|-------|------------|-------------------|---------|--------|-----------------|--------|----------|
EOF_REPORT

if [[ -s "$owner_file" ]]; then
  while IFS='|' read -r step_id phase priority owner target_sla estimated_minutes trigger action expected_result status evidence; do
    echo "| $step_id | $phase | $priority | $owner | $target_sla | $estimated_minutes | $trigger | $action | $expected_result | $status | $evidence |" >> "$OUTPUT_FILE"
  done < "$owner_file"
else
  echo "| DRL-000 | cross-team-followup | medium | unassigned | 1bd | 0 | status=unknown | no-action | status->closed | planned | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Rollback Exercise Queue

| step_id | priority | owner | precheck | rollback_action | verify_action | status |
|---------|----------|-------|----------|-----------------|---------------|--------|
EOF_APPEND

if [[ -s "$rollback_queue_file" ]]; then
  while IFS='|' read -r step_id priority owner precheck rollback_action verify_action status; do
    echo "| $step_id | $priority | $owner | $precheck | $rollback_action | $verify_action | $status |" >> "$OUTPUT_FILE"
  done < "$rollback_queue_file"
else
  echo "| none | n/a | unassigned | n/a | no-rollback-action | n/a | planned |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 6) Owner Workload

| owner | critical_items | high_items | medium_items | total_items | recommended_window |
|-------|----------------|------------|--------------|-------------|--------------------|
EOF_APPEND

if [[ -s "$workload_file" ]]; then
  while IFS='|' read -r owner critical_items high_items medium_items total_items recommended_window; do
    echo "| $owner | $critical_items | $high_items | $medium_items | $total_items | $recommended_window |" >> "$OUTPUT_FILE"
  done < "$workload_file"
else
  echo "| unassigned | 0 | 0 | 0 | 0 | next-weekly |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 7) Suggested Actions

- immediate:
  - $release_advice
- followup:
  - rerun-linkage-rollback-drill-after-remediation-closure
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$drill_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass drill status: $drill_status" >&2
  exit 1
fi

echo "[PASS] linkage rollback drill plan generated"
