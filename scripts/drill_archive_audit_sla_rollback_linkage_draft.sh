#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

EXERCISE_ID=""
SLA_ALERT_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md"
ROLLBACK_VERSIONING_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md"
DRILL_PLAN_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_SAMPLE_B50.md"
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计 SLA 与回滚联动演练脚本（Draft）

用途：
  联合 B48 的 SLA 预警与 B46/B50 的回滚演练信息，生成“可执行联动队列 + 升级波次 + 责任人负载”报告。

用法：
  scripts/drill_archive_audit_sla_rollback_linkage_draft.sh [options]

选项：
  --exercise-id ID              演练 ID（默认: yyyyMMdd_HHmmss）
  --sla-alert-report FILE       SLA 预警报告（默认: B48 样例）
  --rollback-report FILE        回滚版本化报告（默认: B46 样例）
  --drill-plan-report FILE      联动回滚演练计划（默认: B50 样例）
  --operator NAME               操作人/作业名（默认: codex）
  --output FILE                 输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_<id>.md）
  --strict                      linkage_status 非 pass 时返回非 0
  --dry-run                     仅打印计划，不写文件
  --help                        显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --exercise-id)
      EXERCISE_ID="$2"
      shift 2
      ;;
    --sla-alert-report)
      SLA_ALERT_REPORT="$2"
      shift 2
      ;;
    --rollback-report)
      ROLLBACK_VERSIONING_REPORT="$2"
      shift 2
      ;;
    --drill-plan-report)
      DRILL_PLAN_REPORT="$2"
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

if [[ -z "$EXERCISE_ID" ]]; then
  EXERCISE_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_${EXERCISE_ID}.md"
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

SLA_ALERT_REPORT="$(resolve_input_path "$SLA_ALERT_REPORT")"
ROLLBACK_VERSIONING_REPORT="$(resolve_input_path "$ROLLBACK_VERSIONING_REPORT")"
DRILL_PLAN_REPORT="$(resolve_input_path "$DRILL_PLAN_REPORT")"
OUTPUT_FILE="$(resolve_output_path "$OUTPUT_FILE")"

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] exercise_id=$EXERCISE_ID"
  echo "[DRY-RUN] sla_alert_report=$SLA_ALERT_REPORT"
  echo "[DRY-RUN] rollback_report=$ROLLBACK_VERSIONING_REPORT"
  echo "[DRY-RUN] drill_plan_report=$DRILL_PLAN_REPORT"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$SLA_ALERT_REPORT" "$ROLLBACK_VERSIONING_REPORT" "$DRILL_PLAN_REPORT"; do
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
    pass|warn|fail|unknown|open|queued|closed|pending|in-progress|planned|done|waived|ok|review|watch|breach-risk-high|breach-risk-medium)
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

sla_to_minutes() {
  case "$1" in
    "<1h") echo 60 ;;
    "4h") echo 240 ;;
    "1bd") echo 480 ;;
    "next-weekly") echo 10080 ;;
    *) echo 720 ;;
  esac
}

priority_to_minutes() {
  case "$1" in
    critical) echo 45 ;;
    high) echo 120 ;;
    medium) echo 240 ;;
    low) echo 360 ;;
    *) echo 240 ;;
  esac
}

alert_level_to_priority() {
  case "$1" in
    breach-risk-high) echo "critical" ;;
    breach-risk-medium) echo "high" ;;
    watch) echo "medium" ;;
    ok) echo "low" ;;
    *) echo "high" ;;
  esac
}

priority_bucket() {
  case "$1" in
    critical) echo 4 ;;
    high) echo 3 ;;
    medium) echo 2 ;;
    low) echo 1 ;;
    *) echo 0 ;;
  esac
}

minutes_to_wave() {
  local minutes="$1"
  if (( minutes <= 60 )); then
    echo "wave-1-immediate"
  elif (( minutes <= 240 )); then
    echo "wave-2-short-window"
  else
    echo "wave-3-watchlist"
  fi
}

alert_rows_file="$(mktemp)"
linkage_rows_file="$(mktemp)"
missing_alert_rows_file="$(mktemp)"
alert_only_rows_file="$(mktemp)"
wave_rows_file="$(mktemp)"
owner_rows_file="$(mktemp)"
trap 'rm -f "$alert_rows_file" "$linkage_rows_file" "$missing_alert_rows_file" "$alert_only_rows_file" "$wave_rows_file" "$owner_rows_file"' EXIT

# Load SLA alert rows
while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 c7 c8 _ <<< "$row"

  alert_id="$(trim "$c1")"
  source="$(trim "$c2")"
  owner="$(trim "$c3")"
  target_sla="$(trim "$c4")"
  target_minutes_raw="$(trim "$c5")"
  observed="$(trim "$c6")"
  alert_level="$(trim "$c7")"
  escalation_action="$(trim "$c8")"

  if [[ -z "$alert_id" || "$alert_id" == "none" || "$alert_id" == "<alert_id>" ]]; then
    continue
  fi

  if [[ ! "$target_minutes_raw" =~ ^[0-9]+$ ]]; then
    target_minutes_raw="$(sla_to_minutes "$target_sla")"
  fi

  echo "$alert_id|$source|$owner|$target_sla|$target_minutes_raw|$observed|$alert_level|$escalation_action" >> "$alert_rows_file"
done < <(extract_section_rows "$SLA_ALERT_REPORT" "4) Alert Rows")

declare -A ALERT_SOURCE
declare -A ALERT_OWNER
declare -A ALERT_SLA
declare -A ALERT_MINUTES
declare -A ALERT_OBSERVED
declare -A ALERT_LEVEL
declare -A ALERT_ACTION

while IFS='|' read -r alert_id source owner target_sla target_minutes observed alert_level escalation_action; do
  [[ -z "$alert_id" ]] && continue
  ALERT_SOURCE["$alert_id"]="$source"
  ALERT_OWNER["$alert_id"]="$owner"
  ALERT_SLA["$alert_id"]="$target_sla"
  ALERT_MINUTES["$alert_id"]="$target_minutes"
  ALERT_OBSERVED["$alert_id"]="$observed"
  ALERT_LEVEL["$alert_id"]="$alert_level"
  ALERT_ACTION["$alert_id"]="$escalation_action"
done < "$alert_rows_file"

# Load rollback verify action hints from B50 drill plan queue
declare -A DRILL_PRECHECK
declare -A DRILL_VERIFY
declare -A DRILL_STATUS

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 c7 _ <<< "$row"
  rollback_action="$(trim "$c5")"
  verify_action="$(trim "$c6")"
  precheck="$(trim "$c4")"
  queue_status="$(trim "$c7")"

  blocker_code="$(echo "$rollback_action" | grep -oE 'BLK-[0-9]+' | head -1 || true)"
  [[ -z "$blocker_code" ]] && continue

  if [[ -z "${DRILL_VERIFY[$blocker_code]:-}" ]]; then
    DRILL_VERIFY["$blocker_code"]="$verify_action"
    DRILL_PRECHECK["$blocker_code"]="$precheck"
    DRILL_STATUS["$blocker_code"]="$(normalize_status "$queue_status")"
  fi
done < <(extract_section_rows "$DRILL_PLAN_REPORT" "5) Rollback Exercise Queue")

versioning_status="$(normalize_status "$(trim "$(extract_metric "$ROLLBACK_VERSIONING_REPORT" "versioning_status")")")"
rollback_candidates="$(to_int_or_zero "$(extract_metric "$ROLLBACK_VERSIONING_REPORT" "rollback_candidates")")"
target_version="$(trim "$(extract_metric "$ROLLBACK_VERSIONING_REPORT" "target_version")")"
rollback_version="$(trim "$(extract_metric "$ROLLBACK_VERSIONING_REPORT" "rollback_version")")"

sla_breach_status="$(normalize_status "$(trim "$(extract_metric "$SLA_ALERT_REPORT" "sla_breach_status")")")"
total_alert_items="$(to_int_or_zero "$(extract_metric "$SLA_ALERT_REPORT" "total_alert_items")")"
critical_alert_items="$(to_int_or_zero "$(extract_metric "$SLA_ALERT_REPORT" "critical_alert_items")")"
high_alert_items="$(to_int_or_zero "$(extract_metric "$SLA_ALERT_REPORT" "high_alert_items")")"

linkage_items_total=0
matched_rollback_alerts=0
missing_alert_mappings=0
alert_without_rollback=0
critical_linkage_items=0
high_linkage_items=0
medium_linkage_items=0
estimated_total_minutes=0

wave1_count=0
wave2_count=0
wave3_count=0

declare -A MATCHED_ALERTS
declare -A OWNER_CRITICAL
declare -A OWNER_HIGH
declare -A OWNER_MEDIUM
declare -A OWNER_TOTAL

bump_owner() {
  local owner="$1"
  local priority="$2"

  OWNER_TOTAL["$owner"]=$(( ${OWNER_TOTAL["$owner"]:-0} + 1 ))
  case "$priority" in
    critical) OWNER_CRITICAL["$owner"]=$(( ${OWNER_CRITICAL["$owner"]:-0} + 1 )) ;;
    high) OWNER_HIGH["$owner"]=$(( ${OWNER_HIGH["$owner"]:-0} + 1 )) ;;
    *) OWNER_MEDIUM["$owner"]=$(( ${OWNER_MEDIUM["$owner"]:-0} + 1 )) ;;
  esac
}

row_no=0
while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 _ <<< "$row"

  blocker_code="$(trim "$c1")"
  current_status_raw="$(trim "$c2")"
  rollback_version_row="$(trim "$c3")"
  rollback_reason="$(trim "$c4")"
  rollback_note="$(trim "$c5")"

  if [[ -z "$blocker_code" || "$blocker_code" == "none" || "$blocker_code" == "<blocker_code>" ]]; then
    continue
  fi

  row_no=$((row_no + 1))
  step_id="SLR-$(printf '%03d' "$row_no")"

  expected_alert_id="RB-${blocker_code}"
  owner="release-manager"
  target_sla="4h"
  target_minutes=240
  alert_level="missing-linkage"
  escalation_action="create-sla-alert-for-${blocker_code}"
  alert_observed="status=unknown"
  alert_source="rollback_queue"
  priority="high"

  if [[ -n "${ALERT_LEVEL[$expected_alert_id]:-}" ]]; then
    matched_rollback_alerts=$((matched_rollback_alerts + 1))
    MATCHED_ALERTS["$expected_alert_id"]=1

    owner="$(trim "${ALERT_OWNER[$expected_alert_id]}")"
    [[ -z "$owner" ]] && owner="release-manager"
    target_sla="$(trim "${ALERT_SLA[$expected_alert_id]}")"
    [[ -z "$target_sla" ]] && target_sla="4h"

    target_minutes="$(to_int_or_zero "${ALERT_MINUTES[$expected_alert_id]}")"
    if (( target_minutes == 0 )); then
      target_minutes="$(sla_to_minutes "$target_sla")"
    fi

    alert_level="$(trim "${ALERT_LEVEL[$expected_alert_id]}")"
    escalation_action="$(trim "${ALERT_ACTION[$expected_alert_id]}")"
    [[ -z "$escalation_action" ]] && escalation_action="execute-rollback-${rollback_version_row:-$rollback_version}-for-${blocker_code}"

    alert_observed="$(trim "${ALERT_OBSERVED[$expected_alert_id]}")"
    alert_source="$(trim "${ALERT_SOURCE[$expected_alert_id]}")"
    priority="$(alert_level_to_priority "$alert_level")"
  else
    missing_alert_mappings=$((missing_alert_mappings + 1))
    echo "$expected_alert_id|$blocker_code|$rollback_version_row|$rollback_reason|$rollback_note" >> "$missing_alert_rows_file"
  fi

  case "$priority" in
    critical) critical_linkage_items=$((critical_linkage_items + 1)) ;;
    high) high_linkage_items=$((high_linkage_items + 1)) ;;
    *) medium_linkage_items=$((medium_linkage_items + 1)) ;;
  esac

  if [[ -z "$rollback_version_row" ]]; then
    rollback_version_row="$rollback_version"
  fi

  precheck="${DRILL_PRECHECK[$blocker_code]:-confirm-change-freeze-and-owner-ack}"
  verify_action="${DRILL_VERIFY[$blocker_code]:-rerun-versioning-and-sla-alert-checks}"

  target_minutes_int="$(to_int_or_zero "$target_minutes")"
  if (( target_minutes_int == 0 )); then
    target_minutes_int="$(priority_to_minutes "$priority")"
  fi

  wave="$(minutes_to_wave "$target_minutes_int")"
  case "$wave" in
    wave-1-immediate) wave1_count=$((wave1_count + 1)) ;;
    wave-2-short-window) wave2_count=$((wave2_count + 1)) ;;
    *) wave3_count=$((wave3_count + 1)) ;;
  esac

  estimated_minutes="$(( target_minutes_int < $(priority_to_minutes "$priority") ? target_minutes_int : $(priority_to_minutes "$priority") ))"
  estimated_total_minutes=$((estimated_total_minutes + estimated_minutes))

  owner="$(trim "$owner")"
  [[ -z "$owner" ]] && owner="unassigned"
  bump_owner "$owner" "$priority"

  linkage_items_total=$((linkage_items_total + 1))

  echo "$step_id|$expected_alert_id|$blocker_code|$priority|$owner|$target_sla|$target_minutes_int|$rollback_version_row|$precheck|$escalation_action|$verify_action|status=$(normalize_status "$current_status_raw"); alert=${alert_level}|planned|alert_source=${alert_source}; observed=${alert_observed}; rollback_reason=${rollback_reason}; note=${rollback_note}" >> "$linkage_rows_file"
  echo "$wave|$step_id|$owner|$target_minutes_int|$priority" >> "$wave_rows_file"
done < <(extract_section_rows "$ROLLBACK_VERSIONING_REPORT" "5) Rollback Queue")

while IFS='|' read -r alert_id source owner target_sla target_minutes observed alert_level escalation_action; do
  [[ -z "$alert_id" ]] && continue
  if [[ "$alert_id" == RB-* && -z "${MATCHED_ALERTS[$alert_id]:-}" ]]; then
    alert_without_rollback=$((alert_without_rollback + 1))
    echo "$alert_id|$owner|$target_sla|$target_minutes|$alert_level|$escalation_action|missing-rollback-queue-item" >> "$alert_only_rows_file"
  fi
done < "$alert_rows_file"

owner_hotspots=0
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

  echo "$owner|$critical_count|$high_count|$medium_count|$total_count|$recommended_window" >> "$owner_rows_file"
done

linkage_status="pass"
release_advice="proceed-with-controlled-sla-rollback-drill"

if (( linkage_items_total == 0 )); then
  linkage_status="warn"
  release_advice="no-rollback-items-detected-check-input-reports"
fi

if [[ "$versioning_status" == "fail" || "$sla_breach_status" == "fail" ]]; then
  linkage_status="fail"
  release_advice="block-release-and-open-sla-rollback-war-room"
fi

if (( missing_alert_mappings > 0 || alert_without_rollback > 0 )); then
  linkage_status="fail"
  release_advice="block-release-until-sla-rollback-mapping-complete"
fi

if [[ "$linkage_status" != "fail" && ( "$critical_linkage_items" -gt 0 ) ]]; then
  linkage_status="warn"
  release_advice="proceed-with-hourly-watch-and-critical-owner-ack"
elif [[ "$linkage_status" != "fail" && ( "$high_linkage_items" -gt 0 ) ]]; then
  linkage_status="warn"
  release_advice="proceed-with-4h-watchlist-and-rerun-gates"
fi

if [[ "$linkage_status" != "fail" && "$versioning_status" == "pass" && "$sla_breach_status" == "pass" && "$critical_linkage_items" -eq 0 && "$high_linkage_items" -eq 0 && "$missing_alert_mappings" -eq 0 && "$alert_without_rollback" -eq 0 && "$linkage_items_total" -gt 0 ]]; then
  linkage_status="pass"
  release_advice="mapping-clean-ready-for-controlled-release"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit SLA-Rollback Linkage Drill（Draft）

## 1) Metadata

| field | value |
|------|-------|
| exercise_id | $EXERCISE_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| sla_alert_report | $SLA_ALERT_REPORT |
| rollback_report | $ROLLBACK_VERSIONING_REPORT |
| drill_plan_report | $DRILL_PLAN_REPORT |
| target_version | ${target_version:-n/a} |
| rollback_version | ${rollback_version:-n/a} |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| versioning_status | $versioning_status |
| rollback_candidates | $rollback_candidates |
| sla_breach_status | $sla_breach_status |
| total_alert_items | $total_alert_items |
| critical_alert_items | $critical_alert_items |
| high_alert_items | $high_alert_items |

## 3) Linkage Summary

| metric | value |
|--------|-------|
| linkage_items_total | $linkage_items_total |
| matched_rollback_alerts | $matched_rollback_alerts |
| missing_alert_mappings | $missing_alert_mappings |
| alert_without_rollback | $alert_without_rollback |
| critical_linkage_items | $critical_linkage_items |
| high_linkage_items | $high_linkage_items |
| medium_linkage_items | $medium_linkage_items |
| wave_1_immediate | $wave1_count |
| wave_2_short_window | $wave2_count |
| wave_3_watchlist | $wave3_count |
| owner_hotspots | $owner_hotspots |
| estimated_total_minutes | $estimated_total_minutes |
| linkage_status | $linkage_status |
| release_advice | $release_advice |

## 4) Linkage Queue

| step_id | alert_id | rollback_item | priority | owner | target_sla | target_minutes | rollback_version | precheck | rollback_action | verify_action | trigger | status | evidence |
|---------|----------|---------------|----------|-------|------------|----------------|------------------|----------|-----------------|---------------|---------|--------|----------|
EOF_REPORT

if [[ -s "$linkage_rows_file" ]]; then
  while IFS='|' read -r step_id alert_id rollback_item priority owner target_sla target_minutes rollback_version_row precheck rollback_action verify_action trigger status evidence; do
    echo "| $step_id | $alert_id | $rollback_item | $priority | $owner | $target_sla | $target_minutes | $rollback_version_row | $precheck | $rollback_action | $verify_action | $trigger | $status | $evidence |" >> "$OUTPUT_FILE"
  done < "$linkage_rows_file"
else
  echo "| SLR-000 | n/a | none | medium | unassigned | 1bd | 480 | n/a | n/a | no-rollback-action | n/a | status=unknown | planned | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Missing SLA Mappings

| expected_alert_id | rollback_item | rollback_version | rollback_reason | note |
|-------------------|---------------|------------------|-----------------|------|
EOF_APPEND

if [[ -s "$missing_alert_rows_file" ]]; then
  while IFS='|' read -r expected_alert_id rollback_item rollback_version_row rollback_reason note; do
    echo "| $expected_alert_id | $rollback_item | $rollback_version_row | $rollback_reason | $note |" >> "$OUTPUT_FILE"
  done < "$missing_alert_rows_file"
else
  echo "| none | n/a | n/a | n/a | all rollback items linked to SLA alerts |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 6) Alert Rows Without Rollback Items

| alert_id | owner | target_sla | target_minutes | alert_level | escalation_action | note |
|----------|-------|------------|----------------|-------------|-------------------|------|
EOF_APPEND

if [[ -s "$alert_only_rows_file" ]]; then
  while IFS='|' read -r alert_id owner target_sla target_minutes alert_level escalation_action note; do
    echo "| $alert_id | $owner | $target_sla | $target_minutes | $alert_level | $escalation_action | $note |" >> "$OUTPUT_FILE"
  done < "$alert_only_rows_file"
else
  echo "| none | n/a | n/a | 0 | ok | n/a | all rollback-related alerts linked |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 7) Escalation Waves

| wave | item_count | target_window |
|------|------------|---------------|
EOF_APPEND

echo "| wave-1-immediate | $wave1_count | <1h |" >> "$OUTPUT_FILE"
echo "| wave-2-short-window | $wave2_count | 4h |" >> "$OUTPUT_FILE"
echo "| wave-3-watchlist | $wave3_count | 1bd+ |" >> "$OUTPUT_FILE"

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 8) Owner Workload

| owner | critical_items | high_items | medium_items | total_items | recommended_window |
|-------|----------------|------------|--------------|-------------|--------------------|
EOF_APPEND

if [[ -s "$owner_rows_file" ]]; then
  while IFS='|' read -r owner critical_items high_items medium_items total_items recommended_window; do
    echo "| $owner | $critical_items | $high_items | $medium_items | $total_items | $recommended_window |" >> "$OUTPUT_FILE"
  done < "$owner_rows_file"
else
  echo "| unassigned | 0 | 0 | 0 | 0 | next-weekly |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 9) Suggested Actions

- immediate:
  - $release_advice
- followup:
  - rerun-sla-rollback-linkage-drill-after-mapping-closure
  - rerun-b48-sla-alert-and-b46-versioning-validation
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$linkage_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass linkage status: $linkage_status" >&2
  exit 1
fi

echo "[PASS] sla rollback linkage drill generated"
