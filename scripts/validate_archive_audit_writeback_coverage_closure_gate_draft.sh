#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

GATE_ID=""
TRACKER_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CHANGE_COVERAGE_REMEDIATION_TRACKER_SAMPLE_B49.md"
SLA_ROLLBACK_LINKAGE_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_SAMPLE_B52.md"
VERSIONING_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md"
MIN_COVERAGE=100
MAX_HIGH_GAP=0
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计回写覆盖率修复闭环验收门禁脚本（Draft）

用途：
  聚合 B49 回写覆盖率追踪、B52 SLA/回滚联动结果与 B46 版本化回滚状态，
  输出闭环验收门禁报告（acceptance gate）。

用法：
  scripts/validate_archive_audit_writeback_coverage_closure_gate_draft.sh [options]

选项：
  --gate-id ID                  门禁 ID（默认: yyyyMMdd_HHmmss）
  --tracker-report FILE         回写覆盖率追踪报告（默认: B49 样例）
  --sla-rollback-report FILE    SLA/回滚联动报告（默认: B52 样例）
  --versioning-report FILE      版本化回滚报告（默认: B46 样例）
  --min-coverage N              最低覆盖率阈值（默认: 100）
  --max-high-gap N              high 缺口可容忍上限（默认: 0）
  --operator NAME               操作人/作业名（默认: codex）
  --output FILE                 输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_<id>.md）
  --strict                      acceptance_status 非 pass 时返回非 0
  --dry-run                     仅打印计划，不写文件
  --help                        显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --gate-id)
      GATE_ID="$2"
      shift 2
      ;;
    --tracker-report)
      TRACKER_REPORT="$2"
      shift 2
      ;;
    --sla-rollback-report)
      SLA_ROLLBACK_LINKAGE_REPORT="$2"
      shift 2
      ;;
    --versioning-report)
      VERSIONING_REPORT="$2"
      shift 2
      ;;
    --min-coverage)
      MIN_COVERAGE="$2"
      shift 2
      ;;
    --max-high-gap)
      MAX_HIGH_GAP="$2"
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

if [[ -z "$GATE_ID" ]]; then
  GATE_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_${GATE_ID}.md"
fi

if ! [[ "$MIN_COVERAGE" =~ ^[0-9]+$ ]]; then
  echo "[FAIL] --min-coverage must be integer" >&2
  exit 1
fi

if ! [[ "$MAX_HIGH_GAP" =~ ^[0-9]+$ ]]; then
  echo "[FAIL] --max-high-gap must be integer" >&2
  exit 1
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] gate_id=$GATE_ID"
  echo "[DRY-RUN] tracker_report=$TRACKER_REPORT"
  echo "[DRY-RUN] sla_rollback_report=$SLA_ROLLBACK_LINKAGE_REPORT"
  echo "[DRY-RUN] versioning_report=$VERSIONING_REPORT"
  echo "[DRY-RUN] min_coverage=$MIN_COVERAGE"
  echo "[DRY-RUN] max_high_gap=$MAX_HIGH_GAP"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$TRACKER_REPORT" "$SLA_ROLLBACK_LINKAGE_REPORT" "$VERSIONING_REPORT"; do
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
    pass|warn|fail|unknown|open|queued|closed|pending|in-progress|done|waived|ok|review|watch|breach-risk-high|breach-risk-medium)
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

# Load key metrics
tracker_status="$(normalize_status "$(trim "$(extract_metric "$TRACKER_REPORT" "tracker_status")")")"
coverage_percent="$(to_int_or_zero "$(extract_metric "$TRACKER_REPORT" "writeback_change_coverage_percent")")"
total_gap_items="$(to_int_or_zero "$(extract_metric "$TRACKER_REPORT" "total_gap_items")")"
critical_gap_items="$(to_int_or_zero "$(extract_metric "$TRACKER_REPORT" "critical_gap_items")")"
high_gap_items="$(to_int_or_zero "$(extract_metric "$TRACKER_REPORT" "high_gap_items")")"
remediation_queue_items="$(to_int_or_zero "$(extract_metric "$TRACKER_REPORT" "remediation_queue_items")")"
tracker_release_advice="$(trim "$(extract_metric "$TRACKER_REPORT" "release_advice")")"

linkage_status="$(normalize_status "$(trim "$(extract_metric "$SLA_ROLLBACK_LINKAGE_REPORT" "linkage_status")")")"
missing_alert_mappings="$(to_int_or_zero "$(extract_metric "$SLA_ROLLBACK_LINKAGE_REPORT" "missing_alert_mappings")")"
alert_without_rollback="$(to_int_or_zero "$(extract_metric "$SLA_ROLLBACK_LINKAGE_REPORT" "alert_without_rollback")")"
critical_linkage_items="$(to_int_or_zero "$(extract_metric "$SLA_ROLLBACK_LINKAGE_REPORT" "critical_linkage_items")")"
high_linkage_items="$(to_int_or_zero "$(extract_metric "$SLA_ROLLBACK_LINKAGE_REPORT" "high_linkage_items")")"
linkage_items_total="$(to_int_or_zero "$(extract_metric "$SLA_ROLLBACK_LINKAGE_REPORT" "linkage_items_total")")"
linkage_release_advice="$(trim "$(extract_metric "$SLA_ROLLBACK_LINKAGE_REPORT" "release_advice")")"

versioning_status="$(normalize_status "$(trim "$(extract_metric "$VERSIONING_REPORT" "versioning_status")")")"
rollback_candidates="$(to_int_or_zero "$(extract_metric "$VERSIONING_REPORT" "rollback_candidates")")"
target_version="$(trim "$(extract_metric "$VERSIONING_REPORT" "target_version")")"
rollback_version="$(trim "$(extract_metric "$VERSIONING_REPORT" "rollback_version")")"

checks_file="$(mktemp)"
blockers_file="$(mktemp)"
owner_file="$(mktemp)"
trap 'rm -f "$checks_file" "$blockers_file" "$owner_file"' EXIT

checks_total=0
checks_passed=0
checks_failed=0
checks_warn=0
critical_failed_checks=0
high_failed_checks=0

add_check() {
  local check_id="$1"
  local severity="$2"
  local observed="$3"
  local threshold="$4"
  local result="$5"
  local remediation="$6"

  checks_total=$((checks_total + 1))
  case "$result" in
    pass) checks_passed=$((checks_passed + 1)) ;;
    warn) checks_warn=$((checks_warn + 1)) ;;
    *)
      checks_failed=$((checks_failed + 1))
      if [[ "$severity" == "critical" ]]; then
        critical_failed_checks=$((critical_failed_checks + 1))
      elif [[ "$severity" == "high" ]]; then
        high_failed_checks=$((high_failed_checks + 1))
      fi
      ;;
  esac

  echo "$check_id|$severity|$observed|$threshold|$result|$remediation" >> "$checks_file"
}

# Gate checks
if [[ "$tracker_status" == "pass" ]]; then
  add_check "gate-tracker-status" "critical" "$tracker_status" "pass" "pass" "keep-tracker-status-green"
else
  add_check "gate-tracker-status" "critical" "$tracker_status" "pass" "fail" "rerun-b49-remediation-until-tracker-pass"
fi

if (( coverage_percent >= MIN_COVERAGE )); then
  add_check "gate-coverage-percent" "critical" "${coverage_percent}%" ">=${MIN_COVERAGE}%" "pass" "keep-writeback-coverage-above-threshold"
else
  add_check "gate-coverage-percent" "critical" "${coverage_percent}%" ">=${MIN_COVERAGE}%" "fail" "execute-writeback-remediation-and-rerun-b49"
fi

if (( total_gap_items == 0 )); then
  add_check "gate-total-gap-items" "critical" "$total_gap_items" "=0" "pass" "maintain-zero-gap"
else
  add_check "gate-total-gap-items" "critical" "$total_gap_items" "=0" "fail" "close-all-gap-items-in-remediation-queue"
fi

if (( critical_gap_items == 0 )); then
  add_check "gate-critical-gap-items" "critical" "$critical_gap_items" "=0" "pass" "maintain-no-critical-gap"
else
  add_check "gate-critical-gap-items" "critical" "$critical_gap_items" "=0" "fail" "close-critical-gap-items-before-release"
fi

if (( high_gap_items <= MAX_HIGH_GAP )); then
  add_check "gate-high-gap-items" "high" "$high_gap_items" "<=${MAX_HIGH_GAP}" "pass" "keep-high-gap-under-threshold"
else
  add_check "gate-high-gap-items" "high" "$high_gap_items" "<=${MAX_HIGH_GAP}" "fail" "reduce-high-gap-items-and-rerun-gate"
fi

if [[ "$versioning_status" == "pass" ]]; then
  add_check "gate-versioning-status" "critical" "$versioning_status" "pass" "pass" "maintain-versioning-pass"
else
  add_check "gate-versioning-status" "critical" "$versioning_status" "pass" "fail" "resolve-b46-versioning-status-before-closure"
fi

if (( rollback_candidates == 0 )); then
  add_check "gate-rollback-candidates" "critical" "$rollback_candidates" "=0" "pass" "keep-rollback-candidates-empty"
else
  add_check "gate-rollback-candidates" "critical" "$rollback_candidates" "=0" "fail" "drain-rollback-queue-and-rerun-b46"
fi

if [[ "$linkage_status" == "pass" ]]; then
  add_check "gate-linkage-status" "critical" "$linkage_status" "pass" "pass" "maintain-b52-linkage-pass"
else
  add_check "gate-linkage-status" "critical" "$linkage_status" "pass" "fail" "resolve-b52-linkage-failures"
fi

if (( missing_alert_mappings == 0 )); then
  add_check "gate-missing-alert-mappings" "critical" "$missing_alert_mappings" "=0" "pass" "keep-alert-mappings-complete"
else
  add_check "gate-missing-alert-mappings" "critical" "$missing_alert_mappings" "=0" "fail" "map-rollback-items-to-sla-alerts"
fi

if (( alert_without_rollback == 0 )); then
  add_check "gate-alert-without-rollback" "high" "$alert_without_rollback" "=0" "pass" "keep-alerts-bound-to-rollback-items"
else
  add_check "gate-alert-without-rollback" "high" "$alert_without_rollback" "=0" "fail" "align-sla-alerts-with-rollback-queue"
fi

if (( linkage_items_total == rollback_candidates )); then
  add_check "gate-linkage-volume-alignment" "medium" "linkage=${linkage_items_total}, rollback=${rollback_candidates}" "equal" "pass" "keep-linkage-and-versioning-volume-aligned"
else
  add_check "gate-linkage-volume-alignment" "medium" "linkage=${linkage_items_total}, rollback=${rollback_candidates}" "equal" "warn" "investigate-count-drift-between-b52-and-b46"
fi

# Build blocker snapshot from B49 remediation queue
blockers_total=0
while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 _ <<< "$row"

  item_id="$(trim "$c1")"
  priority="$(trim "$c2")"
  owner="$(trim "$c3")"
  sla="$(trim "$c4")"
  immediate_action="$(trim "$c5")"
  status_raw="$(trim "$c6")"
  status_norm="$(normalize_status "$status_raw")"

  if [[ -z "$item_id" || "$item_id" == "none" || "$item_id" == "<item_id>" ]]; then
    continue
  fi

  if [[ "$status_norm" == "done" || "$status_norm" == "closed" || "$status_norm" == "pass" || "$status_norm" == "ok" || "$status_norm" == "waived" ]]; then
    continue
  fi

  blockers_total=$((blockers_total + 1))
  echo "$item_id|$priority|$owner|$sla|$status_raw|$immediate_action" >> "$blockers_file"

done < <(extract_section_rows "$TRACKER_REPORT" "5) Remediation Queue")

# Owner summary from blockers
if [[ -s "$blockers_file" ]]; then
  awk -F'|' '
    {
      owner=$3
      priority=$2
      total[owner]++
      if (priority=="critical") critical[owner]++
      else if (priority=="high") high[owner]++
      else medium[owner]++
    }
    END {
      for (o in total) {
        c = (o in critical ? critical[o] : 0)
        h = (o in high ? high[o] : 0)
        m = (o in medium ? medium[o] : 0)
        window="1bd"
        if (c > 0) window="<1h"
        else if (h > 0) window="4h"
        print o "|" c "|" h "|" m "|" total[o] "|" window
      }
    }
  ' "$blockers_file" > "$owner_file"
fi

owner_hotspots=0
if [[ -s "$owner_file" ]]; then
  while IFS='|' read -r owner c h m total window; do
    if (( c + h >= 3 )); then
      owner_hotspots=$((owner_hotspots + 1))
    fi
  done < "$owner_file"
fi

acceptance_status="pass"
release_advice="closure-gate-pass-ready-for-controlled-release"

if (( checks_failed > 0 )); then
  acceptance_status="fail"
  release_advice="block-release-and-close-writeback-coverage-loop"
elif (( checks_warn > 0 )); then
  acceptance_status="warn"
  release_advice="proceed-with-watch-and-rerun-closure-gate"
fi

if [[ "$acceptance_status" != "fail" && "$tracker_status" == "pass" && "$linkage_status" == "pass" && "$versioning_status" == "pass" && "$coverage_percent" -ge "$MIN_COVERAGE" && "$total_gap_items" -eq 0 && "$critical_gap_items" -eq 0 && "$high_gap_items" -le "$MAX_HIGH_GAP" && "$rollback_candidates" -eq 0 && "$missing_alert_mappings" -eq 0 && "$alert_without_rollback" -eq 0 ]]; then
  acceptance_status="pass"
  release_advice="all-closure-gates-green"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Writeback Coverage Closure Acceptance Gate（Draft）

## 1) Metadata

| field | value |
|------|-------|
| gate_id | $GATE_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| tracker_report | $TRACKER_REPORT |
| sla_rollback_report | $SLA_ROLLBACK_LINKAGE_REPORT |
| versioning_report | $VERSIONING_REPORT |
| min_coverage | ${MIN_COVERAGE}% |
| max_high_gap | $MAX_HIGH_GAP |
| target_version | ${target_version:-n/a} |
| rollback_version | ${rollback_version:-n/a} |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| tracker_status | $tracker_status |
| writeback_change_coverage_percent | ${coverage_percent}% |
| total_gap_items | $total_gap_items |
| critical_gap_items | $critical_gap_items |
| high_gap_items | $high_gap_items |
| remediation_queue_items | $remediation_queue_items |
| versioning_status | $versioning_status |
| rollback_candidates | $rollback_candidates |
| linkage_status | $linkage_status |
| linkage_items_total | $linkage_items_total |
| missing_alert_mappings | $missing_alert_mappings |
| alert_without_rollback | $alert_without_rollback |
| critical_linkage_items | $critical_linkage_items |
| high_linkage_items | $high_linkage_items |

## 3) Acceptance Summary

| metric | value |
|--------|-------|
| checks_total | $checks_total |
| checks_passed | $checks_passed |
| checks_warn | $checks_warn |
| checks_failed | $checks_failed |
| critical_failed_checks | $critical_failed_checks |
| high_failed_checks | $high_failed_checks |
| outstanding_blockers | $blockers_total |
| owner_hotspots | $owner_hotspots |
| acceptance_status | $acceptance_status |
| release_advice | $release_advice |

## 4) Gate Checks

| check_id | severity | observed | threshold | result | remediation_action |
|----------|----------|----------|-----------|--------|--------------------|
EOF_REPORT

if [[ -s "$checks_file" ]]; then
  while IFS='|' read -r check_id severity observed threshold result remediation_action; do
    echo "| $check_id | $severity | $observed | $threshold | $result | $remediation_action |" >> "$OUTPUT_FILE"
  done < "$checks_file"
else
  echo "| gate-not-initialized | medium | n/a | n/a | fail | initialize-checks |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Outstanding Blockers

| item_id | priority | owner | sla | status | immediate_action |
|---------|----------|-------|-----|--------|------------------|
EOF_APPEND

if [[ -s "$blockers_file" ]]; then
  while IFS='|' read -r item_id priority owner sla status immediate_action; do
    echo "| $item_id | $priority | $owner | $sla | $status | $immediate_action |" >> "$OUTPUT_FILE"
  done < "$blockers_file"
else
  echo "| none | n/a | unassigned | n/a | closed | no-action |" >> "$OUTPUT_FILE"
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
  - ${tracker_release_advice:-rerun-b49-remediation-tracker}
  - ${linkage_release_advice:-rerun-b52-sla-rollback-linkage}
  - rerun-closure-acceptance-gate-after-action-closure
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$acceptance_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass acceptance status: $acceptance_status" >&2
  exit 1
fi

echo "[PASS] writeback coverage closure acceptance gate generated"
