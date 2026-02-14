#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

BACKTEST_ID=""
DASHBOARD_GLOB="docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD*.md"
DUE_SOON_WARN_THRESHOLD=1
BLOCKING_HIGH_THRESHOLD=3
CHECKLIST_WARN_THRESHOLD=1
DRIFT_ALERT_THRESHOLD=1
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计阈值策略回测与漂移监控脚本（Draft）

用途：
  对多份 dashboard 报告执行阈值回测，并输出漂移监控摘要。

用法：
  scripts/backtest_archive_audit_threshold_policy_draft.sh [options]

选项：
  --backtest-id ID               回测 ID（默认: yyyyMMdd_HHmmss）
  --dashboard-glob GLOB          dashboard 匹配（默认: docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD*.md）
  --due-soon-warn-threshold N    due_soon 告警阈值（默认: 1）
  --blocking-high-threshold N    blocking_reasons 高风险阈值（默认: 3）
  --checklist-warn-threshold N   checklist warn 阈值（默认: 1）
  --drift-alert-threshold N      漂移告警阈值（默认: 1）
  --operator NAME                操作人/作业名（默认: codex）
  --output FILE                  输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST_<id>.md）
  --strict                       backtest_status 非 pass 时返回非 0
  --dry-run                      仅打印计划，不写文件
  --help                         显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --backtest-id)
      BACKTEST_ID="$2"
      shift 2
      ;;
    --dashboard-glob)
      DASHBOARD_GLOB="$2"
      shift 2
      ;;
    --due-soon-warn-threshold)
      DUE_SOON_WARN_THRESHOLD="$2"
      shift 2
      ;;
    --blocking-high-threshold)
      BLOCKING_HIGH_THRESHOLD="$2"
      shift 2
      ;;
    --checklist-warn-threshold)
      CHECKLIST_WARN_THRESHOLD="$2"
      shift 2
      ;;
    --drift-alert-threshold)
      DRIFT_ALERT_THRESHOLD="$2"
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

if [[ -z "$BACKTEST_ID" ]]; then
  BACKTEST_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST_${BACKTEST_ID}.md"
fi

resolve_output_path() {
  local path="$1"

  if [[ "$path" == /* ]]; then
    echo "$path"
  else
    echo "$PROJECT_ROOT/$path"
  fi
}

OUTPUT_FILE="$(resolve_output_path "$OUTPUT_FILE")"

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] backtest_id=$BACKTEST_ID"
  echo "[DRY-RUN] dashboard_glob=$DASHBOARD_GLOB"
  echo "[DRY-RUN] due_soon_warn_threshold=$DUE_SOON_WARN_THRESHOLD"
  echo "[DRY-RUN] blocking_high_threshold=$BLOCKING_HIGH_THRESHOLD"
  echo "[DRY-RUN] checklist_warn_threshold=$CHECKLIST_WARN_THRESHOLD"
  echo "[DRY-RUN] drift_alert_threshold=$DRIFT_ALERT_THRESHOLD"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for value in "$DUE_SOON_WARN_THRESHOLD" "$BLOCKING_HIGH_THRESHOLD" "$CHECKLIST_WARN_THRESHOLD" "$DRIFT_ALERT_THRESHOLD"; do
  if ! [[ "$value" =~ ^[0-9]+$ ]]; then
    echo "[FAIL] threshold should be non-negative integer: $value" >&2
    exit 1
  fi
done

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

mapfile -t DASHBOARD_FILES < <(collect_files "$DASHBOARD_GLOB")

resolve_report_abs_path() {
  local path="$1"

  if [[ "$path" == /* ]]; then
    echo "$path"
  else
    echo "$PROJECT_ROOT/$path"
  fi
}

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

level_rank() {
  case "$1" in
    critical) echo 4 ;;
    high) echo 3 ;;
    medium) echo 2 ;;
    low) echo 1 ;;
    *) echo 0 ;;
  esac
}

abs_diff() {
  local a="$1"
  local b="$2"
  if (( a >= b )); then
    echo $((a - b))
  else
    echo $((b - a))
  fi
}

rows_file="$(mktemp)"
drift_rows_file="$(mktemp)"
trap 'rm -f "$rows_file" "$drift_rows_file"' EXIT

critical_runs=0
high_runs=0
medium_runs=0
low_runs=0

total_due_soon=0
total_blocking=0
total_checklist_fail=0

first_file="n/a"
last_file="n/a"
first_due_soon=0
first_blocking=0
first_checklist_fail=0
last_due_soon=0
last_blocking=0
last_checklist_fail=0

for idx in "${!DASHBOARD_FILES[@]}"; do
  file="${DASHBOARD_FILES[$idx]}"
  abs="$(resolve_report_abs_path "$file")"

  hold_overdue_total="$(to_int_or_zero "$(extract_metric "$abs" "hold_overdue_total")")"
  hold_due_soon_total="$(to_int_or_zero "$(extract_metric "$abs" "hold_due_soon_total")")"
  hold_missing_invalid_total="$(to_int_or_zero "$(extract_metric "$abs" "hold_missing_or_invalid_expiry_total")")"
  checklist_fail_total="$(to_int_or_zero "$(extract_metric "$abs" "checklist_readiness_fail")")"
  checklist_warn_total="$(to_int_or_zero "$(extract_metric "$abs" "checklist_readiness_warn_or_unknown")")"
  weekly_fail_count="$(to_int_or_zero "$(extract_metric "$abs" "weekly_fail_count")")"
  blocking_reason_total="$(to_int_or_zero "$(extract_metric "$abs" "blocking_reason_total")")"
  linkage_risk_total="$(to_int_or_zero "$(extract_metric "$abs" "linkage_risk_total")")"

  level="low"
  if (( hold_overdue_total > 0 || hold_missing_invalid_total > 0 || checklist_fail_total > 0 || weekly_fail_count > 0 || linkage_risk_total > 0 )); then
    level="critical"
  elif (( blocking_reason_total >= BLOCKING_HIGH_THRESHOLD && BLOCKING_HIGH_THRESHOLD > 0 )); then
    level="high"
  elif (( hold_due_soon_total >= DUE_SOON_WARN_THRESHOLD && DUE_SOON_WARN_THRESHOLD > 0 )) || (( checklist_warn_total >= CHECKLIST_WARN_THRESHOLD && CHECKLIST_WARN_THRESHOLD > 0 )); then
    level="medium"
  fi

  case "$level" in
    critical) critical_runs=$((critical_runs + 1)) ;;
    high) high_runs=$((high_runs + 1)) ;;
    medium) medium_runs=$((medium_runs + 1)) ;;
    low) low_runs=$((low_runs + 1)) ;;
    *) ;;
  esac

  total_due_soon=$((total_due_soon + hold_due_soon_total))
  total_blocking=$((total_blocking + blocking_reason_total))
  total_checklist_fail=$((total_checklist_fail + checklist_fail_total))

  if (( idx == 0 )); then
    first_file="$file"
    first_due_soon="$hold_due_soon_total"
    first_blocking="$blocking_reason_total"
    first_checklist_fail="$checklist_fail_total"
  fi

  last_file="$file"
  last_due_soon="$hold_due_soon_total"
  last_blocking="$blocking_reason_total"
  last_checklist_fail="$checklist_fail_total"

  echo "$file|$level|$hold_overdue_total|$hold_due_soon_total|$hold_missing_invalid_total|$checklist_fail_total|$checklist_warn_total|$weekly_fail_count|$linkage_risk_total|$blocking_reason_total" >> "$rows_file"
done

total_runs=${#DASHBOARD_FILES[@]}

avg_due_soon=0
avg_blocking=0
avg_checklist_fail=0
if (( total_runs > 0 )); then
  avg_due_soon=$((total_due_soon / total_runs))
  avg_blocking=$((total_blocking / total_runs))
  avg_checklist_fail=$((total_checklist_fail / total_runs))
fi

drift_due_soon=0
drift_blocking=0
drift_checklist_fail=0

if (( total_runs > 1 )); then
  drift_due_soon="$(abs_diff "$first_due_soon" "$last_due_soon")"
  drift_blocking="$(abs_diff "$first_blocking" "$last_blocking")"
  drift_checklist_fail="$(abs_diff "$first_checklist_fail" "$last_checklist_fail")"
fi

drift_alerts=0

if (( drift_due_soon >= DRIFT_ALERT_THRESHOLD && total_runs > 1 )); then
  drift_alerts=$((drift_alerts + 1))
  echo "hold_due_soon_total|$first_due_soon|$last_due_soon|$drift_due_soon|alert" >> "$drift_rows_file"
else
  echo "hold_due_soon_total|$first_due_soon|$last_due_soon|$drift_due_soon|ok" >> "$drift_rows_file"
fi

if (( drift_blocking >= DRIFT_ALERT_THRESHOLD && total_runs > 1 )); then
  drift_alerts=$((drift_alerts + 1))
  echo "blocking_reason_total|$first_blocking|$last_blocking|$drift_blocking|alert" >> "$drift_rows_file"
else
  echo "blocking_reason_total|$first_blocking|$last_blocking|$drift_blocking|ok" >> "$drift_rows_file"
fi

if (( drift_checklist_fail >= DRIFT_ALERT_THRESHOLD && total_runs > 1 )); then
  drift_alerts=$((drift_alerts + 1))
  echo "checklist_readiness_fail|$first_checklist_fail|$last_checklist_fail|$drift_checklist_fail|alert" >> "$drift_rows_file"
else
  echo "checklist_readiness_fail|$first_checklist_fail|$last_checklist_fail|$drift_checklist_fail|ok" >> "$drift_rows_file"
fi

backtest_status="pass"
release_guidance="proceed"

if (( critical_runs > 0 || high_runs > 0 )); then
  backtest_status="fail"
  release_guidance="block-policy-rollout-until-high-critical-cleared"
elif (( medium_runs > 0 || drift_alerts > 0 )); then
  backtest_status="warn"
  release_guidance="proceed-with-monitoring-and-tuning"
fi

if (( total_runs == 0 )); then
  backtest_status="warn"
  release_guidance="insufficient-inputs"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Threshold Policy Backtest & Drift Monitor（Draft）

## 1) Metadata

| field | value |
|------|-------|
| backtest_id | $BACKTEST_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| dashboard_glob | $DASHBOARD_GLOB |
| total_runs | $total_runs |
| operator | $OPERATOR |

## 2) Threshold Configuration

| threshold | value |
|-----------|-------|
| due_soon_warn_threshold | $DUE_SOON_WARN_THRESHOLD |
| blocking_high_threshold | $BLOCKING_HIGH_THRESHOLD |
| checklist_warn_threshold | $CHECKLIST_WARN_THRESHOLD |
| drift_alert_threshold | $DRIFT_ALERT_THRESHOLD |

## 3) Backtest Summary

| metric | value |
|--------|-------|
| critical_runs | $critical_runs |
| high_runs | $high_runs |
| medium_runs | $medium_runs |
| low_runs | $low_runs |
| avg_hold_due_soon_total | $avg_due_soon |
| avg_blocking_reason_total | $avg_blocking |
| avg_checklist_readiness_fail | $avg_checklist_fail |
| drift_alerts | $drift_alerts |
| backtest_status | $backtest_status |
| release_guidance | $release_guidance |

## 4) Per-Run Evaluation

| source | escalation_level | hold_overdue_total | hold_due_soon_total | hold_missing_or_invalid_expiry_total | checklist_fail | checklist_warn | weekly_fail_count | linkage_risk_total | blocking_reason_total |
|--------|------------------|--------------------|---------------------|--------------------------------------|----------------|----------------|-------------------|--------------------|-----------------------|
EOF_REPORT

if [[ -s "$rows_file" ]]; then
  while IFS='|' read -r source level hold_overdue due_soon missing_invalid checklist_fail checklist_warn weekly_fail linkage_risk blocking_total; do
    echo "| $source | $level | $hold_overdue | $due_soon | $missing_invalid | $checklist_fail | $checklist_warn | $weekly_fail | $linkage_risk | $blocking_total |" >> "$OUTPUT_FILE"
  done < "$rows_file"
else
  echo "| n/a | n/a | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 5) Drift Monitor

| metric | first_run | last_run | absolute_diff | drift_status |
|--------|-----------|----------|---------------|--------------|
EOF_APPEND

while IFS='|' read -r metric first_run last_run diff status; do
  echo "| $metric | $first_run | $last_run | $diff | $status |" >> "$OUTPUT_FILE"
done < "$drift_rows_file"

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 6) Suggested Actions

- immediate:
  - $release_guidance
- followup:
  - rerun-backtest-after-threshold-adjustment
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$backtest_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass backtest status: $backtest_status" >&2
  exit 1
fi

echo "[PASS] threshold policy backtest completed"
