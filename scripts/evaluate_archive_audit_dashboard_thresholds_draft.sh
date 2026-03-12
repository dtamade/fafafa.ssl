#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

POLICY_ID=""
DASHBOARD_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md"
DUE_SOON_WARN_THRESHOLD=1
BLOCKING_HIGH_THRESHOLD=3
CHECKLIST_WARN_THRESHOLD=1
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计状态看板阈值与升级策略评估脚本（Draft）

用途：
  对状态看板关键指标应用阈值规则，输出升级等级、责任人、SLA 与动作建议。

用法：
  scripts/evaluate_archive_audit_dashboard_thresholds_draft.sh [options]

选项：
  --policy-id ID                  策略评估 ID（默认: yyyyMMdd_HHmmss）
  --dashboard FILE                状态看板报告（默认: docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md）
  --due-soon-warn-threshold N     due_soon 告警阈值（默认: 1）
  --blocking-high-threshold N     blocking_reasons 触发 high 阈值（默认: 3）
  --checklist-warn-threshold N    checklist warn 触发阈值（默认: 1）
  --operator NAME                 操作人/作业名（默认: codex）
  --output FILE                   输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_DASHBOARD_THRESHOLD_POLICY_<id>.md）
  --strict                        escalation_level 非 low 时返回非 0
  --dry-run                       仅打印计划，不写文件
  --help                          显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --policy-id)
      POLICY_ID="$2"
      shift 2
      ;;
    --dashboard)
      DASHBOARD_FILE="$2"
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

if [[ -z "$POLICY_ID" ]]; then
  POLICY_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_DASHBOARD_THRESHOLD_POLICY_${POLICY_ID}.md"
fi

resolve_input_path() {
  local path="$1"
  if [[ "$path" == /* ]]; then
    echo "$path"
  elif [[ -e "$path" ]]; then
    echo "$path"
  else
    echo "$PROJECT_ROOT/$path"
  fi
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
OUTPUT_FILE="$(resolve_output_path "$OUTPUT_FILE")"

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] policy_id=$POLICY_ID"
  echo "[DRY-RUN] dashboard=$DASHBOARD_FILE"
  echo "[DRY-RUN] due_soon_warn_threshold=$DUE_SOON_WARN_THRESHOLD"
  echo "[DRY-RUN] blocking_high_threshold=$BLOCKING_HIGH_THRESHOLD"
  echo "[DRY-RUN] checklist_warn_threshold=$CHECKLIST_WARN_THRESHOLD"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

if [[ ! -f "$DASHBOARD_FILE" ]]; then
  echo "[FAIL] dashboard file not found: $DASHBOARD_FILE" >&2
  exit 1
fi

for value in "$DUE_SOON_WARN_THRESHOLD" "$BLOCKING_HIGH_THRESHOLD" "$CHECKLIST_WARN_THRESHOLD"; do
  if ! [[ "$value" =~ ^[0-9]+$ ]]; then
    echo "[FAIL] threshold should be non-negative integer: $value" >&2
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

severity_rank() {
  case "$1" in
    critical) echo 4 ;;
    high) echo 3 ;;
    medium) echo 2 ;;
    low) echo 1 ;;
    *) echo 0 ;;
  esac
}

severity_to_owner() {
  case "$1" in
    critical) echo "release-manager+secops" ;;
    high) echo "release-manager" ;;
    medium) echo "qa-owner" ;;
    low) echo "audit-owner" ;;
    *) echo "audit-owner" ;;
  esac
}

severity_to_sla() {
  case "$1" in
    critical) echo "<1h" ;;
    high) echo "4h" ;;
    medium) echo "1bd" ;;
    low) echo "next-weekly" ;;
    *) echo "next-weekly" ;;
  esac
}

severity_to_action() {
  case "$1" in
    critical) echo "immediate-escalation-and-release-block" ;;
    high) echo "open-incident-and-clear-before-cut" ;;
    medium) echo "track-and-mitigate-with-owner" ;;
    low) echo "monitor-only" ;;
    *) echo "monitor-only" ;;
  esac
}

dashboard_status="$(trim "$(extract_metric "$DASHBOARD_FILE" "dashboard_status")")"
hold_overdue_total="$(to_int_or_zero "$(extract_metric "$DASHBOARD_FILE" "hold_overdue_total")")"
hold_due_soon_total="$(to_int_or_zero "$(extract_metric "$DASHBOARD_FILE" "hold_due_soon_total")")"
hold_missing_invalid_total="$(to_int_or_zero "$(extract_metric "$DASHBOARD_FILE" "hold_missing_or_invalid_expiry_total")")"
linkage_risk_total="$(to_int_or_zero "$(extract_metric "$DASHBOARD_FILE" "linkage_risk_total")")"
checklist_fail_total="$(to_int_or_zero "$(extract_metric "$DASHBOARD_FILE" "checklist_readiness_fail")")"
checklist_warn_total="$(to_int_or_zero "$(extract_metric "$DASHBOARD_FILE" "checklist_readiness_warn_or_unknown")")"
weekly_fail_count="$(to_int_or_zero "$(extract_metric "$DASHBOARD_FILE" "weekly_fail_count")")"
weekly_warn_count="$(to_int_or_zero "$(extract_metric "$DASHBOARD_FILE" "weekly_warn_or_unknown_count")")"
blocking_reason_total="$(to_int_or_zero "$(extract_metric "$DASHBOARD_FILE" "blocking_reason_total")")"

[[ -z "$dashboard_status" ]] && dashboard_status="unknown"

eval_rows_file="$(mktemp)"
trap 'rm -f "$eval_rows_file"' EXIT

current_level="low"

evaluate_metric() {
  local metric="$1"
  local value="$2"
  local critical_condition="$3"
  local high_condition="$4"
  local medium_condition="$5"
  local threshold_desc="$6"

  local severity="low"

  if eval "$critical_condition"; then
    severity="critical"
  elif eval "$high_condition"; then
    severity="high"
  elif eval "$medium_condition"; then
    severity="medium"
  fi

  local owner
  local sla
  local action
  owner="$(severity_to_owner "$severity")"
  sla="$(severity_to_sla "$severity")"
  action="$(severity_to_action "$severity")"

  echo "$metric|$value|$threshold_desc|$severity|$owner|$sla|$action" >> "$eval_rows_file"

  if (( $(severity_rank "$severity") > $(severity_rank "$current_level") )); then
    current_level="$severity"
  fi
}

evaluate_metric "dashboard_status" "$dashboard_status" \
  "[[ \"$dashboard_status\" == \"fail\" || \"$dashboard_status\" == \"unknown\" ]]" \
  "[[ \"$dashboard_status\" == \"warn\" ]]" \
  "false" \
  "fail/unknown=>critical; warn=>high"

evaluate_metric "hold_overdue_total" "$hold_overdue_total" \
  "(( hold_overdue_total > 0 ))" \
  "false" \
  "false" \
  ">0=>critical"

evaluate_metric "hold_missing_or_invalid_expiry_total" "$hold_missing_invalid_total" \
  "(( hold_missing_invalid_total > 0 ))" \
  "false" \
  "false" \
  ">0=>critical"

evaluate_metric "linkage_risk_total" "$linkage_risk_total" \
  "(( linkage_risk_total > 0 ))" \
  "false" \
  "false" \
  ">0=>critical"

evaluate_metric "checklist_readiness_fail" "$checklist_fail_total" \
  "(( checklist_fail_total > 0 ))" \
  "false" \
  "false" \
  ">0=>critical"

evaluate_metric "weekly_fail_count" "$weekly_fail_count" \
  "(( weekly_fail_count > 0 ))" \
  "false" \
  "false" \
  ">0=>critical"

evaluate_metric "blocking_reason_total" "$blocking_reason_total" \
  "(( blocking_reason_total >= BLOCKING_HIGH_THRESHOLD && BLOCKING_HIGH_THRESHOLD == 0 ))" \
  "(( blocking_reason_total >= BLOCKING_HIGH_THRESHOLD && BLOCKING_HIGH_THRESHOLD > 0 ))" \
  "(( blocking_reason_total > 0 && blocking_reason_total < BLOCKING_HIGH_THRESHOLD ))" \
  "<${BLOCKING_HIGH_THRESHOLD}=>medium; >=${BLOCKING_HIGH_THRESHOLD}=>high"

evaluate_metric "hold_due_soon_total" "$hold_due_soon_total" \
  "false" \
  "(( hold_due_soon_total >= DUE_SOON_WARN_THRESHOLD && DUE_SOON_WARN_THRESHOLD == 0 ))" \
  "(( hold_due_soon_total >= DUE_SOON_WARN_THRESHOLD && DUE_SOON_WARN_THRESHOLD > 0 ))" \
  ">=${DUE_SOON_WARN_THRESHOLD}=>medium"

evaluate_metric "checklist_readiness_warn_or_unknown" "$checklist_warn_total" \
  "false" \
  "(( checklist_warn_total >= CHECKLIST_WARN_THRESHOLD && CHECKLIST_WARN_THRESHOLD == 0 ))" \
  "(( checklist_warn_total >= CHECKLIST_WARN_THRESHOLD && CHECKLIST_WARN_THRESHOLD > 0 ))" \
  ">=${CHECKLIST_WARN_THRESHOLD}=>medium"

evaluate_metric "weekly_warn_or_unknown_count" "$weekly_warn_count" \
  "false" \
  "false" \
  "(( weekly_warn_count > 0 ))" \
  ">0=>medium"

escalation_level="$current_level"

decision_status="pass"
release_policy="proceed"

case "$escalation_level" in
  critical)
    decision_status="fail"
    release_policy="block-release"
    ;;
  high)
    decision_status="fail"
    release_policy="hold-until-risk-reduced"
    ;;
  medium)
    decision_status="warn"
    release_policy="proceed-with-mitigation"
    ;;
  low)
    decision_status="pass"
    release_policy="proceed"
    ;;
esac

critical_count=0
high_count=0
medium_count=0

while IFS='|' read -r _ _ _ severity _ _ _; do
  case "$severity" in
    critical) critical_count=$((critical_count + 1)) ;;
    high) high_count=$((high_count + 1)) ;;
    medium) medium_count=$((medium_count + 1)) ;;
    *) ;;
  esac
done < "$eval_rows_file"

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Dashboard Threshold Policy Report（Draft）

## 1) Metadata

| field | value |
|------|-------|
| policy_id | $POLICY_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| dashboard_report | $DASHBOARD_FILE |
| operator | $OPERATOR |

## 2) Threshold Configuration

| threshold | value |
|-----------|-------|
| due_soon_warn_threshold | $DUE_SOON_WARN_THRESHOLD |
| blocking_high_threshold | $BLOCKING_HIGH_THRESHOLD |
| checklist_warn_threshold | $CHECKLIST_WARN_THRESHOLD |

## 3) Metric Evaluation

| metric | value | threshold_rule | severity | owner | sla | action |
|--------|-------|----------------|----------|-------|-----|--------|
EOF_REPORT

while IFS='|' read -r metric value threshold_rule severity owner sla action; do
  echo "| $metric | $value | $threshold_rule | $severity | $owner | $sla | $action |" >> "$OUTPUT_FILE"
done < "$eval_rows_file"

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 4) Escalation Summary

| metric | value |
|--------|-------|
| escalation_level | $escalation_level |
| decision_status | $decision_status |
| release_policy | $release_policy |
| critical_signals | $critical_count |
| high_signals | $high_count |
| medium_signals | $medium_count |

## 5) Suggested Escalation Runbook

- immediate:
  - $(severity_to_action "$escalation_level")
- followup:
  - align-threshold-policy-with-release-board
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$escalation_level" != "low" ]]; then
  echo "[FAIL] strict mode detected non-low escalation level: $escalation_level" >&2
  exit 1
fi

echo "[PASS] dashboard threshold policy evaluated"
