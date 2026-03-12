#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

POLICY_ID=""
CONVERGENCE_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_MULTIWEEK_RISK_CONVERGENCE_DASHBOARD_SAMPLE_B41.md"
LINKAGE_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_RETEST_APPROVAL_WRITEBACK_LINKAGE_SAMPLE_B44.md"
BASE_DUE_SOON_WARN_THRESHOLD=1
BASE_BLOCKING_HIGH_THRESHOLD=3
BASE_CHECKLIST_WARN_THRESHOLD=1
BASE_TREND_ALERT_THRESHOLD=1
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计收敛看板阈值自适应策略生成脚本（Draft）

用途：
  基于风险收敛看板与重测-签批-回写联动结果，输出下一轮阈值自适应建议。

用法：
  scripts/generate_archive_audit_convergence_adaptive_threshold_policy_draft.sh [options]

选项：
  --policy-id ID                       策略 ID（默认: yyyyMMdd_HHmmss）
  --convergence-report FILE            收敛看板报告（默认: B41 样例）
  --linkage-report FILE                联动一致性报告（默认: B44 样例）
  --base-due-soon-warn-threshold N     base due_soon 告警阈值（默认: 1）
  --base-blocking-high-threshold N     base blocking high 阈值（默认: 3）
  --base-checklist-warn-threshold N    base checklist warn 阈值（默认: 1）
  --base-trend-alert-threshold N       base trend alert 阈值（默认: 1）
  --operator NAME                      操作人/作业名（默认: codex）
  --output FILE                        输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_CONVERGENCE_ADAPTIVE_THRESHOLD_POLICY_<id>.md）
  --strict                             adaptive_status 非 pass 时返回非 0
  --dry-run                            仅打印计划，不写文件
  --help                               显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --policy-id)
      POLICY_ID="$2"
      shift 2
      ;;
    --convergence-report)
      CONVERGENCE_REPORT="$2"
      shift 2
      ;;
    --linkage-report)
      LINKAGE_REPORT="$2"
      shift 2
      ;;
    --base-due-soon-warn-threshold)
      BASE_DUE_SOON_WARN_THRESHOLD="$2"
      shift 2
      ;;
    --base-blocking-high-threshold)
      BASE_BLOCKING_HIGH_THRESHOLD="$2"
      shift 2
      ;;
    --base-checklist-warn-threshold)
      BASE_CHECKLIST_WARN_THRESHOLD="$2"
      shift 2
      ;;
    --base-trend-alert-threshold)
      BASE_TREND_ALERT_THRESHOLD="$2"
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
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_CONVERGENCE_ADAPTIVE_THRESHOLD_POLICY_${POLICY_ID}.md"
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

CONVERGENCE_REPORT="$(resolve_input_path "$CONVERGENCE_REPORT")"
LINKAGE_REPORT="$(resolve_input_path "$LINKAGE_REPORT")"
OUTPUT_FILE="$(resolve_output_path "$OUTPUT_FILE")"

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] policy_id=$POLICY_ID"
  echo "[DRY-RUN] convergence_report=$CONVERGENCE_REPORT"
  echo "[DRY-RUN] linkage_report=$LINKAGE_REPORT"
  echo "[DRY-RUN] base_due_soon_warn_threshold=$BASE_DUE_SOON_WARN_THRESHOLD"
  echo "[DRY-RUN] base_blocking_high_threshold=$BASE_BLOCKING_HIGH_THRESHOLD"
  echo "[DRY-RUN] base_checklist_warn_threshold=$BASE_CHECKLIST_WARN_THRESHOLD"
  echo "[DRY-RUN] base_trend_alert_threshold=$BASE_TREND_ALERT_THRESHOLD"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$CONVERGENCE_REPORT" "$LINKAGE_REPORT"; do
  if [[ ! -f "$file" ]]; then
    echo "[FAIL] input file not found: $file" >&2
    exit 1
  fi
done

for value in "$BASE_DUE_SOON_WARN_THRESHOLD" "$BASE_BLOCKING_HIGH_THRESHOLD" "$BASE_CHECKLIST_WARN_THRESHOLD" "$BASE_TREND_ALERT_THRESHOLD"; do
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
  value="${value//%/}"
  if [[ "$value" =~ ^[0-9]+$ ]]; then
    echo "$value"
  else
    echo 0
  fi
}

normalize_status() {
  case "$1" in
    pass|warn|fail|unknown) echo "$1" ;;
    *) echo "unknown" ;;
  esac
}

max_int() {
  if (( $1 > $2 )); then
    echo "$1"
  else
    echo "$2"
  fi
}

risk_convergence_status="$(normalize_status "$(trim "$(extract_metric "$CONVERGENCE_REPORT" "risk_convergence_status")")")"
convergence_index="$(to_int_or_zero "$(extract_metric "$CONVERGENCE_REPORT" "convergence_index")")"
trend_alerts="$(to_int_or_zero "$(extract_metric "$CONVERGENCE_REPORT" "trend_alerts")")"

linkage_status="$(normalize_status "$(trim "$(extract_metric "$LINKAGE_REPORT" "linkage_status")")")"
mismatch_rows="$(to_int_or_zero "$(extract_metric "$LINKAGE_REPORT" "mismatch_rows")")"
missing_payload_rows="$(to_int_or_zero "$(extract_metric "$LINKAGE_REPORT" "missing_payload_rows")")"
writeback_signaled_items="$(to_int_or_zero "$(extract_metric "$LINKAGE_REPORT" "writeback_signaled_items")")"
writeback_changed_items="$(to_int_or_zero "$(extract_metric "$LINKAGE_REPORT" "writeback_changed_items")")"
gate_alignment_status="$(normalize_status "$(trim "$(extract_metric "$LINKAGE_REPORT" "gate_alignment_status")")")"

pressure_score=0

case "$risk_convergence_status" in
  fail) pressure_score=$((pressure_score + 2)) ;;
  warn) pressure_score=$((pressure_score + 1)) ;;
esac

case "$linkage_status" in
  fail) pressure_score=$((pressure_score + 2)) ;;
  warn) pressure_score=$((pressure_score + 1)) ;;
esac

if [[ "$gate_alignment_status" == "fail" || "$gate_alignment_status" == "warn" ]]; then
  pressure_score=$((pressure_score + 1))
fi

if (( trend_alerts > 0 )); then
  pressure_score=$((pressure_score + 1))
fi

if (( mismatch_rows > 0 || missing_payload_rows > 0 )); then
  pressure_score=$((pressure_score + 1))
fi

if (( writeback_signaled_items > 0 && writeback_changed_items == 0 )); then
  pressure_score=$((pressure_score + 1))
fi

if (( convergence_index < 50 )); then
  pressure_score=$((pressure_score + 1))
elif (( convergence_index >= 85 )) && [[ "$risk_convergence_status" == "pass" && "$linkage_status" == "pass" ]]; then
  pressure_score=$((pressure_score - 1))
fi

if (( pressure_score < 0 )); then
  pressure_score=0
fi

adaptation_mode="hold"
adaptation_reason="stable-risk-window"

if (( pressure_score >= 5 )); then
  adaptation_mode="tighten"
  adaptation_reason="high-pressure-from-linkage-and-convergence"
elif (( pressure_score >= 3 )); then
  adaptation_mode="reinforce"
  adaptation_reason="non-pass-signals-require-tight-monitoring"
elif (( pressure_score == 0 && convergence_index >= 85 )); then
  adaptation_mode="relax"
  adaptation_reason="stable-convergence-and-clean-linkage"
fi

next_due_soon_warn_threshold="$BASE_DUE_SOON_WARN_THRESHOLD"
next_blocking_high_threshold="$BASE_BLOCKING_HIGH_THRESHOLD"
next_checklist_warn_threshold="$BASE_CHECKLIST_WARN_THRESHOLD"
next_trend_alert_threshold="$BASE_TREND_ALERT_THRESHOLD"

if [[ "$adaptation_mode" == "tighten" ]]; then
  next_due_soon_warn_threshold="$(max_int 0 $((BASE_DUE_SOON_WARN_THRESHOLD - 1)))"
  next_blocking_high_threshold="$(max_int 1 $((BASE_BLOCKING_HIGH_THRESHOLD - 1)))"
  next_checklist_warn_threshold="$(max_int 0 $((BASE_CHECKLIST_WARN_THRESHOLD - 1)))"
  next_trend_alert_threshold="$(max_int 0 $((BASE_TREND_ALERT_THRESHOLD - 1)))"
elif [[ "$adaptation_mode" == "reinforce" ]]; then
  next_trend_alert_threshold="$(max_int 0 $((BASE_TREND_ALERT_THRESHOLD - 1)))"
elif [[ "$adaptation_mode" == "relax" ]]; then
  next_due_soon_warn_threshold=$((BASE_DUE_SOON_WARN_THRESHOLD + 1))
  next_blocking_high_threshold=$((BASE_BLOCKING_HIGH_THRESHOLD + 1))
  next_checklist_warn_threshold=$((BASE_CHECKLIST_WARN_THRESHOLD + 1))
  next_trend_alert_threshold=$((BASE_TREND_ALERT_THRESHOLD + 1))
fi

adaptive_status="pass"
release_guidance="proceed-with-adaptive-policy"

if [[ "$adaptation_mode" == "tighten" ]]; then
  adaptive_status="fail"
  release_guidance="block-release-and-apply-tightened-thresholds"
elif [[ "$adaptation_mode" == "reinforce" || "$risk_convergence_status" != "pass" || "$linkage_status" != "pass" ]]; then
  adaptive_status="warn"
  release_guidance="apply-policy-with-manual-review"
fi

if (( mismatch_rows > 0 || missing_payload_rows > 0 )); then
  adaptive_status="fail"
  release_guidance="block-release-until-linkage-payload-consistent"
fi

if (( writeback_signaled_items > 0 && writeback_changed_items == 0 )); then
  adaptive_status="fail"
  release_guidance="block-release-until-writeback-change-coverage-increases"
fi

convergence_gate_result="review"
if [[ "$risk_convergence_status" == "pass" ]] && (( convergence_index >= 85 )); then
  convergence_gate_result="pass"
fi

linkage_payload_result="fail"
if (( mismatch_rows == 0 && missing_payload_rows == 0 )); then
  linkage_payload_result="pass"
fi

writeback_coverage_result="fail"
if (( writeback_signaled_items == 0 || writeback_changed_items > 0 )); then
  writeback_coverage_result="pass"
fi

trend_alert_result="stable"
if (( trend_alerts > 0 )); then
  trend_alert_result="reinforce"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Convergence Adaptive Threshold Policy（Draft）

## 1) Metadata

| field | value |
|------|-------|
| policy_id | $POLICY_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| convergence_report | $CONVERGENCE_REPORT |
| linkage_report | $LINKAGE_REPORT |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| risk_convergence_status | $risk_convergence_status |
| convergence_index | ${convergence_index}% |
| trend_alerts | $trend_alerts |
| linkage_status | $linkage_status |
| gate_alignment_status | $gate_alignment_status |
| mismatch_rows | $mismatch_rows |
| missing_payload_rows | $missing_payload_rows |
| writeback_signaled_items | $writeback_signaled_items |
| writeback_changed_items | $writeback_changed_items |

## 3) Adaptation Summary

| metric | value |
|--------|-------|
| pressure_score | $pressure_score |
| adaptation_mode | $adaptation_mode |
| adaptation_reason | $adaptation_reason |
| adaptive_status | $adaptive_status |
| release_guidance | $release_guidance |

## 4) Threshold Recommendation

| threshold | base_value | recommended_value | delta |
|-----------|------------|-------------------|-------|
| due_soon_warn_threshold | $BASE_DUE_SOON_WARN_THRESHOLD | $next_due_soon_warn_threshold | $((next_due_soon_warn_threshold - BASE_DUE_SOON_WARN_THRESHOLD)) |
| blocking_high_threshold | $BASE_BLOCKING_HIGH_THRESHOLD | $next_blocking_high_threshold | $((next_blocking_high_threshold - BASE_BLOCKING_HIGH_THRESHOLD)) |
| checklist_warn_threshold | $BASE_CHECKLIST_WARN_THRESHOLD | $next_checklist_warn_threshold | $((next_checklist_warn_threshold - BASE_CHECKLIST_WARN_THRESHOLD)) |
| trend_alert_threshold | $BASE_TREND_ALERT_THRESHOLD | $next_trend_alert_threshold | $((next_trend_alert_threshold - BASE_TREND_ALERT_THRESHOLD)) |

## 5) Decision Queue

| check_id | observed | rule | result |
|----------|----------|------|--------|
| convergence-status | $risk_convergence_status/${convergence_index}% | pass 且指数>=85 才可 relax | $convergence_gate_result |
| linkage-payload | mismatch=$mismatch_rows, missing=$missing_payload_rows | mismatch/missing 必须为 0 | $linkage_payload_result |
| writeback-change-coverage | signaled=$writeback_signaled_items, changed=$writeback_changed_items | signaled>0 时 changed 必须>0 | $writeback_coverage_result |
| trend-alert-load | trend_alerts=$trend_alerts | alert>0 时至少 reinforce | $trend_alert_result |

## 6) Suggested Actions

- immediate:
  - $release_guidance
- followup:
  - regenerate-backtest-after-threshold-policy-apply
EOF_REPORT

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$adaptive_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass adaptive status: $adaptive_status" >&2
  exit 1
fi

echo "[PASS] convergence adaptive threshold policy generated"
