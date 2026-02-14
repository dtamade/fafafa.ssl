#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

DASHBOARD_ID=""
BACKTEST_GLOB="docs/test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST*.md"
APPROVAL_CHAIN_GLOB="docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN*.md"
RETEST_GATE_GLOB="docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE*.md"
TREND_ALERT_THRESHOLD=1
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计多周趋势风险收敛看板生成脚本（Draft）

用途：
  汇总 backtest/approval/retest 三类报告，生成多周风险收敛看板与趋势信号。

用法：
  scripts/generate_archive_audit_multiweek_risk_convergence_dashboard_draft.sh [options]

选项：
  --dashboard-id ID           看板 ID（默认: yyyyMMdd_HHmmss）
  --backtest-glob GLOB        backtest 报告匹配（默认: docs/test_reports/ARCHIVE_AUDIT_THRESHOLD_POLICY_BACKTEST*.md）
  --approval-chain-glob GLOB  approval chain 报告匹配（默认: docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN*.md）
  --retest-gate-glob GLOB     retest gate 报告匹配（默认: docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE*.md）
  --trend-alert-threshold N   趋势告警阈值（默认: 1）
  --operator NAME             操作人/作业名（默认: codex）
  --output FILE               输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_MULTIWEEK_RISK_CONVERGENCE_DASHBOARD_<id>.md）
  --strict                    risk_convergence_status 非 pass 时返回非 0
  --dry-run                   仅打印计划，不写文件
  --help                      显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --dashboard-id)
      DASHBOARD_ID="$2"
      shift 2
      ;;
    --backtest-glob)
      BACKTEST_GLOB="$2"
      shift 2
      ;;
    --approval-chain-glob)
      APPROVAL_CHAIN_GLOB="$2"
      shift 2
      ;;
    --retest-gate-glob)
      RETEST_GATE_GLOB="$2"
      shift 2
      ;;
    --trend-alert-threshold)
      TREND_ALERT_THRESHOLD="$2"
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

if [[ -z "$DASHBOARD_ID" ]]; then
  DASHBOARD_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_MULTIWEEK_RISK_CONVERGENCE_DASHBOARD_${DASHBOARD_ID}.md"
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
  echo "[DRY-RUN] dashboard_id=$DASHBOARD_ID"
  echo "[DRY-RUN] backtest_glob=$BACKTEST_GLOB"
  echo "[DRY-RUN] approval_chain_glob=$APPROVAL_CHAIN_GLOB"
  echo "[DRY-RUN] retest_gate_glob=$RETEST_GATE_GLOB"
  echo "[DRY-RUN] trend_alert_threshold=$TREND_ALERT_THRESHOLD"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

if ! [[ "$TREND_ALERT_THRESHOLD" =~ ^[0-9]+$ ]]; then
  echo "[FAIL] trend-alert-threshold should be non-negative integer" >&2
  exit 1
fi

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

mapfile -t BACKTEST_FILES < <(collect_files "$BACKTEST_GLOB")
mapfile -t APPROVAL_FILES < <(collect_files "$APPROVAL_CHAIN_GLOB")
mapfile -t RETEST_FILES < <(collect_files "$RETEST_GATE_GLOB")

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

status_score() {
  case "$1" in
    pass) echo 2 ;;
    warn) echo 1 ;;
    fail) echo 0 ;;
    unknown) echo 0 ;;
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

trend_direction() {
  local first="$1"
  local last="$2"

  if (( last < first )); then
    echo "down"
  elif (( last > first )); then
    echo "up"
  else
    echo "flat"
  fi
}

rows_file="$(mktemp)"
signal_rows_file="$(mktemp)"
trap 'rm -f "$rows_file" "$signal_rows_file"' EXIT

total_score=0
max_score=0
trend_alerts=0
insufficient_signals=0

backtest_pass=0
backtest_warn=0
backtest_fail=0
backtest_unknown=0
latest_backtest_status="unknown"
latest_backtest_file="none"
first_critical_runs=0
last_critical_runs=0

approval_pass=0
approval_warn=0
approval_fail=0
approval_unknown=0
latest_approval_status="unknown"
latest_approval_file="none"
first_rejected_stages=0
last_rejected_stages=0

retest_pass=0
retest_warn=0
retest_fail=0
retest_unknown=0
latest_retest_status="unknown"
latest_retest_file="none"
first_retest_failed=0
last_retest_failed=0

for idx in "${!BACKTEST_FILES[@]}"; do
  file="${BACKTEST_FILES[$idx]}"
  abs="$(resolve_report_abs_path "$file")"

  critical_runs="$(to_int_or_zero "$(extract_metric "$abs" "critical_runs")")"
  high_runs="$(to_int_or_zero "$(extract_metric "$abs" "high_runs")")"
  status="$(trim "$(extract_metric "$abs" "backtest_status")")"
  [[ -z "$status" ]] && status="unknown"

  score="$(status_score "$status")"
  total_score=$((total_score + score))
  max_score=$((max_score + 2))

  case "$status" in
    pass) backtest_pass=$((backtest_pass + 1)) ;;
    warn) backtest_warn=$((backtest_warn + 1)) ;;
    fail) backtest_fail=$((backtest_fail + 1)) ;;
    *) backtest_unknown=$((backtest_unknown + 1)) ;;
  esac

  if (( idx == 0 )); then
    first_critical_runs="$critical_runs"
  fi
  last_critical_runs="$critical_runs"
  latest_backtest_status="$status"
  latest_backtest_file="$file"

  echo "backtest|$file|$critical_runs|$high_runs|$status|$score" >> "$rows_file"
done

for idx in "${!APPROVAL_FILES[@]}"; do
  file="${APPROVAL_FILES[$idx]}"
  abs="$(resolve_report_abs_path "$file")"

  rejected_stages="$(to_int_or_zero "$(extract_metric "$abs" "rejected_stages")")"
  conditional_stages="$(to_int_or_zero "$(extract_metric "$abs" "conditional_stages")")"
  status="$(trim "$(extract_metric "$abs" "approval_status")")"
  [[ -z "$status" ]] && status="unknown"

  score="$(status_score "$status")"
  total_score=$((total_score + score))
  max_score=$((max_score + 2))

  case "$status" in
    pass) approval_pass=$((approval_pass + 1)) ;;
    warn) approval_warn=$((approval_warn + 1)) ;;
    fail) approval_fail=$((approval_fail + 1)) ;;
    *) approval_unknown=$((approval_unknown + 1)) ;;
  esac

  if (( idx == 0 )); then
    first_rejected_stages="$rejected_stages"
  fi
  last_rejected_stages="$rejected_stages"
  latest_approval_status="$status"
  latest_approval_file="$file"

  echo "approval_chain|$file|$rejected_stages|$conditional_stages|$status|$score" >> "$rows_file"
done

for idx in "${!RETEST_FILES[@]}"; do
  file="${RETEST_FILES[$idx]}"
  abs="$(resolve_report_abs_path "$file")"

  retest_failed="$(to_int_or_zero "$(extract_metric "$abs" "retest_failed")")"
  open_critical="$(to_int_or_zero "$(extract_metric "$abs" "open_critical_after_retest")")"
  status="$(trim "$(extract_metric "$abs" "regression_gate_status")")"
  [[ -z "$status" ]] && status="unknown"

  score="$(status_score "$status")"
  total_score=$((total_score + score))
  max_score=$((max_score + 2))

  case "$status" in
    pass) retest_pass=$((retest_pass + 1)) ;;
    warn) retest_warn=$((retest_warn + 1)) ;;
    fail) retest_fail=$((retest_fail + 1)) ;;
    *) retest_unknown=$((retest_unknown + 1)) ;;
  esac

  if (( idx == 0 )); then
    first_retest_failed="$retest_failed"
  fi
  last_retest_failed="$retest_failed"
  latest_retest_status="$status"
  latest_retest_file="$file"

  echo "retest_gate|$file|$retest_failed|$open_critical|$status|$score" >> "$rows_file"
done

emit_signal() {
  local metric="$1"
  local first_value="$2"
  local last_value="$3"
  local run_count="$4"

  local diff
  local direction
  local signal_status

  diff="$(abs_diff "$first_value" "$last_value")"
  direction="$(trend_direction "$first_value" "$last_value")"
  signal_status="stable"

  if (( run_count <= 1 )); then
    signal_status="insufficient-window"
    insufficient_signals=$((insufficient_signals + 1))
  elif [[ "$direction" == "up" ]] && (( diff >= TREND_ALERT_THRESHOLD )); then
    signal_status="alert"
    trend_alerts=$((trend_alerts + 1))
  elif [[ "$direction" == "down" ]] && (( diff >= TREND_ALERT_THRESHOLD )); then
    signal_status="improving"
  fi

  echo "$metric|$first_value|$last_value|$diff|$direction|$signal_status" >> "$signal_rows_file"
}

emit_signal "backtest_critical_runs" "$first_critical_runs" "$last_critical_runs" "${#BACKTEST_FILES[@]}"
emit_signal "approval_rejected_stages" "$first_rejected_stages" "$last_rejected_stages" "${#APPROVAL_FILES[@]}"
emit_signal "retest_failed_items" "$first_retest_failed" "$last_retest_failed" "${#RETEST_FILES[@]}"

convergence_index=0
if (( max_score > 0 )); then
  convergence_index=$(( total_score * 100 / max_score ))
fi

risk_convergence_status="pass"
release_guidance="proceed-with-convergence-dashboard"

latest_has_fail=false
latest_has_warn_or_unknown=false
for status in "$latest_backtest_status" "$latest_approval_status" "$latest_retest_status"; do
  if [[ "$status" == "fail" ]]; then
    latest_has_fail=true
  elif [[ "$status" == "warn" || "$status" == "unknown" ]]; then
    latest_has_warn_or_unknown=true
  fi
done

if [[ "$latest_has_fail" == "true" ]] || (( trend_alerts > 0 )); then
  risk_convergence_status="fail"
  release_guidance="block-release-until-risk-converges"
elif [[ "$latest_has_warn_or_unknown" == "true" ]] || (( convergence_index < 80 )) || (( insufficient_signals > 0 )); then
  risk_convergence_status="warn"
  release_guidance="proceed-with-weekly-convergence-tracking"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Multi-Week Risk Convergence Dashboard（Draft）

## 1) Metadata

| field | value |
|------|-------|
| dashboard_id | $DASHBOARD_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| backtest_glob | $BACKTEST_GLOB |
| approval_chain_glob | $APPROVAL_CHAIN_GLOB |
| retest_gate_glob | $RETEST_GATE_GLOB |
| backtest_files | ${#BACKTEST_FILES[@]} |
| approval_chain_files | ${#APPROVAL_FILES[@]} |
| retest_gate_files | ${#RETEST_FILES[@]} |
| operator | $OPERATOR |

## 2) Latest Snapshot

| metric | value |
|--------|-------|
| latest_backtest_status | $latest_backtest_status |
| latest_backtest_file | $latest_backtest_file |
| latest_approval_status | $latest_approval_status |
| latest_approval_file | $latest_approval_file |
| latest_retest_status | $latest_retest_status |
| latest_retest_file | $latest_retest_file |

## 3) Convergence Summary

| metric | value |
|--------|-------|
| total_score | $total_score |
| max_score | $max_score |
| convergence_index | ${convergence_index}% |
| trend_alerts | $trend_alerts |
| insufficient_signals | $insufficient_signals |
| risk_convergence_status | $risk_convergence_status |
| release_guidance | $release_guidance |

## 4) Stream Snapshot

| stream | files | pass | warn | fail | unknown | latest_status |
|--------|-------|------|------|------|---------|---------------|
| backtest | ${#BACKTEST_FILES[@]} | $backtest_pass | $backtest_warn | $backtest_fail | $backtest_unknown | $latest_backtest_status |
| approval_chain | ${#APPROVAL_FILES[@]} | $approval_pass | $approval_warn | $approval_fail | $approval_unknown | $latest_approval_status |
| retest_gate | ${#RETEST_FILES[@]} | $retest_pass | $retest_warn | $retest_fail | $retest_unknown | $latest_retest_status |

## 5) Per-Run Snapshot

| stream | source | primary_metric | secondary_metric | status | score |
|--------|--------|----------------|------------------|--------|-------|
EOF_REPORT

if [[ -s "$rows_file" ]]; then
  while IFS='|' read -r stream source primary_metric secondary_metric status score; do
    echo "| $stream | $source | $primary_metric | $secondary_metric | $status | $score |" >> "$OUTPUT_FILE"
  done < "$rows_file"
else
  echo "| none | none | 0 | 0 | unknown | 0 |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 6) Trend Signals

| metric | first_value | last_value | absolute_diff | trend_direction | signal_status |
|--------|-------------|------------|---------------|-----------------|---------------|
EOF_APPEND

while IFS='|' read -r metric first_value last_value diff direction signal_status; do
  echo "| $metric | $first_value | $last_value | $diff | $direction | $signal_status |" >> "$OUTPUT_FILE"
done < "$signal_rows_file"

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 7) Suggested Actions

- immediate:
  - $release_guidance
- followup:
  - refresh-dashboard-after-next-weekly-cycle
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$risk_convergence_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass risk convergence status: $risk_convergence_status" >&2
  exit 1
fi

echo "[PASS] multi-week risk convergence dashboard generated"
