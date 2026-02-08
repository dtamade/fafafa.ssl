#!/usr/bin/env bash
# review_closure_gate_weekly_trend_drift_draft.sh
# B57: 闭环门禁周趋势与漂移复核草案
# 分析闭环门禁的周趋势，检测漂移并生成复核报告

set -euo pipefail

# ============================================================
# 参数解析
# ============================================================
REVIEW_ID=""
OUTPUT=""
GATE_REPORT_GLOB=""
WEEKS=4
DRIFT_THRESHOLD=10
TREND_DIRECTION=""
STRICT=false

usage() {
  cat <<EOF
Usage: $0 [OPTIONS]

Options:
  --gate-report-glob PATTERN   闭环门禁报告 glob 模式（必需）
  --review-id ID               复核批次 ID（必需）
  --output FILE                输出报告路径（可选）
  --weeks N                    分析周数（默认 4）
  --drift-threshold N          漂移阈值百分比（默认 10）
  --strict                     严格模式：检测到漂移则 exit 1
  -h, --help                   显示帮助

Examples:
  $0 --gate-report-glob "docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_*.md" \\
     --review-id b57_sample_20260207_2100 \\
     --output docs/test_reports/ARCHIVE_AUDIT_CLOSURE_TREND_SAMPLE_B57.md
  $0 --gate-report-glob "..." --strict
EOF
  exit 0
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --gate-report-glob) GATE_REPORT_GLOB="$2"; shift 2 ;;
    --review-id) REVIEW_ID="$2"; shift 2 ;;
    --output) OUTPUT="$2"; shift 2 ;;
    --weeks) WEEKS="$2"; shift 2 ;;
    --drift-threshold) DRIFT_THRESHOLD="$2"; shift 2 ;;
    --strict) STRICT=true; shift ;;
    -h|--help) usage ;;
    *) echo "Unknown option: $1"; usage ;;
  esac
done

if [[ -z "$REVIEW_ID" ]]; then
  echo "Error: --review-id is required"
  exit 1
fi

if [[ -z "$GATE_REPORT_GLOB" ]]; then
  echo "Error: --gate-report-glob is required"
  exit 1
fi

TIMESTAMP=$(date +%Y-%m-%d\ %H:%M:%S\ %z)

# ============================================================
# 数据提取函数
# ============================================================
extract_field() {
  local file="$1"
  local field="$2"
  if [[ -f "$file" ]]; then
    grep -E "^\| *${field} *\|" "$file" 2>/dev/null | head -1 | sed -E 's/.*\| *[^|]+ *\| *([^|]+) *\|.*/\1/' | xargs
  else
    echo "n/a"
  fi
}

extract_numeric() {
  local value="$1"
  echo "$value" | grep -oE '[0-9]+' | head -1 || echo "0"
}

# ============================================================
# 趋势分析
# ============================================================
analyze_trend() {
  local reports=()
  local total_items=()
  local fail_items=()
  local pass_rates=()

  # 收集报告
  for file in $GATE_REPORT_GLOB; do
    [[ -f "$file" ]] && reports+=("$file")
  done

  # 按时间排序（假设文件名包含时间戳）
  IFS=$'\n' sorted_reports=($(printf '%s\n' "${reports[@]}" | sort))
  unset IFS

  # 提取指标
  for report in "${sorted_reports[@]:-}"; do
    local total fail
    total=$(extract_field "$report" "total_checks")
    fail=$(extract_field "$report" "checks_failed")

    total=$(extract_numeric "$total")
    fail=$(extract_numeric "$fail")

    total_items+=("$total")
    fail_items+=("$fail")

    if [[ $total -gt 0 ]]; then
      local pass_rate=$(( (total - fail) * 100 / total ))
      pass_rates+=("$pass_rate")
    else
      pass_rates+=("0")
    fi
  done

  # 计算趋势
  local count=${#pass_rates[@]}
  local trend="stable"
  local drift=0

  if [[ $count -ge 2 ]]; then
    local first=${pass_rates[0]}
    local last=${pass_rates[$((count-1))]}

    if [[ $last -gt $first ]]; then
      trend="improving"
      drift=$((last - first))
    elif [[ $last -lt $first ]]; then
      trend="degrading"
      drift=$((first - last))
    fi
  fi

  # 输出分析结果
  echo "reports_count|$count"
  echo "trend|$trend"
  echo "drift_percent|$drift"
  echo "first_pass_rate|${pass_rates[0]:-0}"
  echo "last_pass_rate|${pass_rates[$((count-1))]:-0}"

  # 输出每周数据
  local week=1
  for i in "${!sorted_reports[@]}"; do
    echo "week_${week}_report|${sorted_reports[$i]}"
    echo "week_${week}_total|${total_items[$i]}"
    echo "week_${week}_fail|${fail_items[$i]}"
    echo "week_${week}_pass_rate|${pass_rates[$i]}"
    week=$((week + 1))
  done
}

# ============================================================
# 漂移检测
# ============================================================
detect_drift() {
  local drift="$1"
  local threshold="$2"

  if [[ $drift -ge $threshold ]]; then
    echo "detected"
  else
    echo "none"
  fi
}

# ============================================================
# 报告生成
# ============================================================
generate_report() {
  local analysis="$1"

  # 解析分析结果
  local reports_count trend drift_percent first_rate last_rate
  reports_count=$(echo "$analysis" | grep "^reports_count|" | cut -d'|' -f2)
  trend=$(echo "$analysis" | grep "^trend|" | cut -d'|' -f2)
  drift_percent=$(echo "$analysis" | grep "^drift_percent|" | cut -d'|' -f2)
  first_rate=$(echo "$analysis" | grep "^first_pass_rate|" | cut -d'|' -f2)
  last_rate=$(echo "$analysis" | grep "^last_pass_rate|" | cut -d'|' -f2)

  local drift_status
  drift_status=$(detect_drift "$drift_percent" "$DRIFT_THRESHOLD")

  local review_status="pass"
  if [[ "$drift_status" == "detected" && "$trend" == "degrading" ]]; then
    review_status="fail"
  elif [[ "$drift_status" == "detected" ]]; then
    review_status="warn"
  fi

  cat <<EOF
# Archive Audit Closure Gate Weekly Trend Review

## Metadata

| Field | Value |
|-------|-------|
| review_id | $REVIEW_ID |
| generated_at | $TIMESTAMP |
| gate_report_glob | $GATE_REPORT_GLOB |
| analysis_weeks | $WEEKS |
| drift_threshold | ${DRIFT_THRESHOLD}% |

## Summary

| Metric | Value |
|--------|-------|
| reports_analyzed | $reports_count |
| trend_direction | $trend |
| drift_percent | ${drift_percent}% |
| first_pass_rate | ${first_rate}% |
| last_pass_rate | ${last_rate}% |
| drift_status | $drift_status |
| review_status | $review_status |

## Weekly Trend Data

| week | report | total_checks | fail_checks | pass_rate |
|------|--------|--------------|-------------|-----------|
EOF

  # 输出每周数据
  local week=1
  while true; do
    local report total fail rate
    report=$(echo "$analysis" | grep "^week_${week}_report|" | cut -d'|' -f2)
    [[ -z "$report" ]] && break

    total=$(echo "$analysis" | grep "^week_${week}_total|" | cut -d'|' -f2)
    fail=$(echo "$analysis" | grep "^week_${week}_fail|" | cut -d'|' -f2)
    rate=$(echo "$analysis" | grep "^week_${week}_pass_rate|" | cut -d'|' -f2)

    local report_name
    report_name=$(basename "$report")
    echo "| $week | $report_name | $total | $fail | ${rate}% |"

    week=$((week + 1))
  done

  if [[ $reports_count -eq 0 ]]; then
    echo "| (none) | - | - | - | - |"
  fi

  cat <<EOF

## Drift Analysis

| Check | Status | Detail |
|-------|--------|--------|
| trend_direction | $trend | $(if [[ "$trend" == "improving" ]]; then echo "positive"; elif [[ "$trend" == "degrading" ]]; then echo "negative"; else echo "neutral"; fi) |
| drift_magnitude | ${drift_percent}% | threshold=${DRIFT_THRESHOLD}% |
| drift_detected | $drift_status | $(if [[ "$drift_status" == "detected" ]]; then echo "action-required"; else echo "within-tolerance"; fi) |

## Recommendations

EOF

  if [[ "$review_status" == "pass" ]]; then
    echo "- Trend is stable or improving."
    echo "- No immediate action required."
    echo "- Continue monitoring weekly."
  elif [[ "$review_status" == "warn" ]]; then
    echo "- Drift detected but trend is not degrading."
    echo "- Review recent changes for potential causes."
    echo "- Consider preventive measures."
  else
    echo "- Degrading trend with significant drift detected."
    echo "- Immediate investigation required."
    echo "- Review failed checks and address root causes."
  fi

  cat <<EOF

## Release Advice

| Condition | Advice |
|-----------|--------|
| review_status=pass | proceed-with-monitoring |
| review_status=warn | review-before-release |
| review_status=fail | block-until-trend-reverses |
EOF
}

# ============================================================
# 主流程
# ============================================================
main() {
  # 执行趋势分析
  local analysis
  analysis=$(analyze_trend)

  # 生成报告
  local report
  report=$(generate_report "$analysis")

  if [[ -n "$OUTPUT" ]]; then
    echo "$report" > "$OUTPUT"
    echo "Report written to: $OUTPUT"
  else
    echo "$report"
  fi

  # 严格模式检查
  if [[ "$STRICT" == "true" ]]; then
    local drift_percent trend
    drift_percent=$(echo "$analysis" | grep "^drift_percent|" | cut -d'|' -f2)
    trend=$(echo "$analysis" | grep "^trend|" | cut -d'|' -f2)

    if [[ $drift_percent -ge $DRIFT_THRESHOLD && "$trend" == "degrading" ]]; then
      echo "Strict mode: Degrading trend with ${drift_percent}% drift detected"
      exit 1
    fi
  fi
}

main
