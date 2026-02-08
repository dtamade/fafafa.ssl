#!/usr/bin/env bash
# generate_archive_audit_full_chain_closure_report_draft.sh
# B59: 归档审计全链路闭环验收报告草案
# 汇总所有归档审计环节，生成全链路闭环验收报告

set -euo pipefail

# ============================================================
# 参数解析
# ============================================================
REPORT_ID=""
OUTPUT=""
CLOSURE_GATE_REPORT=""
AUTOFIX_REPORT=""
REVALIDATE_REPORT=""
TREND_REPORT=""
RETRY_REPORT=""
SLA_DRILL_REPORT=""
VERIFY_REPORT=""
STRICT=false

usage() {
  cat <<EOF
Usage: $0 [OPTIONS]

Options:
  --closure-gate-report FILE   B53 闭环门禁报告（可选）
  --autofix-report FILE        B54 自动修复报告（可选）
  --revalidate-report FILE     B58 重验报告（可选）
  --trend-report FILE          B57 趋势报告（可选）
  --retry-report FILE          B56 重试报告（可选）
  --sla-drill-report FILE      B52 SLA演练报告（可选）
  --verify-report FILE         B55 验真报告（可选）
  --report-id ID               报告批次 ID（必需）
  --output FILE                输出报告路径（可选）
  --strict                     严格模式：任一环节失败则 exit 1
  -h, --help                   显示帮助

Examples:
  $0 --report-id b59_sample_20260207_2300 \\
     --closure-gate-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_SAMPLE_B53.md \\
     --autofix-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_AUTOFIX_SAMPLE_B54.md \\
     --output docs/test_reports/ARCHIVE_AUDIT_FULL_CHAIN_CLOSURE_SAMPLE_B59.md
EOF
  exit 0
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --closure-gate-report) CLOSURE_GATE_REPORT="$2"; shift 2 ;;
    --autofix-report) AUTOFIX_REPORT="$2"; shift 2 ;;
    --revalidate-report) REVALIDATE_REPORT="$2"; shift 2 ;;
    --trend-report) TREND_REPORT="$2"; shift 2 ;;
    --retry-report) RETRY_REPORT="$2"; shift 2 ;;
    --sla-drill-report) SLA_DRILL_REPORT="$2"; shift 2 ;;
    --verify-report) VERIFY_REPORT="$2"; shift 2 ;;
    --report-id) REPORT_ID="$2"; shift 2 ;;
    --output) OUTPUT="$2"; shift 2 ;;
    --strict) STRICT=true; shift ;;
    -h|--help) usage ;;
    *) echo "Unknown option: $1"; usage ;;
  esac
done

if [[ -z "$REPORT_ID" ]]; then
  echo "Error: --report-id is required"
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

check_report_status() {
  local file="$1"
  local status_field="$2"

  if [[ ! -f "$file" ]]; then
    echo "missing"
    return
  fi

  local status
  status=$(extract_field "$file" "$status_field")

  case "$status" in
    pass|complete|success) echo "pass" ;;
    warn|warning|pending) echo "warn" ;;
    fail|error|failed) echo "fail" ;;
    *) echo "unknown" ;;
  esac
}

# ============================================================
# 环节状态收集
# ============================================================
collect_chain_status() {
  local chain_data=""

  # B53: 闭环门禁
  local gate_status="n/a"
  if [[ -n "$CLOSURE_GATE_REPORT" ]]; then
    gate_status=$(check_report_status "$CLOSURE_GATE_REPORT" "acceptance_status")
  fi
  chain_data+="closure_gate|$gate_status|${CLOSURE_GATE_REPORT:-n/a}\n"

  # B54: 自动修复
  local autofix_status="n/a"
  if [[ -n "$AUTOFIX_REPORT" ]]; then
    autofix_status=$(check_report_status "$AUTOFIX_REPORT" "autofix_status")
  fi
  chain_data+="autofix|$autofix_status|${AUTOFIX_REPORT:-n/a}\n"

  # B55: 验真
  local verify_status="n/a"
  if [[ -n "$VERIFY_REPORT" ]]; then
    verify_status=$(check_report_status "$VERIFY_REPORT" "verify_status")
  fi
  chain_data+="verify|$verify_status|${VERIFY_REPORT:-n/a}\n"

  # B56: 重试
  local retry_status="n/a"
  if [[ -n "$RETRY_REPORT" ]]; then
    retry_status=$(check_report_status "$RETRY_REPORT" "retry_status")
  fi
  chain_data+="retry|$retry_status|${RETRY_REPORT:-n/a}\n"

  # B57: 趋势
  local trend_status="n/a"
  if [[ -n "$TREND_REPORT" ]]; then
    trend_status=$(check_report_status "$TREND_REPORT" "review_status")
  fi
  chain_data+="trend|$trend_status|${TREND_REPORT:-n/a}\n"

  # B58: 重验
  local revalidate_status="n/a"
  if [[ -n "$REVALIDATE_REPORT" ]]; then
    revalidate_status=$(check_report_status "$REVALIDATE_REPORT" "overall_status")
  fi
  chain_data+="revalidate|$revalidate_status|${REVALIDATE_REPORT:-n/a}\n"

  # B52: SLA演练
  local sla_status="n/a"
  if [[ -n "$SLA_DRILL_REPORT" ]]; then
    sla_status=$(check_report_status "$SLA_DRILL_REPORT" "drill_status")
  fi
  chain_data+="sla_drill|$sla_status|${SLA_DRILL_REPORT:-n/a}\n"

  echo -e "$chain_data"
}

# ============================================================
# 综合状态计算
# ============================================================
calculate_overall_status() {
  local chain_data="$1"

  local total=0
  local pass=0
  local warn=0
  local fail=0
  local missing=0

  while IFS='|' read -r stage status report; do
    [[ -z "$stage" ]] && continue
    total=$((total + 1))
    case "$status" in
      pass) pass=$((pass + 1)) ;;
      warn) warn=$((warn + 1)) ;;
      fail) fail=$((fail + 1)) ;;
      missing) missing=$((missing + 1)) ;;
      n/a) ;;  # 未配置的环节不计入
      *) warn=$((warn + 1)) ;;
    esac
  done <<< "$chain_data"

  local configured=$((total - missing))
  local overall="pending"

  if [[ $fail -gt 0 ]]; then
    overall="fail"
  elif [[ $warn -gt 0 ]]; then
    overall="warn"
  elif [[ $pass -gt 0 && $pass -eq $configured ]]; then
    overall="pass"
  fi

  echo "total_stages|$total"
  echo "configured_stages|$configured"
  echo "pass_stages|$pass"
  echo "warn_stages|$warn"
  echo "fail_stages|$fail"
  echo "missing_stages|$missing"
  echo "overall_status|$overall"
}

# ============================================================
# 报告生成
# ============================================================
generate_report() {
  local chain_data="$1"
  local summary="$2"

  # 解析汇总数据
  local total configured pass_count warn_count fail_count missing overall
  total=$(echo "$summary" | grep "^total_stages|" | cut -d'|' -f2)
  configured=$(echo "$summary" | grep "^configured_stages|" | cut -d'|' -f2)
  pass_count=$(echo "$summary" | grep "^pass_stages|" | cut -d'|' -f2)
  warn_count=$(echo "$summary" | grep "^warn_stages|" | cut -d'|' -f2)
  fail_count=$(echo "$summary" | grep "^fail_stages|" | cut -d'|' -f2)
  missing=$(echo "$summary" | grep "^missing_stages|" | cut -d'|' -f2)
  overall=$(echo "$summary" | grep "^overall_status|" | cut -d'|' -f2)

  # 计算完成率
  local completion_rate=0
  if [[ $configured -gt 0 ]]; then
    completion_rate=$(( pass_count * 100 / configured ))
  fi

  cat <<EOF
# Archive Audit Full Chain Closure Report

## Metadata

| Field | Value |
|-------|-------|
| report_id | $REPORT_ID |
| generated_at | $TIMESTAMP |
| total_stages | $total |
| configured_stages | $configured |

## Summary

| Metric | Value |
|--------|-------|
| pass_stages | $pass_count |
| warn_stages | $warn_count |
| fail_stages | $fail_count |
| missing_stages | $missing |
| completion_rate | ${completion_rate}% |
| overall_status | $overall |

## Chain Status

| Stage | Batch | Status | Report |
|-------|-------|--------|--------|
EOF

  # 输出各环节状态
  while IFS='|' read -r stage status report; do
    [[ -z "$stage" ]] && continue
    local batch=""
    case "$stage" in
      closure_gate) batch="B53" ;;
      autofix) batch="B54" ;;
      verify) batch="B55" ;;
      retry) batch="B56" ;;
      trend) batch="B57" ;;
      revalidate) batch="B58" ;;
      sla_drill) batch="B52" ;;
      *) batch="n/a" ;;
    esac
    local report_name
    if [[ "$report" != "n/a" && -f "$report" ]]; then
      report_name=$(basename "$report")
    else
      report_name="n/a"
    fi
    echo "| $stage | $batch | $status | $report_name |"
  done <<< "$chain_data"

  cat <<EOF

## Chain Flow

~~~
[B53: Closure Gate] --> [B54: Autofix] --> [B58: Revalidate]
         |                    |
         v                    v
    [B55: Verify]        [B56: Retry]
         |                    |
         v                    v
    [B52: SLA Drill]     [B57: Trend]
         |                    |
         +--------------------+
                  |
                  v
         [B59: Full Chain Report]
~~~

## Assessment

| Check | Status | Detail |
|-------|--------|--------|
| chain_coverage | $(if [[ $configured -ge 3 ]]; then echo "adequate"; else echo "partial"; fi) | $configured of $total stages configured |
| pass_rate | $(if [[ $completion_rate -ge 80 ]]; then echo "high"; elif [[ $completion_rate -ge 50 ]]; then echo "medium"; else echo "low"; fi) | ${completion_rate}% |
| blocking_issues | $(if [[ $fail_count -gt 0 ]]; then echo "yes"; else echo "no"; fi) | $fail_count failed stages |
| overall_readiness | $overall | $(if [[ "$overall" == "pass" ]]; then echo "ready-for-release"; elif [[ "$overall" == "warn" ]]; then echo "review-required"; else echo "action-required"; fi) |

## Recommendations

EOF

  if [[ "$overall" == "pass" ]]; then
    echo "- All configured stages passed."
    echo "- Full chain closure verified."
    echo "- Ready to proceed with release."
  elif [[ "$overall" == "warn" ]]; then
    echo "- Some stages require attention."
    echo "- Review warning stages before proceeding."
    echo "- Consider addressing warnings for cleaner release."
  else
    echo "- One or more stages failed."
    echo "- Address failed stages before release."
    echo "- Re-run full chain verification after fixes."
  fi

  if [[ $missing -gt 0 ]]; then
    echo "- $missing stages have missing reports."
    echo "- Consider running missing stages for complete coverage."
  fi

  cat <<EOF

## Release Advice

| Condition | Advice |
|-----------|--------|
| overall_status=pass | proceed-to-release |
| overall_status=warn | review-before-release |
| overall_status=fail | block-until-resolved |
| overall_status=pending | complete-chain-first |
EOF
}

# ============================================================
# 主流程
# ============================================================
main() {
  # 收集各环节状态
  local chain_data
  chain_data=$(collect_chain_status)

  # 计算综合状态
  local summary
  summary=$(calculate_overall_status "$chain_data")

  # 生成报告
  local report
  report=$(generate_report "$chain_data" "$summary")

  if [[ -n "$OUTPUT" ]]; then
    echo "$report" > "$OUTPUT"
    echo "Report written to: $OUTPUT"
  else
    echo "$report"
  fi

  # 严格模式检查
  if [[ "$STRICT" == "true" ]]; then
    local overall
    overall=$(echo "$summary" | grep "^overall_status|" | cut -d'|' -f2)

    if [[ "$overall" == "fail" ]]; then
      echo "Strict mode: Chain closure failed with status=$overall"
      exit 1
    fi
  fi
}

main
