#!/usr/bin/env bash
# revalidate_closure_gate_after_autofix_draft.sh
# B58: 自动修复执行后闭环门禁重验脚本草案
# 在自动修复执行后重新运行闭环门禁验证，确认修复效果

set -euo pipefail

# ============================================================
# 参数解析
# ============================================================
REVALIDATE_ID=""
OUTPUT=""
AUTOFIX_REPORT=""
CLOSURE_GATE_SCRIPT=""
CLOSURE_GATE_ARGS=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<EOF
Usage: $0 [OPTIONS]

Options:
  --autofix-report FILE        B54 自动修复报告（必需）
  --closure-gate-script FILE   闭环门禁脚本路径（必需）
  --closure-gate-args ARGS     闭环门禁脚本参数（可选）
  --revalidate-id ID           重验批次 ID（必需）
  --output FILE                输出报告路径（可选）
  --dry-run                    仅生成重验计划，不实际执行
  --strict                     严格模式：重验失败则 exit 1
  -h, --help                   显示帮助

Examples:
  $0 --dry-run --revalidate-id b58_dryrun_sample
  $0 --autofix-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_AUTOFIX_SAMPLE_B54.md \\
     --closure-gate-script scripts/validate_archive_audit_writeback_coverage_closure_gate_draft.sh \\
     --revalidate-id b58_sample_20260207_2200 \\
     --output docs/test_reports/ARCHIVE_AUDIT_CLOSURE_REVALIDATE_SAMPLE_B58.md
EOF
  exit 0
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --autofix-report) AUTOFIX_REPORT="$2"; shift 2 ;;
    --closure-gate-script) CLOSURE_GATE_SCRIPT="$2"; shift 2 ;;
    --closure-gate-args) CLOSURE_GATE_ARGS="$2"; shift 2 ;;
    --revalidate-id) REVALIDATE_ID="$2"; shift 2 ;;
    --output) OUTPUT="$2"; shift 2 ;;
    --dry-run) DRY_RUN=true; shift ;;
    --strict) STRICT=true; shift ;;
    -h|--help) usage ;;
    *) echo "Unknown option: $1"; usage ;;
  esac
done

if [[ -z "$REVALIDATE_ID" ]]; then
  echo "Error: --revalidate-id is required"
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
# 自动修复报告解析
# ============================================================
parse_autofix_report() {
  local report="$1"

  if [[ ! -f "$report" ]]; then
    echo "autofix_status|n/a"
    echo "total_actions|0"
    echo "applied_actions|0"
    echo "failed_actions|0"
    return
  fi

  local status total applied failed
  status=$(extract_field "$report" "autofix_status")
  total=$(extract_field "$report" "total_actions")
  applied=$(extract_field "$report" "applied_actions")
  failed=$(extract_field "$report" "failed_actions")

  echo "autofix_status|$status"
  echo "total_actions|$(extract_numeric "$total")"
  echo "applied_actions|$(extract_numeric "$applied")"
  echo "failed_actions|$(extract_numeric "$failed")"
}

# ============================================================
# 闭环门禁重验
# ============================================================
run_closure_gate_revalidation() {
  local script="$1"
  local args="$2"

  if [[ "$DRY_RUN" == "true" ]]; then
    echo "revalidation_mode|dry-run"
    echo "revalidation_status|pending"
    echo "gate_pass_rate|n/a"
    echo "gate_total_checks|n/a"
    echo "gate_failed_checks|n/a"
    return
  fi

  if [[ ! -f "$script" ]]; then
    echo "revalidation_mode|error"
    echo "revalidation_status|script-not-found"
    echo "gate_pass_rate|n/a"
    echo "gate_total_checks|n/a"
    echo "gate_failed_checks|n/a"
    return
  fi

  # 创建临时输出文件
  local temp_output
  temp_output=$(mktemp)

  # 执行闭环门禁脚本
  local exit_code=0
  if [[ -n "$args" ]]; then
    bash "$script" $args --output "$temp_output" 2>/dev/null || exit_code=$?
  else
    bash "$script" --dry-run --gate-id "revalidate_${REVALIDATE_ID}" --output "$temp_output" 2>/dev/null || exit_code=$?
  fi

  # 解析结果
  local total failed pass_rate status
  if [[ -f "$temp_output" ]]; then
    total=$(extract_field "$temp_output" "total_checks")
    failed=$(extract_field "$temp_output" "checks_failed")
    total=$(extract_numeric "$total")
    failed=$(extract_numeric "$failed")

    if [[ $total -gt 0 ]]; then
      pass_rate=$(( (total - failed) * 100 / total ))
    else
      pass_rate=0
    fi

    if [[ $exit_code -eq 0 && $failed -eq 0 ]]; then
      status="pass"
    elif [[ $exit_code -eq 0 ]]; then
      status="warn"
    else
      status="fail"
    fi
  else
    total=0
    failed=0
    pass_rate=0
    status="error"
  fi

  rm -f "$temp_output"

  echo "revalidation_mode|execute"
  echo "revalidation_status|$status"
  echo "gate_pass_rate|$pass_rate"
  echo "gate_total_checks|$total"
  echo "gate_failed_checks|$failed"
}

# ============================================================
# 报告生成
# ============================================================
generate_report() {
  local autofix_data="$1"
  local revalidation_data="$2"

  # 解析自动修复数据
  local autofix_status total_actions applied_actions failed_actions
  autofix_status=$(echo "$autofix_data" | grep "^autofix_status|" | cut -d'|' -f2)
  total_actions=$(echo "$autofix_data" | grep "^total_actions|" | cut -d'|' -f2)
  applied_actions=$(echo "$autofix_data" | grep "^applied_actions|" | cut -d'|' -f2)
  failed_actions=$(echo "$autofix_data" | grep "^failed_actions|" | cut -d'|' -f2)

  # 解析重验数据
  local revalidation_mode revalidation_status gate_pass_rate gate_total gate_failed
  revalidation_mode=$(echo "$revalidation_data" | grep "^revalidation_mode|" | cut -d'|' -f2)
  revalidation_status=$(echo "$revalidation_data" | grep "^revalidation_status|" | cut -d'|' -f2)
  gate_pass_rate=$(echo "$revalidation_data" | grep "^gate_pass_rate|" | cut -d'|' -f2)
  gate_total=$(echo "$revalidation_data" | grep "^gate_total_checks|" | cut -d'|' -f2)
  gate_failed=$(echo "$revalidation_data" | grep "^gate_failed_checks|" | cut -d'|' -f2)

  # 计算综合状态
  local overall_status="pending"
  if [[ "$revalidation_status" == "pass" ]]; then
    overall_status="pass"
  elif [[ "$revalidation_status" == "warn" ]]; then
    overall_status="warn"
  elif [[ "$revalidation_status" == "fail" || "$revalidation_status" == "error" ]]; then
    overall_status="fail"
  fi

  cat <<EOF
# Archive Audit Closure Gate Revalidation Report

## Metadata

| Field | Value |
|-------|-------|
| revalidate_id | $REVALIDATE_ID |
| generated_at | $TIMESTAMP |
| mode | $(if [[ "$DRY_RUN" == "true" ]]; then echo "dry-run"; else echo "execute"; fi) |
| autofix_report | ${AUTOFIX_REPORT:-n/a} |
| closure_gate_script | ${CLOSURE_GATE_SCRIPT:-n/a} |

## Autofix Summary

| Metric | Value |
|--------|-------|
| autofix_status | $autofix_status |
| total_actions | $total_actions |
| applied_actions | $applied_actions |
| failed_actions | $failed_actions |

## Revalidation Results

| Metric | Value |
|--------|-------|
| revalidation_mode | $revalidation_mode |
| revalidation_status | $revalidation_status |
| gate_total_checks | $gate_total |
| gate_failed_checks | $gate_failed |
| gate_pass_rate | ${gate_pass_rate}% |

## Overall Assessment

| Check | Status | Detail |
|-------|--------|--------|
| autofix_applied | $(if [[ "$applied_actions" != "0" && "$applied_actions" != "n/a" ]]; then echo "yes"; else echo "no"; fi) | $applied_actions actions applied |
| autofix_failures | $(if [[ "$failed_actions" == "0" || "$failed_actions" == "n/a" ]]; then echo "none"; else echo "detected"; fi) | $failed_actions failures |
| gate_revalidation | $revalidation_status | pass_rate=${gate_pass_rate}% |
| overall_status | $overall_status | $(if [[ "$overall_status" == "pass" ]]; then echo "ready-for-release"; elif [[ "$overall_status" == "warn" ]]; then echo "review-required"; else echo "action-required"; fi) |

## Recommendations

EOF

  if [[ "$overall_status" == "pass" ]]; then
    echo "- All autofix actions applied successfully."
    echo "- Closure gate revalidation passed."
    echo "- Ready to proceed with release."
  elif [[ "$overall_status" == "warn" ]]; then
    echo "- Some checks require attention."
    echo "- Review failed checks before proceeding."
    echo "- Consider manual intervention for remaining issues."
  elif [[ "$DRY_RUN" == "true" ]]; then
    echo "- Dry-run mode: no actual revalidation performed."
    echo "- Re-run with actual parameters to execute revalidation."
    echo "- Ensure autofix has been applied before revalidation."
  else
    echo "- Revalidation failed or encountered errors."
    echo "- Review autofix report for failed actions."
    echo "- Address root causes before re-attempting."
  fi

  cat <<EOF

## Release Advice

| Condition | Advice |
|-----------|--------|
| overall_status=pass | proceed-to-release |
| overall_status=warn | review-before-release |
| overall_status=fail | block-until-resolved |
| overall_status=pending | complete-revalidation-first |
EOF
}

# ============================================================
# 主流程
# ============================================================
main() {
  # 解析自动修复报告
  local autofix_data
  autofix_data=$(parse_autofix_report "$AUTOFIX_REPORT")

  # 执行闭环门禁重验
  local revalidation_data
  revalidation_data=$(run_closure_gate_revalidation "$CLOSURE_GATE_SCRIPT" "$CLOSURE_GATE_ARGS")

  # 生成报告
  local report
  report=$(generate_report "$autofix_data" "$revalidation_data")

  if [[ -n "$OUTPUT" ]]; then
    echo "$report" > "$OUTPUT"
    echo "Report written to: $OUTPUT"
  else
    echo "$report"
  fi

  # 严格模式检查
  if [[ "$STRICT" == "true" ]]; then
    local revalidation_status
    revalidation_status=$(echo "$revalidation_data" | grep "^revalidation_status|" | cut -d'|' -f2)

    if [[ "$revalidation_status" == "fail" || "$revalidation_status" == "error" ]]; then
      echo "Strict mode: Revalidation failed with status=$revalidation_status"
      exit 1
    fi
  fi
}

main
