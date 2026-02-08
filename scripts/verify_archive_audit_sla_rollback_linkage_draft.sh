#!/usr/bin/env bash
# verify_archive_audit_sla_rollback_linkage_draft.sh
# B55: SLA/回滚联动报告归档验真脚本草案
# 验证 SLA 预警与回滚演练报告的归档完整性与一致性

set -euo pipefail

# ============================================================
# 参数解析
# ============================================================
DRY_RUN=false
VERIFY_ID=""
OUTPUT=""
SLA_ALERT_REPORT=""
ROLLBACK_DRILL_REPORT=""
LINKAGE_DRILL_REPORT=""
ARCHIVE_ROOT="artifacts/audit"
STRICT=false

usage() {
  cat <<EOF
Usage: $0 [OPTIONS]

Options:
  --sla-alert-report FILE       B48 SLA 违约预警报告（可选）
  --rollback-drill-report FILE  B50 回滚演练计划报告（可选）
  --linkage-drill-report FILE   B52 SLA/回滚联动演练报告（可选）
  --archive-root DIR            归档根目录（默认 artifacts/audit）
  --verify-id ID                验真批次 ID（必需）
  --output FILE                 输出报告路径（可选）
  --dry-run                     仅检查，不修复
  --strict                      严格模式：验真失败则 exit 1
  -h, --help                    显示帮助

Examples:
  $0 --dry-run --verify-id b55_dryrun_sample
  $0 --sla-alert-report docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_SAMPLE_B48.md \\
     --rollback-drill-report docs/test_reports/ARCHIVE_AUDIT_LINKAGE_ROLLBACK_DRILL_PLAN_SAMPLE_B50.md \\
     --linkage-drill-report docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_LINKAGE_DRILL_SAMPLE_B52.md \\
     --verify-id b55_sample_20260207_2100 \\
     --output docs/test_reports/ARCHIVE_AUDIT_SLA_ROLLBACK_VERIFY_SAMPLE_B55.md
  $0 --sla-alert-report ... --strict
EOF
  exit 0
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --sla-alert-report) SLA_ALERT_REPORT="$2"; shift 2 ;;
    --rollback-drill-report) ROLLBACK_DRILL_REPORT="$2"; shift 2 ;;
    --linkage-drill-report) LINKAGE_DRILL_REPORT="$2"; shift 2 ;;
    --archive-root) ARCHIVE_ROOT="$2"; shift 2 ;;
    --verify-id) VERIFY_ID="$2"; shift 2 ;;
    --output) OUTPUT="$2"; shift 2 ;;
    --dry-run) DRY_RUN=true; shift ;;
    --strict) STRICT=true; shift ;;
    -h|--help) usage ;;
    *) echo "Unknown option: $1"; usage ;;
  esac
done

if [[ -z "$VERIFY_ID" ]]; then
  echo "Error: --verify-id is required"
  exit 1
fi

TIMESTAMP=$(date +%Y-%m-%d\ %H:%M:%S\ %z)

# ============================================================
# 验真检查函数
# ============================================================
check_file_exists() {
  local file="$1"
  local label="$2"
  if [[ -f "$file" ]]; then
    echo "pass|$label|$file|exists"
  else
    echo "fail|$label|$file|missing"
  fi
}

check_file_not_empty() {
  local file="$1"
  local label="$2"
  if [[ -f "$file" && -s "$file" ]]; then
    echo "pass|$label|$file|non-empty"
  elif [[ -f "$file" ]]; then
    echo "fail|$label|$file|empty"
  else
    echo "skip|$label|$file|not-found"
  fi
}

extract_field() {
  local file="$1"
  local field="$2"
  if [[ -f "$file" ]]; then
    grep -E "^\| *${field} *\|" "$file" 2>/dev/null | head -1 | sed -E 's/.*\| *[^|]+ *\| *([^|]+) *\|.*/\1/' | xargs
  else
    echo "n/a"
  fi
}

check_field_consistency() {
  local file1="$1"
  local file2="$2"
  local field="$3"
  local label="$4"

  local val1 val2
  val1=$(extract_field "$file1" "$field")
  val2=$(extract_field "$file2" "$field")

  if [[ "$val1" == "$val2" ]]; then
    echo "pass|$label|$field|$val1=$val2"
  else
    echo "fail|$label|$field|$val1!=$val2"
  fi
}

# ============================================================
# 主验真流程
# ============================================================
run_verification() {
  local checks=()

  # 1. 文件存在性检查
  if [[ -n "$SLA_ALERT_REPORT" ]]; then
    checks+=("$(check_file_exists "$SLA_ALERT_REPORT" "sla_alert_report")")
    checks+=("$(check_file_not_empty "$SLA_ALERT_REPORT" "sla_alert_content")")
  fi

  if [[ -n "$ROLLBACK_DRILL_REPORT" ]]; then
    checks+=("$(check_file_exists "$ROLLBACK_DRILL_REPORT" "rollback_drill_report")")
    checks+=("$(check_file_not_empty "$ROLLBACK_DRILL_REPORT" "rollback_drill_content")")
  fi

  if [[ -n "$LINKAGE_DRILL_REPORT" ]]; then
    checks+=("$(check_file_exists "$LINKAGE_DRILL_REPORT" "linkage_drill_report")")
    checks+=("$(check_file_not_empty "$LINKAGE_DRILL_REPORT" "linkage_drill_content")")
  fi

  # 2. 归档目录检查
  if [[ -d "$ARCHIVE_ROOT" ]]; then
    checks+=("pass|archive_root|$ARCHIVE_ROOT|exists")
  else
    checks+=("warn|archive_root|$ARCHIVE_ROOT|missing")
  fi

  # 3. 跨报告一致性检查
  if [[ -n "$SLA_ALERT_REPORT" && -n "$LINKAGE_DRILL_REPORT" ]]; then
    # 检查 SLA 预警项是否在联动报告中有对应
    local sla_items linkage_items
    sla_items=$(grep -c "^\| *[A-Z]" "$SLA_ALERT_REPORT" 2>/dev/null || echo "0")
    linkage_items=$(grep -c "^\| *[A-Z]" "$LINKAGE_DRILL_REPORT" 2>/dev/null || echo "0")

    if [[ "$sla_items" -gt 0 && "$linkage_items" -gt 0 ]]; then
      checks+=("pass|cross_report_linkage|sla=$sla_items,linkage=$linkage_items|populated")
    else
      checks+=("warn|cross_report_linkage|sla=$sla_items,linkage=$linkage_items|sparse")
    fi
  fi

  # 4. 时间戳一致性检查
  if [[ -n "$SLA_ALERT_REPORT" && -n "$ROLLBACK_DRILL_REPORT" ]]; then
    local sla_ts rollback_ts
    sla_ts=$(extract_field "$SLA_ALERT_REPORT" "generated_at")
    rollback_ts=$(extract_field "$ROLLBACK_DRILL_REPORT" "generated_at")

    if [[ "$sla_ts" != "n/a" && "$rollback_ts" != "n/a" ]]; then
      checks+=("pass|timestamp_presence|sla=$sla_ts,rollback=$rollback_ts|both-present")
    else
      checks+=("warn|timestamp_presence|sla=$sla_ts,rollback=$rollback_ts|incomplete")
    fi
  fi

  # 输出检查结果
  for check in "${checks[@]:-}"; do
    echo "$check"
  done
}

# ============================================================
# 报告生成
# ============================================================
generate_report() {
  local checks=("$@")
  local total=${#checks[@]}
  local pass_count=0
  local fail_count=0
  local warn_count=0
  local skip_count=0

  for check in "${checks[@]:-}"; do
    local status
    status=$(echo "$check" | cut -d'|' -f1)
    case "$status" in
      pass) pass_count=$((pass_count + 1)) ;;
      fail) fail_count=$((fail_count + 1)) ;;
      warn) warn_count=$((warn_count + 1)) ;;
      skip) skip_count=$((skip_count + 1)) ;;
    esac
  done

  local verify_status="pass"
  if [[ $fail_count -gt 0 ]]; then
    verify_status="fail"
  elif [[ $warn_count -gt 0 ]]; then
    verify_status="warn"
  fi

  cat <<EOF
# Archive Audit SLA/Rollback Linkage Verification Report

## Metadata

| Field | Value |
|-------|-------|
| verify_id | $VERIFY_ID |
| generated_at | $TIMESTAMP |
| sla_alert_report | ${SLA_ALERT_REPORT:-n/a} |
| rollback_drill_report | ${ROLLBACK_DRILL_REPORT:-n/a} |
| linkage_drill_report | ${LINKAGE_DRILL_REPORT:-n/a} |
| archive_root | $ARCHIVE_ROOT |
| dry_run | $DRY_RUN |

## Summary

| Metric | Value |
|--------|-------|
| total_checks | $total |
| pass_checks | $pass_count |
| fail_checks | $fail_count |
| warn_checks | $warn_count |
| skip_checks | $skip_count |
| verify_status | $verify_status |

## Verification Checks

| status | check_label | target | detail |
|--------|-------------|--------|--------|
EOF

  for check in "${checks[@]:-}"; do
    IFS='|' read -r status label target detail <<< "$check"
    echo "| $status | $label | $target | $detail |"
  done

  if [[ ${#checks[@]} -eq 0 ]]; then
    echo "| (none) | - | - | - |"
  fi

  cat <<EOF

## Archive Integrity

| Check | Status |
|-------|--------|
| sla_alert_archived | $(check_archive_status "$SLA_ALERT_REPORT") |
| rollback_drill_archived | $(check_archive_status "$ROLLBACK_DRILL_REPORT") |
| linkage_drill_archived | $(check_archive_status "$LINKAGE_DRILL_REPORT") |

## Next Steps

EOF

  if [[ "$verify_status" == "pass" ]]; then
    echo "- All verification checks passed."
    echo "- Reports are ready for release gate."
  elif [[ "$verify_status" == "warn" ]]; then
    echo "- Some warnings detected, review recommended."
    echo "- Consider re-generating missing reports."
  else
    echo "- Verification failed, address issues before proceeding."
    echo "- Re-run after fixing failed checks."
  fi

  cat <<EOF

## Release Advice

| Condition | Advice |
|-----------|--------|
| verify_status=pass | proceed-to-release |
| verify_status=warn | review-and-proceed |
| verify_status=fail | block-until-fixed |
EOF
}

check_archive_status() {
  local file="$1"
  if [[ -z "$file" || "$file" == "n/a" ]]; then
    echo "n/a"
  elif [[ -f "$file" ]]; then
    echo "present"
  else
    echo "missing"
  fi
}

# ============================================================
# 主流程
# ============================================================
main() {
  # 运行验真检查
  mapfile -t check_results < <(run_verification)

  # 生成报告
  local report
  report=$(generate_report "${check_results[@]:-}")

  if [[ -n "$OUTPUT" ]]; then
    echo "$report" > "$OUTPUT"
    echo "Report written to: $OUTPUT"
  else
    echo "$report"
  fi

  # 严格模式检查
  if [[ "$STRICT" == "true" ]]; then
    local fail_count=0
    for check in "${check_results[@]:-}"; do
      local status
      status=$(echo "$check" | cut -d'|' -f1)
      if [[ "$status" == "fail" ]]; then
        fail_count=$((fail_count + 1))
      fi
    done
    if [[ $fail_count -gt 0 ]]; then
      echo "Strict mode: $fail_count verification checks failed"
      exit 1
    fi
  fi
}

main
