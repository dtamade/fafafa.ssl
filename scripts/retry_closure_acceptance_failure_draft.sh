#!/usr/bin/env bash
# retry_closure_acceptance_failure_draft.sh
# B56: 闭环验收失败自动重试分流脚本草案
# 根据闭环门禁失败项，自动分流重试或升级处理

set -euo pipefail

# ============================================================
# 参数解析
# ============================================================
DRY_RUN=true
RETRY_ID=""
OUTPUT=""
CLOSURE_GATE_REPORT=""
AUTOFIX_REPORT=""
VERIFY_REPORT=""
MAX_RETRIES=3
RETRY_DELAY=5
ESCALATE_THRESHOLD=2
STRICT=false

usage() {
  cat <<EOF
Usage: $0 [OPTIONS]

Options:
  --closure-gate-report FILE   B53 闭环门禁报告（必需）
  --autofix-report FILE        B54 自动修复报告（可选）
  --verify-report FILE         B55 验真报告（可选）
  --retry-id ID                重试批次 ID（必需）
  --output FILE                输出报告路径（可选）
  --max-retries N              最大重试次数（默认 3）
  --retry-delay N              重试间隔秒数（默认 5）
  --escalate-threshold N       升级阈值（默认 2 次失败后升级）
  --dry-run                    仅生成重试计划，不执行（默认）
  --apply                      实际执行重试
  --strict                     严格模式：有未解决项则 exit 1
  -h, --help                   显示帮助

Examples:
  $0 --dry-run --retry-id b56_dryrun_sample
  $0 --closure-gate-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_CLOSURE_ACCEPTANCE_GATE_SAMPLE_B53.md \\
     --autofix-report docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_AUTOFIX_SAMPLE_B54.md \\
     --retry-id b56_sample_20260207_2100 \\
     --output docs/test_reports/ARCHIVE_AUDIT_CLOSURE_RETRY_SAMPLE_B56.md
  $0 --closure-gate-report ... --strict
EOF
  exit 0
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --closure-gate-report) CLOSURE_GATE_REPORT="$2"; shift 2 ;;
    --autofix-report) AUTOFIX_REPORT="$2"; shift 2 ;;
    --verify-report) VERIFY_REPORT="$2"; shift 2 ;;
    --retry-id) RETRY_ID="$2"; shift 2 ;;
    --output) OUTPUT="$2"; shift 2 ;;
    --max-retries) MAX_RETRIES="$2"; shift 2 ;;
    --retry-delay) RETRY_DELAY="$2"; shift 2 ;;
    --escalate-threshold) ESCALATE_THRESHOLD="$2"; shift 2 ;;
    --dry-run) DRY_RUN=true; shift ;;
    --apply) DRY_RUN=false; shift ;;
    --strict) STRICT=true; shift ;;
    -h|--help) usage ;;
    *) echo "Unknown option: $1"; usage ;;
  esac
done

if [[ -z "$RETRY_ID" ]]; then
  echo "Error: --retry-id is required"
  exit 1
fi

TIMESTAMP=$(date +%Y-%m-%d\ %H:%M:%S\ %z)
MODE=$( [[ "$DRY_RUN" == "true" ]] && echo "dry-run" || echo "apply" )

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

extract_failed_items() {
  local file="$1"
  if [[ -f "$file" ]]; then
    # 提取失败状态的项目
    grep -E "^\|.*\| *(fail|pending|simulated) *\|" "$file" 2>/dev/null | while IFS='|' read -r _ item_id priority owner status _; do
      item_id=$(echo "$item_id" | xargs)
      priority=$(echo "$priority" | xargs)
      owner=$(echo "$owner" | xargs)
      status=$(echo "$status" | xargs)
      [[ -z "$item_id" || "$item_id" =~ ^-+$ ]] && continue
      echo "$item_id|$priority|$owner|$status|0"
    done
  fi
}

# ============================================================
# 重试逻辑
# ============================================================
simulate_retry() {
  local item="$1"
  local item_id priority owner status retry_count
  IFS='|' read -r item_id priority owner status retry_count <<< "$item"

  retry_count=$((retry_count + 1))

  if [[ $retry_count -ge $ESCALATE_THRESHOLD ]]; then
    echo "$item_id|$priority|$owner|escalated|$retry_count"
  else
    # 模拟重试结果（实际实现中会调用真实检查）
    if [[ "$DRY_RUN" == "true" ]]; then
      echo "$item_id|$priority|$owner|retry-pending|$retry_count"
    else
      # 实际重试逻辑占位
      echo "$item_id|$priority|$owner|retry-executed|$retry_count"
    fi
  fi
}

# ============================================================
# 分流决策
# ============================================================
decide_action() {
  local item="$1"
  local item_id priority owner status retry_count
  IFS='|' read -r item_id priority owner status retry_count <<< "$item"

  case "$status" in
    pass|closed|waived)
      echo "skip"
      ;;
    escalated)
      echo "escalate"
      ;;
    *)
      if [[ $retry_count -ge $MAX_RETRIES ]]; then
        echo "escalate"
      else
        echo "retry"
      fi
      ;;
  esac
}

# ============================================================
# 报告生成
# ============================================================
generate_report() {
  local items=("$@")
  local total=${#items[@]}
  local retry_count=0
  local escalate_count=0
  local skip_count=0
  local pending_count=0

  for item in "${items[@]:-}"; do
    local action
    action=$(decide_action "$item")
    case "$action" in
      retry) retry_count=$((retry_count + 1)) ;;
      escalate) escalate_count=$((escalate_count + 1)) ;;
      skip) skip_count=$((skip_count + 1)) ;;
    esac

    local status
    status=$(echo "$item" | cut -d'|' -f4)
    if [[ "$status" == "retry-pending" || "$status" == "pending" ]]; then
      pending_count=$((pending_count + 1))
    fi
  done

  local retry_status="pass"
  if [[ $escalate_count -gt 0 ]]; then
    retry_status="escalate"
  elif [[ $pending_count -gt 0 ]]; then
    retry_status="pending"
  fi

  cat <<EOF
# Archive Audit Closure Acceptance Retry Report

## Metadata

| Field | Value |
|-------|-------|
| retry_id | $RETRY_ID |
| generated_at | $TIMESTAMP |
| mode | $MODE |
| closure_gate_report | ${CLOSURE_GATE_REPORT:-n/a} |
| autofix_report | ${AUTOFIX_REPORT:-n/a} |
| verify_report | ${VERIFY_REPORT:-n/a} |
| max_retries | $MAX_RETRIES |
| retry_delay | ${RETRY_DELAY}s |
| escalate_threshold | $ESCALATE_THRESHOLD |

## Summary

| Metric | Value |
|--------|-------|
| total_items | $total |
| retry_items | $retry_count |
| escalate_items | $escalate_count |
| skip_items | $skip_count |
| pending_items | $pending_count |
| retry_status | $retry_status |

## Retry Actions

| item_id | priority | owner | status | retry_count | action |
|---------|----------|-------|--------|-------------|--------|
EOF

  for item in "${items[@]:-}"; do
    local item_id priority owner status retry_count action
    IFS='|' read -r item_id priority owner status retry_count <<< "$item"
    action=$(decide_action "$item")
    echo "| $item_id | $priority | $owner | $status | $retry_count | $action |"
  done

  if [[ ${#items[@]} -eq 0 ]]; then
    echo "| (none) | - | - | - | - | - |"
  fi

  cat <<EOF

## Escalation Queue

| item_id | priority | owner | reason |
|---------|----------|-------|--------|
EOF

  for item in "${items[@]:-}"; do
    local item_id priority owner status retry_count action
    IFS='|' read -r item_id priority owner status retry_count <<< "$item"
    action=$(decide_action "$item")
    if [[ "$action" == "escalate" ]]; then
      echo "| $item_id | $priority | $owner | max-retries-exceeded |"
    fi
  done

  cat <<EOF

## Next Steps

EOF

  if [[ "$retry_status" == "pass" ]]; then
    echo "- All items resolved or skipped."
    echo "- Proceed to release gate."
  elif [[ "$retry_status" == "escalate" ]]; then
    echo "- $escalate_count items require escalation."
    echo "- Contact responsible owners for manual resolution."
  else
    echo "- $pending_count items pending retry."
    echo "- Re-run with --apply to execute retries."
  fi

  cat <<EOF

## Release Advice

| Condition | Advice |
|-----------|--------|
| retry_status=pass | proceed-to-release |
| retry_status=pending | re-run-with-apply |
| retry_status=escalate | manual-intervention-required |
EOF
}

# ============================================================
# 主流程
# ============================================================
main() {
  # 提取失败项
  local failed_items=()
  if [[ -n "$CLOSURE_GATE_REPORT" && -f "$CLOSURE_GATE_REPORT" ]]; then
    while IFS= read -r item; do
      [[ -n "$item" ]] && failed_items+=("$item")
    done < <(extract_failed_items "$CLOSURE_GATE_REPORT")
  fi

  # 补充自动修复报告中的失败项
  if [[ -n "$AUTOFIX_REPORT" && -f "$AUTOFIX_REPORT" ]]; then
    while IFS= read -r item; do
      [[ -n "$item" ]] && failed_items+=("$item")
    done < <(extract_failed_items "$AUTOFIX_REPORT")
  fi

  # 执行重试模拟
  local processed_items=()
  for item in "${failed_items[@]:-}"; do
    result=$(simulate_retry "$item")
    processed_items+=("$result")
  done

  # 生成报告
  local report
  report=$(generate_report "${processed_items[@]:-}")

  if [[ -n "$OUTPUT" ]]; then
    echo "$report" > "$OUTPUT"
    echo "Report written to: $OUTPUT"
  else
    echo "$report"
  fi

  # 严格模式检查
  if [[ "$STRICT" == "true" ]]; then
    local unresolved=0
    for item in "${processed_items[@]:-}"; do
      local status
      status=$(echo "$item" | cut -d'|' -f4)
      if [[ "$status" != "pass" && "$status" != "closed" && "$status" != "waived" && "$status" != "skip" ]]; then
        unresolved=$((unresolved + 1))
      fi
    done
    if [[ $unresolved -gt 0 ]]; then
      echo "Strict mode: $unresolved items still unresolved"
      exit 1
    fi
  fi
}

main
