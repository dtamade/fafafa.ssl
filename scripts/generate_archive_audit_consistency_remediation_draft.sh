#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

PLAN_ID=""
CONSISTENCY_REPORT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_WEEKLY_CHECKLIST_CONSISTENCY_SAMPLE_B33.md"
CLOSURE_RECORD_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_SAMPLE_B36.md"
BLOCKERS_FILE="$PROJECT_ROOT/docs/test_reports/PRE_RELEASE_AUDIT_BLOCKERS_SAMPLE_B32.md"
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计一致性偏差修复建议生成脚本（Draft）

用途：
  聚合一致性报告、关闭校验与阻断清单，输出分级修复建议与优先级计划。

用法：
  scripts/generate_archive_audit_consistency_remediation_draft.sh [options]

选项：
  --plan-id ID              修复建议 ID（默认: yyyyMMdd_HHmmss）
  --consistency-report FILE 一致性报告（默认: docs/test_reports/ARCHIVE_AUDIT_WEEKLY_CHECKLIST_CONSISTENCY_SAMPLE_B33.md）
  --closure-record FILE     关闭校验记录（默认: docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_SAMPLE_B36.md）
  --blockers FILE           阻断项清单（默认: docs/test_reports/PRE_RELEASE_AUDIT_BLOCKERS_SAMPLE_B32.md）
  --operator NAME           操作人/作业名（默认: codex）
  --output FILE             输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_CONSISTENCY_REMEDIATION_<id>.md）
  --strict                  remediation_status 非 pass 时返回非 0
  --dry-run                 仅打印计划，不写文件
  --help                    显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --plan-id)
      PLAN_ID="$2"
      shift 2
      ;;
    --consistency-report)
      CONSISTENCY_REPORT_FILE="$2"
      shift 2
      ;;
    --closure-record)
      CLOSURE_RECORD_FILE="$2"
      shift 2
      ;;
    --blockers)
      BLOCKERS_FILE="$2"
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

if [[ -z "$PLAN_ID" ]]; then
  PLAN_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_CONSISTENCY_REMEDIATION_${PLAN_ID}.md"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] plan_id=$PLAN_ID"
  echo "[DRY-RUN] consistency_report=$CONSISTENCY_REPORT_FILE"
  echo "[DRY-RUN] closure_record=$CLOSURE_RECORD_FILE"
  echo "[DRY-RUN] blockers=$BLOCKERS_FILE"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$CONSISTENCY_REPORT_FILE" "$CLOSURE_RECORD_FILE" "$BLOCKERS_FILE"; do
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

to_int_or_zero() {
  local value="$1"
  if [[ "$value" =~ ^[0-9]+$ ]]; then
    echo "$value"
  else
    echo 0
  fi
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

consistency_status="$(trim "$(extract_metric "$CONSISTENCY_REPORT_FILE" "consistency_status")")"
critical_fail_count="$(to_int_or_zero "$(extract_metric "$CONSISTENCY_REPORT_FILE" "critical_fail_count")")"
warning_count="$(to_int_or_zero "$(extract_metric "$CONSISTENCY_REPORT_FILE" "warning_count")")"

closure_status="$(trim "$(extract_metric "$CLOSURE_RECORD_FILE" "closure_status")")"
critical_unclosed="$(to_int_or_zero "$(extract_metric "$CLOSURE_RECORD_FILE" "critical_unclosed")")"
high_unclosed="$(to_int_or_zero "$(extract_metric "$CLOSURE_RECORD_FILE" "high_unclosed")")"
close_percent_raw="$(trim "$(extract_metric "$CLOSURE_RECORD_FILE" "close_percent")")"

blockers_critical="$(to_int_or_zero "$(extract_metric "$BLOCKERS_FILE" "blockers_critical")")"
blockers_high="$(to_int_or_zero "$(extract_metric "$BLOCKERS_FILE" "blockers_high")")"
blockers_medium="$(to_int_or_zero "$(extract_metric "$BLOCKERS_FILE" "blockers_medium")")"
blockers_status="$(trim "$(extract_metric "$BLOCKERS_FILE" "blockers_status")")"

[[ -z "$consistency_status" ]] && consistency_status="unknown"
[[ -z "$closure_status" ]] && closure_status="unknown"
[[ -z "$blockers_status" ]] && blockers_status="unknown"
[[ -z "$close_percent_raw" ]] && close_percent_raw="0%"

actions_file="$(mktemp)"
trap 'rm -f "$actions_file"' EXIT

critical_actions=0
high_actions=0
medium_actions=0

add_action() {
  local priority="$1"
  local area="$2"
  local owner="$3"
  local target_window="$4"
  local suggestion="$5"
  local trigger="$6"

  echo "$priority|$area|$owner|$target_window|$suggestion|$trigger" >> "$actions_file"

  case "$priority" in
    critical) critical_actions=$((critical_actions + 1)) ;;
    high) high_actions=$((high_actions + 1)) ;;
    medium) medium_actions=$((medium_actions + 1)) ;;
    *) ;;
  esac
}

if [[ "$consistency_status" == "fail" ]] || (( critical_fail_count > 0 )); then
  add_action "critical" "weekly-checklist-alignment" "release-ops" "<4h" "修复周报与清单字段不一致并重跑一致性检查" "consistency_status=$consistency_status; critical_fail_count=$critical_fail_count"
elif [[ "$consistency_status" == "warn" ]] || (( warning_count > 0 )); then
  add_action "medium" "weekly-checklist-alignment" "release-ops" "1bd" "关闭 warning 级一致性偏差并复核" "consistency_status=$consistency_status; warning_count=$warning_count"
fi

if [[ "$closure_status" == "fail" ]] || (( critical_unclosed > 0 || high_unclosed > 0 )); then
  add_action "critical" "blocker-closure" "release-manager+secops" "<1h" "优先关闭 critical/high 未闭环阻断项" "closure_status=$closure_status; critical_unclosed=$critical_unclosed; high_unclosed=$high_unclosed"
elif [[ "$closure_status" == "warn" ]]; then
  add_action "high" "blocker-closure" "release-manager" "4h" "完成剩余阻断项闭环并补齐豁免依据" "closure_status=$closure_status; close_percent=$close_percent_raw"
fi

if [[ "$blockers_status" == "fail" ]] || (( blockers_critical > 0 || blockers_high > 0 )); then
  add_action "high" "risk-blocker-reduction" "qa-secops" "4h" "压降 high/critical blocker 数量并同步执行回执" "blockers_status=$blockers_status; critical=$blockers_critical; high=$blockers_high"
elif (( blockers_medium > 0 )); then
  add_action "medium" "risk-blocker-reduction" "qa-owner" "1bd" "跟踪 medium blocker 并制定清理计划" "medium=$blockers_medium"
fi

if (( critical_actions == 0 && high_actions == 0 && medium_actions == 0 )); then
  add_action "medium" "routine-review" "audit-owner" "next-weekly" "保持周度巡检并确认指标稳定" "no-significant-gaps-detected"
fi

remediation_status="pass"
release_guidance="proceed"

if (( critical_actions > 0 )); then
  remediation_status="fail"
  release_guidance="block-release-until-critical-actions-closed"
elif (( high_actions > 0 )); then
  remediation_status="fail"
  release_guidance="hold-release-until-high-actions-closed"
elif (( medium_actions > 0 )); then
  remediation_status="warn"
  release_guidance="proceed-with-remediation-tracking"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Consistency Gap Remediation Plan（Draft）

## 1) Metadata

| field | value |
|------|-------|
| plan_id | $PLAN_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| consistency_report | $CONSISTENCY_REPORT_FILE |
| closure_record | $CLOSURE_RECORD_FILE |
| blockers_report | $BLOCKERS_FILE |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| consistency_status | $consistency_status |
| consistency_critical_fail_count | $critical_fail_count |
| consistency_warning_count | $warning_count |
| closure_status | $closure_status |
| closure_critical_unclosed | $critical_unclosed |
| closure_high_unclosed | $high_unclosed |
| closure_close_percent | $close_percent_raw |
| blockers_status | $blockers_status |
| blockers_critical | $blockers_critical |
| blockers_high | $blockers_high |
| blockers_medium | $blockers_medium |

## 3) Remediation Summary

| metric | value |
|--------|-------|
| critical_actions | $critical_actions |
| high_actions | $high_actions |
| medium_actions | $medium_actions |
| remediation_status | $remediation_status |
| release_guidance | $release_guidance |

## 4) Recommended Actions

| priority | area | owner | target_window | suggestion | trigger |
|----------|------|-------|---------------|------------|---------|
EOF_REPORT

while IFS='|' read -r priority area owner target_window suggestion trigger; do
  echo "| $priority | $area | $owner | $target_window | $suggestion | $trigger |" >> "$OUTPUT_FILE"
done < "$actions_file"

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 5) Suggested Next Step

- immediate:
  - $release_guidance
- followup:
  - rerun-consistency-closure-after-remediation
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$remediation_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass remediation status: $remediation_status" >&2
  exit 1
fi

echo "[PASS] consistency gap remediation plan generated"
