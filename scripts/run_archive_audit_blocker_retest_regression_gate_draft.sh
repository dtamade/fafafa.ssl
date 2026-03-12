#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

GATE_ID=""
CLOSURE_RECORD_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_SAMPLE_B36.md"
APPROVAL_CHAIN_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md"
RETEST_PASS_BLOCKERS=""
RETEST_WAIVE_BLOCKERS=""
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计阻断项重测与回归门禁脚本（Draft）

用途：
  基于关闭校验记录与签批链路，生成阻断项重测结论并给出回归门禁建议。

用法：
  scripts/run_archive_audit_blocker_retest_regression_gate_draft.sh [options]

选项：
  --gate-id ID                回归门禁 ID（默认: yyyyMMdd_HHmmss）
  --closure-record FILE       关闭校验记录（默认: docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_SAMPLE_B36.md）
  --approval-chain FILE       签批链路报告（默认: docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md）
  --retest-pass-blockers LIST 逗号分隔 blocker_code，标记为重测通过
  --retest-waive-blockers LIST 逗号分隔 blocker_code，标记为重测豁免
  --operator NAME             操作人/作业名（默认: codex）
  --output FILE               输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_<id>.md）
  --strict                    regression_gate_status 非 pass 时返回非 0
  --dry-run                   仅打印计划，不写文件
  --help                      显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --gate-id)
      GATE_ID="$2"
      shift 2
      ;;
    --closure-record)
      CLOSURE_RECORD_FILE="$2"
      shift 2
      ;;
    --approval-chain)
      APPROVAL_CHAIN_FILE="$2"
      shift 2
      ;;
    --retest-pass-blockers)
      RETEST_PASS_BLOCKERS="$2"
      shift 2
      ;;
    --retest-waive-blockers)
      RETEST_WAIVE_BLOCKERS="$2"
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

if [[ -z "$GATE_ID" ]]; then
  GATE_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_${GATE_ID}.md"
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

CLOSURE_RECORD_FILE="$(resolve_input_path "$CLOSURE_RECORD_FILE")"
APPROVAL_CHAIN_FILE="$(resolve_input_path "$APPROVAL_CHAIN_FILE")"
OUTPUT_FILE="$(resolve_output_path "$OUTPUT_FILE")"

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] gate_id=$GATE_ID"
  echo "[DRY-RUN] closure_record=$CLOSURE_RECORD_FILE"
  echo "[DRY-RUN] approval_chain=$APPROVAL_CHAIN_FILE"
  echo "[DRY-RUN] retest_pass_blockers=$RETEST_PASS_BLOCKERS"
  echo "[DRY-RUN] retest_waive_blockers=$RETEST_WAIVE_BLOCKERS"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$CLOSURE_RECORD_FILE" "$APPROVAL_CHAIN_FILE"; do
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

list_contains() {
  local csv_list="$1"
  local target="$2"

  [[ -z "$csv_list" ]] && return 1
  echo "$csv_list" | tr ',' '\n' | sed -E 's/^[[:space:]]+//; s/[[:space:]]+$//' | grep -Fxq "$target"
}

closure_status="$(trim "$(extract_metric "$CLOSURE_RECORD_FILE" "closure_status")")"
close_percent="$(trim "$(extract_metric "$CLOSURE_RECORD_FILE" "close_percent")")"
critical_unclosed="$(trim "$(extract_metric "$CLOSURE_RECORD_FILE" "critical_unclosed")")"
high_unclosed="$(trim "$(extract_metric "$CLOSURE_RECORD_FILE" "high_unclosed")")"

approval_status="$(trim "$(extract_metric "$APPROVAL_CHAIN_FILE" "approval_status")")"
approval_release_decision="$(trim "$(extract_metric "$APPROVAL_CHAIN_FILE" "release_decision")")"

[[ -z "$closure_status" ]] && closure_status="unknown"
[[ -z "$close_percent" ]] && close_percent="0%"
[[ -z "$critical_unclosed" ]] && critical_unclosed="0"
[[ -z "$high_unclosed" ]] && high_unclosed="0"
[[ -z "$approval_status" ]] && approval_status="unknown"
[[ -z "$approval_release_decision" ]] && approval_release_decision="unknown"

rows_file="$(mktemp)"
escalation_rows_file="$(mktemp)"
trap 'rm -f "$rows_file" "$escalation_rows_file"' EXIT

total_retest_items=0
retest_passed=0
retest_waived=0
retest_failed=0
retest_warn=0
open_critical=0
open_high=0

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 _ <<< "$row"

  blocker_code="$(trim "$c1")"
  severity="$(trim "$c2")"
  owner="$(trim "$c3")"
  action="$(trim "$c4")"
  execution_status="$(trim "$c5")"
  evidence="$(trim "$c6")"

  if [[ -z "$blocker_code" || "$blocker_code" == "none" || "$blocker_code" == "<BLK-001>" ]]; then
    continue
  fi

  total_retest_items=$((total_retest_items + 1))

  retest_status="fail"
  gate_signal="regression-risk"

  if list_contains "$RETEST_PASS_BLOCKERS" "$blocker_code"; then
    retest_status="pass"
    gate_signal="cleared-after-retest"
  elif list_contains "$RETEST_WAIVE_BLOCKERS" "$blocker_code"; then
    retest_status="waived"
    gate_signal="accepted-by-waiver"
  elif [[ "$severity" == "critical" || "$severity" == "high" ]]; then
    retest_status="fail"
    gate_signal="severity-${severity}-still-open"
  elif [[ "$execution_status" == "in-progress" ]]; then
    retest_status="warn"
    gate_signal="retest-in-progress"
  else
    retest_status="warn"
    gate_signal="manual-retest-required"
  fi

  case "$retest_status" in
    pass)
      retest_passed=$((retest_passed + 1))
      ;;
    waived)
      retest_waived=$((retest_waived + 1))
      ;;
    fail)
      retest_failed=$((retest_failed + 1))
      if [[ "$severity" == "critical" ]]; then
        open_critical=$((open_critical + 1))
      elif [[ "$severity" == "high" ]]; then
        open_high=$((open_high + 1))
      fi
      ;;
    warn)
      retest_warn=$((retest_warn + 1))
      ;;
    *)
      retest_warn=$((retest_warn + 1))
      ;;
  esac

  echo "$blocker_code|$severity|$owner|$action|$execution_status|$retest_status|$gate_signal|$evidence" >> "$rows_file"
done < <(extract_section_rows "$CLOSURE_RECORD_FILE" "6) Unclosed Items")

total_escalations=0
open_escalations=0

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 _ <<< "$row"

  stage_id="$(trim "$c1")"
  stage_name="$(trim "$c2")"
  stage_status="$(trim "$c3")"
  owner="$(trim "$c4")"
  trigger="$(trim "$c5")"
  required_action="$(trim "$c6")"

  if [[ -z "$stage_id" || "$stage_id" == "none" || "$stage_id" == "<S1>" ]]; then
    continue
  fi

  total_escalations=$((total_escalations + 1))
  if [[ "$stage_status" != "pass" ]]; then
    open_escalations=$((open_escalations + 1))
  fi

  echo "$stage_id|$stage_name|$stage_status|$owner|$trigger|$required_action" >> "$escalation_rows_file"
done < <(extract_section_rows "$APPROVAL_CHAIN_FILE" "5) Escalation Queue")

retest_coverage_percent=0
if (( total_retest_items > 0 )); then
  retest_coverage_percent=$(( (retest_passed + retest_waived) * 100 / total_retest_items ))
fi

regression_gate_status="pass"
release_advice="proceed-regression-gate-pass"

if (( retest_failed > 0 || open_escalations > 0 )) || [[ "$approval_status" == "fail" || "$closure_status" == "fail" ]]; then
  regression_gate_status="fail"
  release_advice="block-release-until-retest-and-escalation-cleared"
elif (( retest_warn > 0 )) || [[ "$approval_status" == "warn" || "$closure_status" == "warn" ]]; then
  regression_gate_status="warn"
  release_advice="proceed-with-conditional-regression-gate"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Blocker Retest & Regression Gate（Draft）

## 1) Metadata

| field | value |
|------|-------|
| gate_id | $GATE_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| closure_record_report | $CLOSURE_RECORD_FILE |
| approval_chain_report | $APPROVAL_CHAIN_FILE |
| retest_pass_blockers | ${RETEST_PASS_BLOCKERS:-none} |
| retest_waive_blockers | ${RETEST_WAIVE_BLOCKERS:-none} |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| closure_status | $closure_status |
| closure_close_percent | $close_percent |
| closure_critical_unclosed | $critical_unclosed |
| closure_high_unclosed | $high_unclosed |
| approval_status | $approval_status |
| approval_release_decision | $approval_release_decision |

## 3) Retest Summary

| metric | value |
|--------|-------|
| total_retest_items | $total_retest_items |
| retest_passed | $retest_passed |
| retest_waived | $retest_waived |
| retest_warn | $retest_warn |
| retest_failed | $retest_failed |
| retest_coverage_percent | ${retest_coverage_percent}% |
| open_critical_after_retest | $open_critical |
| open_high_after_retest | $open_high |
| escalation_open_count | $open_escalations |
| regression_gate_status | $regression_gate_status |
| release_advice | $release_advice |

## 4) Retest Rows

| blocker_code | severity | owner | action | previous_execution_status | retest_status | gate_signal | evidence |
|--------------|----------|-------|--------|---------------------------|---------------|-------------|----------|
EOF_REPORT

if [[ -s "$rows_file" ]]; then
  while IFS='|' read -r blocker_code severity owner action execution_status retest_status gate_signal evidence; do
    echo "| $blocker_code | $severity | $owner | $action | $execution_status | $retest_status | $gate_signal | $evidence |" >> "$OUTPUT_FILE"
  done < "$rows_file"
else
  echo "| none | n/a | n/a | none | done | pass | no-open-blockers | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Escalation Snapshot

| stage_id | stage_name | stage_status | owner | trigger | required_action |
|----------|------------|--------------|-------|---------|-----------------|
EOF_APPEND

if [[ -s "$escalation_rows_file" ]]; then
  while IFS='|' read -r stage_id stage_name stage_status owner trigger required_action; do
    echo "| $stage_id | $stage_name | $stage_status | $owner | $trigger | $required_action |" >> "$OUTPUT_FILE"
  done < "$escalation_rows_file"
else
  echo "| none | none | pass | none | none | none |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 6) Suggested Actions

- immediate:
  - $release_advice
- followup:
  - sync-retest-result-to-approval-chain-and-receipt
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$regression_gate_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass regression gate status: $regression_gate_status" >&2
  exit 1
fi

echo "[PASS] blocker retest regression gate generated"
