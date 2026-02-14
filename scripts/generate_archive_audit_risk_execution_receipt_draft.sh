#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RECEIPT_ID=""
RISK_MATRIX_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_RISK_RESPONSE_SAMPLE_B31.md"
BLOCKERS_FILE="$PROJECT_ROOT/docs/test_reports/PRE_RELEASE_AUDIT_BLOCKERS_SAMPLE_B32.md"
THRESHOLD_POLICY_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_DASHBOARD_THRESHOLD_POLICY_SAMPLE_B34.md"
CLOSE_BLOCKERS=""
WAIVE_BLOCKERS=""
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计风险响应执行回执生成脚本（Draft）

用途：
  汇总风险矩阵、阻断项与阈值策略，输出执行回执与闭环状态。

用法：
  scripts/generate_archive_audit_risk_execution_receipt_draft.sh [options]

选项：
  --receipt-id ID          回执 ID（默认: yyyyMMdd_HHmmss）
  --risk-matrix FILE       风险矩阵报告（默认: docs/test_reports/ARCHIVE_AUDIT_RISK_RESPONSE_SAMPLE_B31.md）
  --blockers FILE          阻断项报告（默认: docs/test_reports/PRE_RELEASE_AUDIT_BLOCKERS_SAMPLE_B32.md）
  --threshold-policy FILE  阈值策略报告（默认: docs/test_reports/ARCHIVE_AUDIT_DASHBOARD_THRESHOLD_POLICY_SAMPLE_B34.md）
  --close-blockers LIST    逗号分隔 blocker_code，标记为 done
  --waive-blockers LIST    逗号分隔 blocker_code，标记为 waived
  --operator NAME          操作人/作业名（默认: codex）
  --output FILE            输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_<id>.md）
  --strict                 execution_readiness 非 pass 时返回非 0
  --dry-run                仅打印计划，不写文件
  --help                   显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --receipt-id)
      RECEIPT_ID="$2"
      shift 2
      ;;
    --risk-matrix)
      RISK_MATRIX_FILE="$2"
      shift 2
      ;;
    --blockers)
      BLOCKERS_FILE="$2"
      shift 2
      ;;
    --threshold-policy)
      THRESHOLD_POLICY_FILE="$2"
      shift 2
      ;;
    --close-blockers)
      CLOSE_BLOCKERS="$2"
      shift 2
      ;;
    --waive-blockers)
      WAIVE_BLOCKERS="$2"
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

if [[ -z "$RECEIPT_ID" ]]; then
  RECEIPT_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_RISK_EXECUTION_RECEIPT_${RECEIPT_ID}.md"
fi

resolve_input_path() {
  local path="$1"

  if [[ "$path" == /* ]]; then
    echo "$path"
    return
  fi

  if [[ -f "$path" ]]; then
    echo "$path"
    return
  fi

  if [[ -f "$PROJECT_ROOT/$path" ]]; then
    echo "$PROJECT_ROOT/$path"
    return
  fi

  echo "$path"
}

resolve_output_path() {
  local path="$1"

  if [[ "$path" == /* ]]; then
    echo "$path"
  else
    echo "$PROJECT_ROOT/$path"
  fi
}

RISK_MATRIX_FILE="$(resolve_input_path "$RISK_MATRIX_FILE")"
BLOCKERS_FILE="$(resolve_input_path "$BLOCKERS_FILE")"
THRESHOLD_POLICY_FILE="$(resolve_input_path "$THRESHOLD_POLICY_FILE")"
OUTPUT_FILE="$(resolve_output_path "$OUTPUT_FILE")"

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] receipt_id=$RECEIPT_ID"
  echo "[DRY-RUN] risk_matrix=$RISK_MATRIX_FILE"
  echo "[DRY-RUN] blockers=$BLOCKERS_FILE"
  echo "[DRY-RUN] threshold_policy=$THRESHOLD_POLICY_FILE"
  echo "[DRY-RUN] close_blockers=$CLOSE_BLOCKERS"
  echo "[DRY-RUN] waive_blockers=$WAIVE_BLOCKERS"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$RISK_MATRIX_FILE" "$BLOCKERS_FILE" "$THRESHOLD_POLICY_FILE"; do
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

list_contains() {
  local csv_list="$1"
  local target="$2"

  [[ -z "$csv_list" ]] && return 1
  echo "$csv_list" | tr ',' '\n' | sed -E 's/^[[:space:]]+//; s/[[:space:]]+$//' | grep -Fxq "$target"
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

risk_overall="$(trim "$(extract_metric "$RISK_MATRIX_FILE" "overall_risk")")"
risk_decision="$(trim "$(extract_metric "$RISK_MATRIX_FILE" "decision_status")")"
risk_release_advice="$(trim "$(extract_metric "$RISK_MATRIX_FILE" "release_advice")")"

threshold_escalation="$(trim "$(extract_metric "$THRESHOLD_POLICY_FILE" "escalation_level")")"
threshold_decision="$(trim "$(extract_metric "$THRESHOLD_POLICY_FILE" "decision_status")")"
threshold_release_policy="$(trim "$(extract_metric "$THRESHOLD_POLICY_FILE" "release_policy")")"

[[ -z "$risk_overall" ]] && risk_overall="unknown"
[[ -z "$risk_decision" ]] && risk_decision="unknown"
[[ -z "$risk_release_advice" ]] && risk_release_advice="unknown"
[[ -z "$threshold_escalation" ]] && threshold_escalation="unknown"
[[ -z "$threshold_decision" ]] && threshold_decision="unknown"
[[ -z "$threshold_release_policy" ]] && threshold_release_policy="unknown"

rows_file="$(mktemp)"
unresolved_file="$(mktemp)"
trap 'rm -f "$rows_file" "$unresolved_file"' EXIT

total_items=0
done_count=0
waived_count=0
pending_count=0
inprogress_count=0
open_critical=0
open_high=0

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 c7 _ <<< "$row"

  blocker_code="$(trim "$c1")"
  source="$(trim "$c2")"
  blocker_key="$(trim "$c3")"
  severity="$(trim "$c4")"
  owner="$(trim "$c5")"
  action="$(trim "$c6")"
  evidence="$(trim "$c7")"

  [[ -z "$blocker_code" || "$blocker_code" == "<BLK-001>" ]] && continue

  total_items=$((total_items + 1))

  execution_status="pending"
  receipt_note="auto-open"

  if list_contains "$CLOSE_BLOCKERS" "$blocker_code"; then
    execution_status="done"
    receipt_note="manually-closed"
  elif list_contains "$WAIVE_BLOCKERS" "$blocker_code"; then
    execution_status="waived"
    receipt_note="manually-waived"
  else
    case "$severity" in
      critical|high)
        execution_status="pending"
        receipt_note="requires-owner-action"
        ;;
      medium)
        execution_status="in-progress"
        receipt_note="mitigation-in-progress"
        ;;
      low)
        execution_status="done"
        receipt_note="monitor-only-acknowledged"
        ;;
      *)
        execution_status="pending"
        receipt_note="unknown-severity-manual-review"
        ;;
    esac
  fi

  case "$execution_status" in
    done) done_count=$((done_count + 1)) ;;
    waived) waived_count=$((waived_count + 1)) ;;
    in-progress) inprogress_count=$((inprogress_count + 1)) ;;
    pending) pending_count=$((pending_count + 1)) ;;
    *) pending_count=$((pending_count + 1)) ;;
  esac

  if [[ "$execution_status" == "pending" || "$execution_status" == "in-progress" ]]; then
    case "$severity" in
      critical)
        open_critical=$((open_critical + 1))
        ;;
      high)
        open_high=$((open_high + 1))
        ;;
      *) ;;
    esac
    echo "$blocker_code|$severity|$owner|$action|$execution_status|$evidence" >> "$unresolved_file"
  fi

  target_sla="$(severity_to_sla "$severity")"
  echo "$blocker_code|$source|$blocker_key|$severity|$owner|$target_sla|$action|$execution_status|$receipt_note|$evidence" >> "$rows_file"
done < <(extract_section_rows "$BLOCKERS_FILE" "4) Blocker Items")

execution_readiness="pass"
release_decision="proceed"

if (( open_critical > 0 )); then
  execution_readiness="fail"
  release_decision="block-release"
elif (( open_high > 0 || pending_count > 0 )); then
  execution_readiness="fail"
  release_decision="hold-until-critical-high-cleared"
elif (( inprogress_count > 0 )); then
  execution_readiness="warn"
  release_decision="proceed-with-mitigation"
fi

completion_percent=0
if (( total_items > 0 )); then
  completion_percent=$(( (done_count + waived_count) * 100 / total_items ))
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Risk Response Execution Receipt（Draft）

## 1) Metadata

| field | value |
|------|-------|
| receipt_id | $RECEIPT_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| risk_matrix_report | $RISK_MATRIX_FILE |
| blockers_report | $BLOCKERS_FILE |
| threshold_policy_report | $THRESHOLD_POLICY_FILE |
| close_blockers | ${CLOSE_BLOCKERS:-none} |
| waive_blockers | ${WAIVE_BLOCKERS:-none} |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| risk_overall | $risk_overall |
| risk_decision_status | $risk_decision |
| risk_release_advice | $risk_release_advice |
| threshold_escalation_level | $threshold_escalation |
| threshold_decision_status | $threshold_decision |
| threshold_release_policy | $threshold_release_policy |

## 3) Execution Summary

| metric | value |
|--------|-------|
| total_items | $total_items |
| done_items | $done_count |
| waived_items | $waived_count |
| inprogress_items | $inprogress_count |
| pending_items | $pending_count |
| open_critical_items | $open_critical |
| open_high_items | $open_high |
| completion_percent | ${completion_percent}% |
| execution_readiness | $execution_readiness |
| release_decision | $release_decision |

## 4) Execution Receipt Rows

| blocker_code | source | blocker_key | severity | owner | target_sla | action | execution_status | receipt_note | evidence |
|--------------|--------|-------------|----------|-------|------------|--------|------------------|--------------|----------|
EOF_REPORT

if [[ -s "$rows_file" ]]; then
  while IFS='|' read -r blocker_code source blocker_key severity owner target_sla action execution_status receipt_note evidence; do
    echo "| $blocker_code | $source | $blocker_key | $severity | $owner | $target_sla | $action | $execution_status | $receipt_note | $evidence |" >> "$OUTPUT_FILE"
  done < "$rows_file"
else
  echo "| n/a | n/a | none | low | n/a | next-weekly | none | done | no-blockers | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Unresolved Items

| blocker_code | severity | owner | action | execution_status | evidence |
|--------------|----------|-------|--------|------------------|----------|
EOF_APPEND

if [[ -s "$unresolved_file" ]]; then
  while IFS='|' read -r blocker_code severity owner action execution_status evidence; do
    echo "| $blocker_code | $severity | $owner | $action | $execution_status | $evidence |" >> "$OUTPUT_FILE"
  done < "$unresolved_file"
else
  echo "| none | n/a | n/a | none | done | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 6) Suggested Follow-up

- immediate:
  - $release_decision
- followup:
  - update-risk-response-and-sync-blocker-status
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$execution_readiness" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass execution readiness: $execution_readiness" >&2
  exit 1
fi

echo "[PASS] risk response execution receipt generated"
