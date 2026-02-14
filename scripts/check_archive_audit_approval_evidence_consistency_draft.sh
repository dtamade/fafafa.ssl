#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

AUDIT_ID=""
APPROVAL_CHAIN_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md"
RETEST_GATE_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md"
WRITEBACK_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md"
CONVERGENCE_DASHBOARD_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_MULTIWEEK_RISK_CONVERGENCE_DASHBOARD_SAMPLE_B41.md"
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计签批证据归档一致性巡检脚本（Draft）

用途：
  校验 approval/retest/writeback/convergence 四类证据在状态与关键计数上的一致性。

用法：
  scripts/check_archive_audit_approval_evidence_consistency_draft.sh [options]

选项：
  --audit-id ID               巡检 ID（默认: yyyyMMdd_HHmmss）
  --approval-chain FILE       签批链路报告（默认: docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md）
  --retest-gate FILE          重测门禁报告（默认: docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md）
  --writeback FILE            回写报告（默认: docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md）
  --convergence-dashboard FILE 多周收敛看板（默认: docs/test_reports/ARCHIVE_AUDIT_MULTIWEEK_RISK_CONVERGENCE_DASHBOARD_SAMPLE_B41.md）
  --operator NAME             操作人/作业名（默认: codex）
  --output FILE               输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_APPROVAL_EVIDENCE_CONSISTENCY_<id>.md）
  --strict                    audit_status 非 pass 时返回非 0
  --dry-run                   仅打印计划，不写文件
  --help                      显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --audit-id)
      AUDIT_ID="$2"
      shift 2
      ;;
    --approval-chain)
      APPROVAL_CHAIN_FILE="$2"
      shift 2
      ;;
    --retest-gate)
      RETEST_GATE_FILE="$2"
      shift 2
      ;;
    --writeback)
      WRITEBACK_FILE="$2"
      shift 2
      ;;
    --convergence-dashboard)
      CONVERGENCE_DASHBOARD_FILE="$2"
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

if [[ -z "$AUDIT_ID" ]]; then
  AUDIT_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_APPROVAL_EVIDENCE_CONSISTENCY_${AUDIT_ID}.md"
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

APPROVAL_CHAIN_FILE="$(resolve_input_path "$APPROVAL_CHAIN_FILE")"
RETEST_GATE_FILE="$(resolve_input_path "$RETEST_GATE_FILE")"
WRITEBACK_FILE="$(resolve_input_path "$WRITEBACK_FILE")"
CONVERGENCE_DASHBOARD_FILE="$(resolve_input_path "$CONVERGENCE_DASHBOARD_FILE")"
OUTPUT_FILE="$(resolve_output_path "$OUTPUT_FILE")"

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] audit_id=$AUDIT_ID"
  echo "[DRY-RUN] approval_chain=$APPROVAL_CHAIN_FILE"
  echo "[DRY-RUN] retest_gate=$RETEST_GATE_FILE"
  echo "[DRY-RUN] writeback=$WRITEBACK_FILE"
  echo "[DRY-RUN] convergence_dashboard=$CONVERGENCE_DASHBOARD_FILE"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$APPROVAL_CHAIN_FILE" "$RETEST_GATE_FILE" "$WRITEBACK_FILE" "$CONVERGENCE_DASHBOARD_FILE"; do
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

percent_to_int() {
  local value="$1"
  value="${value%%%}"
  value="$(trim "$value")"
  to_int_or_zero "$value"
}

approval_status="$(trim "$(extract_metric "$APPROVAL_CHAIN_FILE" "approval_status")")"
approval_release_decision="$(trim "$(extract_metric "$APPROVAL_CHAIN_FILE" "release_decision")")"
approval_rejected_stages="$(to_int_or_zero "$(extract_metric "$APPROVAL_CHAIN_FILE" "rejected_stages")")"

retest_status="$(trim "$(extract_metric "$RETEST_GATE_FILE" "regression_gate_status")")"
retest_release_advice="$(trim "$(extract_metric "$RETEST_GATE_FILE" "release_advice")")"
retest_failed="$(to_int_or_zero "$(extract_metric "$RETEST_GATE_FILE" "retest_failed")")"
retest_open_critical="$(to_int_or_zero "$(extract_metric "$RETEST_GATE_FILE" "open_critical_after_retest")")"

writeback_status="$(trim "$(extract_metric "$WRITEBACK_FILE" "writeback_status")")"
writeback_release_advice="$(trim "$(extract_metric "$WRITEBACK_FILE" "release_advice")")"
writeback_pending_items="$(to_int_or_zero "$(extract_metric "$WRITEBACK_FILE" "writeback_pending_items")")"
writeback_open_critical="$(to_int_or_zero "$(extract_metric "$WRITEBACK_FILE" "open_critical_items")")"
writeback_changed_items="$(to_int_or_zero "$(extract_metric "$WRITEBACK_FILE" "writeback_changed_items")")"
writeback_signaled_items="$(to_int_or_zero "$(extract_metric "$WRITEBACK_FILE" "retest_signaled_items")")"
writeback_close_percent="$(percent_to_int "$(extract_metric "$WRITEBACK_FILE" "writeback_close_percent")")"

convergence_status="$(trim "$(extract_metric "$CONVERGENCE_DASHBOARD_FILE" "risk_convergence_status")")"
convergence_guidance="$(trim "$(extract_metric "$CONVERGENCE_DASHBOARD_FILE" "release_guidance")")"
convergence_index="$(percent_to_int "$(extract_metric "$CONVERGENCE_DASHBOARD_FILE" "convergence_index")")"
trend_alerts="$(to_int_or_zero "$(extract_metric "$CONVERGENCE_DASHBOARD_FILE" "trend_alerts")")"

for status_var in approval_status retest_status writeback_status convergence_status; do
  value="${!status_var}"
  [[ -z "$value" ]] && eval "$status_var=unknown"
done

[[ -z "$approval_release_decision" ]] && approval_release_decision="unknown"
[[ -z "$retest_release_advice" ]] && retest_release_advice="unknown"
[[ -z "$writeback_release_advice" ]] && writeback_release_advice="unknown"
[[ -z "$convergence_guidance" ]] && convergence_guidance="unknown"

checks_file="$(mktemp)"
mismatch_file="$(mktemp)"
trap 'rm -f "$checks_file" "$mismatch_file"' EXIT

check_total=0
check_pass=0
check_warn=0
check_fail=0

add_check() {
  local check_id="$1"
  local area="$2"
  local expected="$3"
  local observed="$4"
  local check_status="$5"
  local note="$6"

  check_total=$((check_total + 1))
  case "$check_status" in
    pass) check_pass=$((check_pass + 1)) ;;
    warn) check_warn=$((check_warn + 1)) ;;
    fail) check_fail=$((check_fail + 1)) ;;
    *) check_warn=$((check_warn + 1)); check_status="warn" ;;
  esac

  echo "$check_id|$area|$expected|$observed|$check_status|$note" >> "$checks_file"
  if [[ "$check_status" != "pass" ]]; then
    echo "$check_id|$area|$check_status|$note|$observed" >> "$mismatch_file"
  fi
}

check_status="pass"
note="approval/retest aligned"
if [[ "$approval_status" == "fail" && "$retest_status" == "pass" ]]; then
  check_status="fail"
  note="approval fail but retest shows pass"
elif [[ "$approval_status" == "warn" && "$retest_status" == "pass" ]]; then
  check_status="warn"
  note="approval warn but retest pass, need manual confirmation"
fi
add_check "C01" "approval-vs-retest" "approval fail should not pair with retest pass" "approval=$approval_status; reteset=$retest_status" "$check_status" "$note"

check_status="pass"
note="retest/writeback pending aligned"
if (( retest_failed > 0 && writeback_pending_items == 0 )); then
  check_status="fail"
  note="retest_failed > 0 but writeback pending is 0"
fi
add_check "C02" "retest-to-writeback" "retest_failed>0 implies writeback_pending_items>0" "retest_failed=$retest_failed; writeback_pending=$writeback_pending_items" "$check_status" "$note"

check_status="pass"
note="close percent and pending count aligned"
if (( writeback_pending_items > 0 && writeback_close_percent == 100 )); then
  check_status="fail"
  note="pending items exist but close percent is 100%"
fi
add_check "C03" "writeback-close-metric" "pending>0 should not have close_percent=100" "pending=$writeback_pending_items; close_percent=${writeback_close_percent}%" "$check_status" "$note"

check_status="pass"
note="convergence reflects upstream risk"
if [[ "$convergence_status" == "pass" && ( "$approval_status" == "fail" || "$retest_status" == "fail" || "$writeback_status" == "fail" ) ]]; then
  check_status="fail"
  note="upstream fail exists but convergence marked pass"
elif [[ "$convergence_status" == "warn" && "$approval_status" == "fail" && "$retest_status" == "fail" && "$writeback_status" == "fail" ]]; then
  check_status="warn"
  note="all upstream fail, convergence should be fail"
fi
add_check "C04" "upstream-vs-convergence" "upstream fail should not yield convergence pass" "approval=$approval_status; retest=$retest_status; writeback=$writeback_status; convergence=$convergence_status" "$check_status" "$note"

check_status="pass"
note="writeback advice aligned"
if [[ "$writeback_status" == "fail" && "$writeback_release_advice" != *block* ]]; then
  check_status="warn"
  note="writeback fail but advice does not include block"
fi
add_check "C05" "writeback-advice" "writeback fail should provide block-oriented advice" "writeback_status=$writeback_status; advice=$writeback_release_advice" "$check_status" "$note"

check_status="pass"
note="writeback change coverage acceptable"
if (( writeback_signaled_items > 0 && writeback_changed_items == 0 )); then
  check_status="fail"
  note="all signaled items kept unchanged"
elif (( writeback_signaled_items > 0 && writeback_changed_items * 100 / writeback_signaled_items < 20 )); then
  check_status="warn"
  note="changed ratio below 20%"
fi
add_check "C06" "writeback-change-coverage" "signaled writeback should produce actionable changes" "signaled=$writeback_signaled_items; changed=$writeback_changed_items" "$check_status" "$note"

check_status="pass"
note="critical open counts aligned"
if (( retest_open_critical != writeback_open_critical )); then
  check_status="warn"
  note="critical open counts differ between retest and writeback"
fi
add_check "C07" "critical-open-parity" "open critical counts should align" "retest_open_critical=$retest_open_critical; writeback_open_critical=$writeback_open_critical" "$check_status" "$note"

check_status="pass"
note="convergence index should support release"
if (( convergence_index < 60 )); then
  check_status="fail"
  note="convergence index below readiness baseline"
fi
add_check "C08" "convergence-index" "convergence_index >= 60" "convergence_index=${convergence_index}%; trend_alerts=$trend_alerts" "$check_status" "$note"

audit_status="pass"
release_advice="proceed-with-evidence-archive-consistency"

if (( check_fail > 0 )); then
  audit_status="fail"
  release_advice="block-release-until-evidence-consistency-restored"
elif (( check_warn > 0 )); then
  audit_status="warn"
  release_advice="proceed-with-manual-consistency-review"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Approval Evidence Consistency Audit（Draft）

## 1) Metadata

| field | value |
|------|-------|
| audit_id | $AUDIT_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| approval_chain_report | $APPROVAL_CHAIN_FILE |
| retest_gate_report | $RETEST_GATE_FILE |
| writeback_report | $WRITEBACK_FILE |
| convergence_dashboard_report | $CONVERGENCE_DASHBOARD_FILE |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| approval_status | $approval_status |
| approval_release_decision | $approval_release_decision |
| approval_rejected_stages | $approval_rejected_stages |
| retest_status | $retest_status |
| retest_failed | $retest_failed |
| retest_release_advice | $retest_release_advice |
| writeback_status | $writeback_status |
| writeback_pending_items | $writeback_pending_items |
| writeback_close_percent | ${writeback_close_percent}% |
| writeback_release_advice | $writeback_release_advice |
| convergence_status | $convergence_status |
| convergence_index | ${convergence_index}% |
| convergence_guidance | $convergence_guidance |

## 3) Audit Summary

| metric | value |
|--------|-------|
| checks_total | $check_total |
| checks_pass | $check_pass |
| checks_warn | $check_warn |
| checks_fail | $check_fail |
| audit_status | $audit_status |
| release_advice | $release_advice |

## 4) Check Rows

| check_id | area | expected | observed | check_status | note |
|----------|------|----------|----------|--------------|------|
EOF_REPORT

while IFS='|' read -r check_id area expected observed check_status_row note; do
  echo "| $check_id | $area | $expected | $observed | $check_status_row | $note |" >> "$OUTPUT_FILE"
done < "$checks_file"

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Mismatch Queue

| check_id | area | severity | note | observed |
|----------|------|----------|------|----------|
EOF_APPEND

if [[ -s "$mismatch_file" ]]; then
  while IFS='|' read -r check_id area severity note observed; do
    echo "| $check_id | $area | $severity | $note | $observed |" >> "$OUTPUT_FILE"
  done < "$mismatch_file"
else
  echo "| none | none | pass | no-mismatch | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 6) Suggested Actions

- immediate:
  - $release_advice
- followup:
  - rerun-consistency-audit-after-writeback-update
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$audit_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass audit status: $audit_status" >&2
  exit 1
fi

echo "[PASS] approval evidence consistency audit generated"
