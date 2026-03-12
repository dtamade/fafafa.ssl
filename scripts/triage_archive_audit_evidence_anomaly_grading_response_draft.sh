#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

RESPONSE_ID=""
AUDIT_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_APPROVAL_EVIDENCE_CONSISTENCY_SAMPLE_B43.md"
ADAPTIVE_POLICY_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_CONVERGENCE_ADAPTIVE_THRESHOLD_POLICY_SAMPLE_B45.md"
VERSIONING_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_SAMPLE_B46.md"
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计证据巡检异常分级处置脚本（Draft）

用途：
  聚合证据巡检、阈值自适应与版本化回滚结果，输出异常分级处置队列。

用法：
  scripts/triage_archive_audit_evidence_anomaly_grading_response_draft.sh [options]

选项：
  --response-id ID            处置 ID（默认: yyyyMMdd_HHmmss）
  --audit-report FILE         证据一致性报告（默认: B43 样例）
  --adaptive-policy FILE      阈值自适应策略报告（默认: B45 样例）
  --versioning-report FILE    回写版本化与回滚报告（默认: B46 样例）
  --operator NAME             操作人/作业名（默认: codex）
  --output FILE               输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_EVIDENCE_ANOMALY_GRADING_RESPONSE_<id>.md）
  --strict                    response_status 非 pass 时返回非 0
  --dry-run                   仅打印计划，不写文件
  --help                      显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --response-id)
      RESPONSE_ID="$2"
      shift 2
      ;;
    --audit-report)
      AUDIT_REPORT="$2"
      shift 2
      ;;
    --adaptive-policy)
      ADAPTIVE_POLICY_REPORT="$2"
      shift 2
      ;;
    --versioning-report)
      VERSIONING_REPORT="$2"
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

if [[ -z "$RESPONSE_ID" ]]; then
  RESPONSE_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_EVIDENCE_ANOMALY_GRADING_RESPONSE_${RESPONSE_ID}.md"
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

AUDIT_REPORT="$(resolve_input_path "$AUDIT_REPORT")"
ADAPTIVE_POLICY_REPORT="$(resolve_input_path "$ADAPTIVE_POLICY_REPORT")"
VERSIONING_REPORT="$(resolve_input_path "$VERSIONING_REPORT")"
OUTPUT_FILE="$(resolve_output_path "$OUTPUT_FILE")"

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] response_id=$RESPONSE_ID"
  echo "[DRY-RUN] audit_report=$AUDIT_REPORT"
  echo "[DRY-RUN] adaptive_policy=$ADAPTIVE_POLICY_REPORT"
  echo "[DRY-RUN] versioning_report=$VERSIONING_REPORT"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$AUDIT_REPORT" "$ADAPTIVE_POLICY_REPORT" "$VERSIONING_REPORT"; do
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

to_int_or_zero() {
  local value="$1"
  value="${value//%/}"
  if [[ "$value" =~ ^[0-9]+$ ]]; then
    echo "$value"
  else
    echo 0
  fi
}

normalize_status() {
  case "$1" in
    pass|warn|fail|unknown|review|stable|reinforce) echo "$1" ;;
    *) echo "unknown" ;;
  esac
}

severity_rank() {
  case "$1" in
    critical) echo 4 ;;
    high) echo 3 ;;
    medium) echo 2 ;;
    low) echo 1 ;;
    *) echo 0 ;;
  esac
}

severity_owner() {
  case "$1" in
    critical) echo "release-manager+secops" ;;
    high) echo "release-manager" ;;
    medium) echo "qa-owner" ;;
    low) echo "audit-owner" ;;
    *) echo "audit-owner" ;;
  esac
}

severity_sla() {
  case "$1" in
    critical) echo "<1h" ;;
    high) echo "4h" ;;
    medium) echo "1bd" ;;
    low) echo "next-weekly" ;;
    *) echo "next-weekly" ;;
  esac
}

audit_status="$(normalize_status "$(trim "$(extract_metric "$AUDIT_REPORT" "audit_status")")")"
audit_checks_fail="$(to_int_or_zero "$(extract_metric "$AUDIT_REPORT" "checks_fail")")"
audit_release_advice="$(trim "$(extract_metric "$AUDIT_REPORT" "release_advice")")"

adaptive_status="$(normalize_status "$(trim "$(extract_metric "$ADAPTIVE_POLICY_REPORT" "adaptive_status")")")"
adaptation_mode="$(trim "$(extract_metric "$ADAPTIVE_POLICY_REPORT" "adaptation_mode")")"
pressure_score="$(to_int_or_zero "$(extract_metric "$ADAPTIVE_POLICY_REPORT" "pressure_score")")"
adaptive_release_guidance="$(trim "$(extract_metric "$ADAPTIVE_POLICY_REPORT" "release_guidance")")"

versioning_status="$(normalize_status "$(trim "$(extract_metric "$VERSIONING_REPORT" "versioning_status")")")"
rollback_candidates="$(to_int_or_zero "$(extract_metric "$VERSIONING_REPORT" "rollback_candidates")")"
versioning_release_advice="$(trim "$(extract_metric "$VERSIONING_REPORT" "release_advice")")"

rows_file="$(mktemp)"
queue_file="$(mktemp)"
trap 'rm -f "$rows_file" "$queue_file"' EXIT

anomalies_total=0
critical_count=0
high_count=0
medium_count=0
low_count=0

add_row() {
  local anomaly_id="$1"
  local source="$2"
  local severity="$3"
  local anomaly_key="$4"
  local observed="$5"
  local disposition="$6"
  local response_action="$7"
  local evidence="$8"

  local owner sla
  owner="$(severity_owner "$severity")"
  sla="$(severity_sla "$severity")"

  anomalies_total=$((anomalies_total + 1))
  case "$severity" in
    critical) critical_count=$((critical_count + 1)) ;;
    high) high_count=$((high_count + 1)) ;;
    medium) medium_count=$((medium_count + 1)) ;;
    low) low_count=$((low_count + 1)) ;;
  esac

  echo "$anomaly_id|$source|$severity|$owner|$sla|$anomaly_key|$observed|$disposition|$response_action|$evidence" >> "$rows_file"

  if [[ "$severity" == "critical" || "$severity" == "high" || "$disposition" == "queued" ]]; then
    echo "$anomaly_id|$severity|$owner|$sla|$response_action|$disposition" >> "$queue_file"
  fi
}

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 _ <<< "$row"

  check_id="$(trim "$c1")"
  area="$(trim "$c2")"
  severity_raw="$(trim "$c3")"
  note="$(trim "$c4")"
  observed="$(trim "$c5")"

  if [[ -z "$check_id" || "$check_id" == "none" || "$check_id" == "<check_id>" ]]; then
    continue
  fi

  severity="medium"
  case "$severity_raw" in
    fail) severity="high" ;;
    warn) severity="medium" ;;
    pass) severity="low" ;;
    *) severity="medium" ;;
  esac

  case "$area" in
    writeback-change-coverage) severity="critical" ;;
    convergence-index) severity="high" ;;
  esac

  add_row "AUD-${check_id}" "evidence_consistency" "$severity" "$area" "$observed" "open" "fix-${area}-and-rerun-audit" "$note"
done < <(extract_section_rows "$AUDIT_REPORT" "5) Mismatch Queue")

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 _ <<< "$row"

  check_id="$(trim "$c1")"
  observed="$(trim "$c2")"
  result="$(normalize_status "$(trim "$c4")")"

  if [[ -z "$check_id" || "$check_id" == "none" || "$check_id" == "<check-1>" ]]; then
    continue
  fi

  if [[ "$result" == "pass" || "$result" == "stable" ]]; then
    continue
  fi

  severity="medium"
  case "$result" in
    fail) severity="high" ;;
    review|reinforce|warn) severity="medium" ;;
    *) severity="medium" ;;
  esac

  add_row "POL-${check_id}" "adaptive_policy" "$severity" "$check_id" "$observed" "open" "resolve-policy-check-${check_id}" "result=${result}"
done < <(extract_section_rows "$ADAPTIVE_POLICY_REPORT" "5) Decision Queue")

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 _ <<< "$row"

  blocker_code="$(trim "$c1")"
  current_status="$(trim "$c2")"
  rollback_version="$(trim "$c3")"
  rollback_reason="$(trim "$c4")"
  note="$(trim "$c5")"

  if [[ -z "$blocker_code" || "$blocker_code" == "none" || "$blocker_code" == "<BLK-001>" ]]; then
    continue
  fi

  severity="high"
  case "$current_status" in
    pending) severity="high" ;;
    in-progress) severity="medium" ;;
    unknown) severity="critical" ;;
    *) severity="medium" ;;
  esac

  add_row "RB-${blocker_code}" "versioning_rollback" "$severity" "$rollback_reason" "status=${current_status}; rollback=${rollback_version}" "queued" "execute-rollback-${rollback_version}-for-${blocker_code}" "$note"
done < <(extract_section_rows "$VERSIONING_REPORT" "5) Rollback Queue")

response_status="pass"
release_advice="proceed-with-standard-audit-cycle"

if (( critical_count > 0 )); then
  response_status="fail"
  release_advice="block-release-and-run-critical-anomaly-playbook"
elif (( high_count > 0 )); then
  response_status="fail"
  release_advice="block-release-until-high-anomalies-cleared"
elif (( medium_count > 0 )); then
  response_status="warn"
  release_advice="proceed-with-mitigation-and-watchlist"
fi

if (( anomalies_total == 0 )); then
  release_advice="no-anomaly-detected"
fi

if [[ "$audit_status" == "fail" || "$adaptive_status" == "fail" || "$versioning_status" == "fail" ]]; then
  response_status="fail"
fi

critical_high_open=$((critical_count + high_count))

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Evidence Anomaly Grading & Response（Draft）

## 1) Metadata

| field | value |
|------|-------|
| response_id | $RESPONSE_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| audit_report | $AUDIT_REPORT |
| adaptive_policy_report | $ADAPTIVE_POLICY_REPORT |
| versioning_report | $VERSIONING_REPORT |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| audit_status | $audit_status |
| audit_checks_fail | $audit_checks_fail |
| audit_release_advice | $audit_release_advice |
| adaptive_status | $adaptive_status |
| adaptation_mode | $adaptation_mode |
| pressure_score | $pressure_score |
| adaptive_release_guidance | $adaptive_release_guidance |
| versioning_status | $versioning_status |
| rollback_candidates | $rollback_candidates |
| versioning_release_advice | $versioning_release_advice |

## 3) Grading Summary

| metric | value |
|--------|-------|
| anomalies_total | $anomalies_total |
| critical_count | $critical_count |
| high_count | $high_count |
| medium_count | $medium_count |
| low_count | $low_count |
| critical_high_open | $critical_high_open |
| response_status | $response_status |
| release_advice | $release_advice |

## 4) Anomaly Rows

| anomaly_id | source | severity | owner | sla | anomaly_key | observed | disposition | response_action | evidence |
|------------|--------|----------|-------|-----|-------------|----------|-------------|-----------------|----------|
EOF_REPORT

if [[ -s "$rows_file" ]]; then
  while IFS='|' read -r anomaly_id source severity owner sla anomaly_key observed disposition response_action evidence; do
    echo "| $anomaly_id | $source | $severity | $owner | $sla | $anomaly_key | $observed | $disposition | $response_action | $evidence |" >> "$OUTPUT_FILE"
  done < "$rows_file"
else
  echo "| none | n/a | low | audit-owner | next-weekly | none | n/a | closed | no-action | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Response Queue

| anomaly_id | severity | owner | sla | immediate_action | status |
|------------|----------|-------|-----|------------------|--------|
EOF_APPEND

if [[ -s "$queue_file" ]]; then
  while IFS='|' read -r anomaly_id severity owner sla immediate_action status; do
    echo "| $anomaly_id | $severity | $owner | $sla | $immediate_action | $status |" >> "$OUTPUT_FILE"
  done < "$queue_file"
else
  echo "| none | n/a | audit-owner | next-weekly | no-action | closed |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 6) Suggested Actions

- immediate:
  - $release_advice
- followup:
  - rerun-evidence-anomaly-triage-after-writeback-remediation
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$response_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass response status: $response_status" >&2
  exit 1
fi

echo "[PASS] evidence anomaly grading response generated"
