#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

ALERT_ID=""
APPROVAL_CHAIN_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md"
ANOMALY_RESPONSE_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_EVIDENCE_ANOMALY_GRADING_RESPONSE_SAMPLE_B47.md"
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计签批链路 SLA 违约预警脚本（Draft）

用途：
  联合签批链路与异常处置队列，生成 SLA 违约风险预警与责任人热点清单。

用法：
  scripts/monitor_archive_audit_approval_chain_sla_breach_alert_draft.sh [options]

选项：
  --alert-id ID                预警 ID（默认: yyyyMMdd_HHmmss）
  --approval-chain-report FILE 签批链路报告（默认: B39 样例）
  --anomaly-response FILE      异常分级处置报告（默认: B47 样例）
  --operator NAME              操作人/作业名（默认: codex）
  --output FILE                输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_<id>.md）
  --strict                     sla_breach_status 非 pass 时返回非 0
  --dry-run                    仅打印计划，不写文件
  --help                       显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --alert-id)
      ALERT_ID="$2"
      shift 2
      ;;
    --approval-chain-report)
      APPROVAL_CHAIN_REPORT="$2"
      shift 2
      ;;
    --anomaly-response)
      ANOMALY_RESPONSE_REPORT="$2"
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

if [[ -z "$ALERT_ID" ]]; then
  ALERT_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_APPROVAL_CHAIN_SLA_BREACH_ALERT_${ALERT_ID}.md"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] alert_id=$ALERT_ID"
  echo "[DRY-RUN] approval_chain_report=$APPROVAL_CHAIN_REPORT"
  echo "[DRY-RUN] anomaly_response=$ANOMALY_RESPONSE_REPORT"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$APPROVAL_CHAIN_REPORT" "$ANOMALY_RESPONSE_REPORT"; do
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
    pass|warn|fail|unknown|open|queued|closed) echo "$1" ;;
    *) echo "unknown" ;;
  esac
}

sla_to_minutes() {
  case "$1" in
    "<1h") echo 60 ;;
    "4h") echo 240 ;;
    "1bd") echo 480 ;;
    "next-weekly") echo 10080 ;;
    *) echo 720 ;;
  esac
}

alert_level_for_stage() {
  local status="$1"
  local sla="$2"

  if [[ "$status" == "fail" && "$sla" == "<1h" ]]; then
    echo "breach-risk-high"
  elif [[ "$status" == "fail" ]]; then
    echo "breach-risk-medium"
  elif [[ "$status" == "warn" ]]; then
    echo "watch"
  else
    echo "ok"
  fi
}

alert_level_for_anomaly() {
  local severity="$1"
  local status="$2"

  if [[ "$severity" == "critical" && "$status" != "closed" ]]; then
    echo "breach-risk-high"
  elif [[ "$severity" == "high" && "$status" != "closed" ]]; then
    echo "breach-risk-medium"
  elif [[ "$status" == "queued" || "$status" == "open" ]]; then
    echo "watch"
  else
    echo "ok"
  fi
}

approval_status="$(normalize_status "$(trim "$(extract_metric "$APPROVAL_CHAIN_REPORT" "approval_status")")")"
rejected_stages="$(to_int_or_zero "$(extract_metric "$APPROVAL_CHAIN_REPORT" "rejected_stages")")"
pending_review_stages="$(to_int_or_zero "$(extract_metric "$APPROVAL_CHAIN_REPORT" "pending_review_stages")")"
chain_release_decision="$(trim "$(extract_metric "$APPROVAL_CHAIN_REPORT" "release_decision")")"

response_status="$(normalize_status "$(trim "$(extract_metric "$ANOMALY_RESPONSE_REPORT" "response_status")")")"
anomalies_total="$(to_int_or_zero "$(extract_metric "$ANOMALY_RESPONSE_REPORT" "anomalies_total")")"
critical_high_open="$(to_int_or_zero "$(extract_metric "$ANOMALY_RESPONSE_REPORT" "critical_high_open")")"

rows_file="$(mktemp)"
owner_load_file="$(mktemp)"
trap 'rm -f "$rows_file" "$owner_load_file"' EXIT

total_alert_items=0
critical_alert_items=0
high_alert_items=0
medium_alert_items=0
queue_items=0

declare -A OWNER_CRITICAL
declare -A OWNER_HIGH
declare -A OWNER_MEDIUM
declare -A OWNER_TOTAL

aaa_owner_bump() {
  local owner="$1"
  local level="$2"

  OWNER_TOTAL["$owner"]=$(( ${OWNER_TOTAL["$owner"]:-0} + 1 ))
  case "$level" in
    breach-risk-high)
      OWNER_CRITICAL["$owner"]=$(( ${OWNER_CRITICAL["$owner"]:-0} + 1 ))
      ;;
    breach-risk-medium)
      OWNER_HIGH["$owner"]=$(( ${OWNER_HIGH["$owner"]:-0} + 1 ))
      ;;
    watch)
      OWNER_MEDIUM["$owner"]=$(( ${OWNER_MEDIUM["$owner"]:-0} + 1 ))
      ;;
    *) ;;
  esac
}

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 _ <<< "$row"

  stage_id="$(trim "$c1")"
  stage_name="$(trim "$c2")"
  stage_status="$(normalize_status "$(trim "$c6")")"
  owner="$(trim "$c7")"
  target_sla="$(trim "$c8")"
  followup_action="$(trim "$c10")"

  if [[ -z "$stage_id" || "$stage_id" == "none" || "$stage_id" == "<stage_id>" ]]; then
    continue
  fi

  alert_level="$(alert_level_for_stage "$stage_status" "$target_sla")"
  target_minutes="$(sla_to_minutes "$target_sla")"

  if [[ "$alert_level" != "ok" ]]; then
    total_alert_items=$((total_alert_items + 1))
    case "$alert_level" in
      breach-risk-high) critical_alert_items=$((critical_alert_items + 1)) ;;
      breach-risk-medium) high_alert_items=$((high_alert_items + 1)) ;;
      watch) medium_alert_items=$((medium_alert_items + 1)) ;;
    esac
    aaa_owner_bump "$owner" "$alert_level"
  fi

  echo "STAGE-${stage_id}|approval_chain|$owner|$target_sla|$target_minutes|status=${stage_status}|$alert_level|$followup_action" >> "$rows_file"
done < <(extract_section_rows "$APPROVAL_CHAIN_REPORT" "4) Approval Chain Rows")

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 _ <<< "$row"

  anomaly_id="$(trim "$c1")"
  severity="$(trim "$c2")"
  owner="$(trim "$c3")"
  sla="$(trim "$c4")"
  immediate_action="$(trim "$c5")"
  status="$(normalize_status "$(trim "$c6")")"

  if [[ -z "$anomaly_id" || "$anomaly_id" == "none" || "$anomaly_id" == "<A-001>" ]]; then
    continue
  fi

  queue_items=$((queue_items + 1))

  alert_level="$(alert_level_for_anomaly "$severity" "$status")"
  target_minutes="$(sla_to_minutes "$sla")"

  if [[ "$alert_level" != "ok" ]]; then
    total_alert_items=$((total_alert_items + 1))
    case "$alert_level" in
      breach-risk-high) critical_alert_items=$((critical_alert_items + 1)) ;;
      breach-risk-medium) high_alert_items=$((high_alert_items + 1)) ;;
      watch) medium_alert_items=$((medium_alert_items + 1)) ;;
    esac
    aaa_owner_bump "$owner" "$alert_level"
  fi

  echo "$anomaly_id|anomaly_response|$owner|$sla|$target_minutes|status=${status}; severity=${severity}|$alert_level|$immediate_action" >> "$rows_file"
done < <(extract_section_rows "$ANOMALY_RESPONSE_REPORT" "5) Response Queue")

owner_hotspots=0
for owner in "${!OWNER_TOTAL[@]}"; do
  critical_open="${OWNER_CRITICAL[$owner]:-0}"
  high_open="${OWNER_HIGH[$owner]:-0}"
  medium_open="${OWNER_MEDIUM[$owner]:-0}"
  queue_total="${OWNER_TOTAL[$owner]:-0}"

  recommended_window="next-weekly"
  if (( critical_open > 0 )); then
    recommended_window="<1h"
  elif (( high_open > 0 )); then
    recommended_window="4h"
  elif (( medium_open > 0 )); then
    recommended_window="1bd"
  fi

  if (( critical_open + high_open >= 3 )); then
    owner_hotspots=$((owner_hotspots + 1))
  fi

  echo "$owner|$critical_open|$high_open|$medium_open|$queue_total|$recommended_window" >> "$owner_load_file"
done

sla_breach_status="pass"
release_advice="proceed-with-standard-sla-monitoring"

if (( critical_alert_items > 0 )); then
  sla_breach_status="fail"
  release_advice="block-release-and-open-sla-incident"
elif (( high_alert_items > 3 )); then
  sla_breach_status="fail"
  release_advice="block-release-until-high-sla-alerts-reduced"
elif (( high_alert_items > 0 || medium_alert_items > 0 )); then
  sla_breach_status="warn"
  release_advice="proceed-with-sla-watchlist-and-owner-followup"
fi

if [[ "$approval_status" == "fail" && "$response_status" == "fail" ]]; then
  sla_breach_status="fail"
  release_advice="block-release-and-run-approval-anomaly-war-room"
fi

if (( total_alert_items == 0 )); then
  release_advice="no-sla-breach-risk-detected"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Approval Chain SLA Breach Alert（Draft）

## 1) Metadata

| field | value |
|------|-------|
| alert_id | $ALERT_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| approval_chain_report | $APPROVAL_CHAIN_REPORT |
| anomaly_response_report | $ANOMALY_RESPONSE_REPORT |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| approval_status | $approval_status |
| rejected_stages | $rejected_stages |
| pending_review_stages | $pending_review_stages |
| chain_release_decision | $chain_release_decision |
| anomaly_response_status | $response_status |
| anomalies_total | $anomalies_total |
| critical_high_open | $critical_high_open |
| queue_items | $queue_items |

## 3) SLA Alert Summary

| metric | value |
|--------|-------|
| total_alert_items | $total_alert_items |
| critical_alert_items | $critical_alert_items |
| high_alert_items | $high_alert_items |
| medium_alert_items | $medium_alert_items |
| owner_hotspots | $owner_hotspots |
| sla_breach_status | $sla_breach_status |
| release_advice | $release_advice |

## 4) Alert Rows

| alert_id | source | owner | target_sla | target_minutes | observed | alert_level | escalation_action |
|----------|--------|-------|------------|----------------|----------|-------------|-------------------|
EOF_REPORT

if [[ -s "$rows_file" ]]; then
  while IFS='|' read -r alert_id source owner target_sla target_minutes observed alert_level escalation_action; do
    echo "| $alert_id | $source | $owner | $target_sla | $target_minutes | $observed | $alert_level | $escalation_action |" >> "$OUTPUT_FILE"
  done < "$rows_file"
else
  echo "| none | n/a | n/a | n/a | 0 | n/a | ok | no-action |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Owner Hotspots

| owner | critical_open | high_open | medium_open | queue_total | recommended_window |
|-------|---------------|-----------|-------------|-------------|--------------------|
EOF_APPEND

if [[ -s "$owner_load_file" ]]; then
  while IFS='|' read -r owner critical_open high_open medium_open queue_total recommended_window; do
    echo "| $owner | $critical_open | $high_open | $medium_open | $queue_total | $recommended_window |" >> "$OUTPUT_FILE"
  done < "$owner_load_file"
else
  echo "| none | 0 | 0 | 0 | 0 | next-weekly |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 6) Suggested Actions

- immediate:
  - $release_advice
- followup:
  - rerun-sla-breach-alert-after-owner-triage
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$sla_breach_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass sla breach status: $sla_breach_status" >&2
  exit 1
fi

echo "[PASS] approval chain sla breach alert generated"
