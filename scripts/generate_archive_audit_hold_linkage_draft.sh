#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

LINKAGE_ID=""
SAMPLING_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_SAMPLING_RECORD_SAMPLE_B23.md"
HOLD_REVIEW_FILE="$PROJECT_ROOT/docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md"
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计抽样与 hold 到期提醒联动脚本（Draft）

用途：
  将 B23 抽样记录与 B25 到期提醒报告按 run_id 联动，输出风险聚合视图。

用法：
  scripts/generate_archive_audit_hold_linkage_draft.sh [options]

选项：
  --linkage-id ID        联动记录 ID（默认: yyyyMMdd_HHmmss）
  --sampling FILE        抽样记录文件（默认: docs/test_reports/ARCHIVE_AUDIT_SAMPLING_RECORD_SAMPLE_B23.md）
  --hold-review FILE     hold 到期提醒文件（默认: docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md）
  --operator NAME        操作人/作业名（默认: codex）
  --output FILE          输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_<id>.md）
  --strict               存在风险条目时返回非 0
  --dry-run              仅打印计划，不写文件
  --help                 显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --linkage-id)
      LINKAGE_ID="$2"
      shift 2
      ;;
    --sampling)
      SAMPLING_FILE="$2"
      shift 2
      ;;
    --hold-review)
      HOLD_REVIEW_FILE="$2"
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

if [[ -z "$LINKAGE_ID" ]]; then
  LINKAGE_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_${LINKAGE_ID}.md"
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

SAMPLING_FILE="$(resolve_input_path "$SAMPLING_FILE")"
HOLD_REVIEW_FILE="$(resolve_input_path "$HOLD_REVIEW_FILE")"
OUTPUT_FILE="$(resolve_output_path "$OUTPUT_FILE")"

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] linkage_id=$LINKAGE_ID"
  echo "[DRY-RUN] sampling_file=$SAMPLING_FILE"
  echo "[DRY-RUN] hold_review_file=$HOLD_REVIEW_FILE"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

if [[ ! -f "$SAMPLING_FILE" ]]; then
  echo "[FAIL] sampling file not found: $SAMPLING_FILE" >&2
  exit 1
fi

if [[ ! -f "$HOLD_REVIEW_FILE" ]]; then
  echo "[FAIL] hold review file not found: $HOLD_REVIEW_FILE" >&2
  exit 1
fi

trim() {
  echo "$1" | sed -E 's/^[[:space:]]+//; s/[[:space:]]+$//'
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

sample_rows_file="$(mktemp)"
hold_rows_file="$(mktemp)"
linked_rows_file="$(mktemp)"
trap 'rm -f "$sample_rows_file" "$hold_rows_file" "$linked_rows_file"' EXIT

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 _ <<< "$row"

  run_id="$(trim "$c1")"
  profile="$(trim "$c2")"
  age_days="$(trim "$c3")"
  sample_hold="$(trim "$c4")"
  manifest="$(trim "$c5")"
  source_path="$(trim "$c6")"

  [[ -z "$run_id" ]] && continue
  echo "$run_id|$profile|$age_days|$sample_hold|$manifest|$source_path" >> "$sample_rows_file"
done < <(extract_section_rows "$SAMPLING_FILE" "4) Sampled Runs")

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 c5 c6 c7 _ <<< "$row"

  run_id="$(trim "$c1")"
  expires_on="$(trim "$c2")"
  days_left="$(trim "$c3")"
  review_status="$(trim "$c4")"
  owner="$(trim "$c5")"
  reason="$(trim "$c6")"
  meta_path="$(trim "$c7")"

  [[ -z "$run_id" ]] && continue
  echo "$run_id|$expires_on|$days_left|$review_status|$owner|$reason|$meta_path" >> "$hold_rows_file"
done < <(extract_section_rows "$HOLD_REVIEW_FILE" "3) Hold Review Rows")

sampled_total=0
with_review=0
missing_review=0
risk_count=0

if [[ -s "$sample_rows_file" ]]; then
  while IFS='|' read -r run_id profile age_days sample_hold manifest source_path; do
    hold_row="$(awk -F'|' -v id="$run_id" '$1==id {print; exit}' "$hold_rows_file" || true)"

    if [[ -n "$hold_row" ]]; then
      with_review=$((with_review + 1))
      IFS='|' read -r _ expires_on days_left review_status owner reason meta_path <<< "$hold_row"
    else
      expires_on="n/a"
      days_left="n/a"
      owner="unknown"
      reason="not-linked"
      meta_path="n/a"
      if [[ "$sample_hold" == "yes" ]]; then
        review_status="not-found"
      else
        review_status="n/a"
      fi
    fi

    action="keep-monitoring"
    is_risk=false

    case "$review_status" in
      overdue)
        action="immediate-review"
        is_risk=true
        ;;
      due-soon)
        action="schedule-review"
        ;;
      missing-expiry|invalid-expiry)
        action="fix-metadata"
        is_risk=true
        ;;
      not-found)
        action="verify-hold-sync"
        is_risk=true
        ;;
      ok)
        action="keep-monitoring"
        ;;
      n/a)
        action="n/a"
        ;;
      *)
        action="investigate"
        is_risk=true
        ;;
    esac

    sampled_total=$((sampled_total + 1))

    if [[ "$review_status" == "not-found" ]]; then
      missing_review=$((missing_review + 1))
    fi

    if [[ "$is_risk" == "true" ]]; then
      risk_count=$((risk_count + 1))
    fi

    echo "$run_id|$sample_hold|$review_status|$expires_on|$days_left|$owner|$reason|$action" >> "$linked_rows_file"
  done < "$sample_rows_file"
fi

status="pass"
if (( risk_count > 0 || missing_review > 0 )); then
  status="warn"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit & Hold Expiry Linkage（Draft）

## 1) Metadata

| field | value |
|------|-------|
| linkage_id | $LINKAGE_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| sampling_record | $SAMPLING_FILE |
| hold_review_record | $HOLD_REVIEW_FILE |
| operator | $OPERATOR |

## 2) Linkage Summary

| metric | value |
|--------|-------|
| sampled_runs_total | $sampled_total |
| sampled_runs_with_hold_review | $with_review |
| sampled_runs_missing_hold_review | $missing_review |
| sampled_runs_risk | $risk_count |
| status | $status |

## 3) Sampled Run Linkage Rows

| run_id | sample_hold | review_status | expires_on | days_left | owner | reason | action |
|--------|-------------|---------------|------------|-----------|-------|--------|--------|
EOF_REPORT

if [[ -s "$linked_rows_file" ]]; then
  while IFS='|' read -r run_id sample_hold review_status expires_on days_left owner reason action; do
    echo "| $run_id | $sample_hold | $review_status | $expires_on | $days_left | $owner | $reason | $action |" >> "$OUTPUT_FILE"
  done < "$linked_rows_file"
else
  echo "| n/a | n/a | n/a | n/a | n/a | n/a | n/a | no sampled rows |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 4) Risk Checklist

- [ ] `overdue` 条目已升级处理。
- [ ] `missing-expiry/invalid-expiry` 条目已补齐日期。
- [ ] `not-found` 且 `sample_hold=yes` 条目已复核元数据同步。

## 5) Attachments

- <archive_audit_sampling_record_path>
- <hold_expiry_review_report_path>
- <followup_ticket_or_log_path>
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$status" != "pass" ]]; then
  echo "[FAIL] strict mode detected linkage risks" >&2
  exit 1
fi

echo "[PASS] archive audit-hold linkage generated"
