#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

CHECKLIST_ID=""
GATE_SUMMARY_FILE="$PROJECT_ROOT/docs/test_reports/CROSS_PLATFORM_GATE_SUMMARY_SAMPLE_B20.md"
HOLD_REVIEW_FILE="$PROJECT_ROOT/docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md"
LINKAGE_REPORT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_HOLD_LINKAGE_SAMPLE_B27.md"
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
发布前归档审计最小核查清单生成脚本（Draft）

用途：
  聚合 gate/hold/linkage 三类报告，生成发布前最小核查清单与 readiness 判定。

用法：
  scripts/generate_pre_release_archive_audit_checklist_draft.sh [options]

选项：
  --checklist-id ID      清单 ID（默认: yyyyMMdd_HHmmss）
  --gate-summary FILE    Gate 聚合摘要文件
  --hold-review FILE     Hold 到期提醒文件
  --linkage-report FILE  抽样联动报告文件
  --operator NAME        操作人/作业名（默认: codex）
  --output FILE          输出文件（默认: docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_<id>.md）
  --strict               readiness 非 pass 时返回非 0
  --dry-run              仅打印计划，不写文件
  --help                 显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --checklist-id)
      CHECKLIST_ID="$2"
      shift 2
      ;;
    --gate-summary)
      GATE_SUMMARY_FILE="$2"
      shift 2
      ;;
    --hold-review)
      HOLD_REVIEW_FILE="$2"
      shift 2
      ;;
    --linkage-report)
      LINKAGE_REPORT_FILE="$2"
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

if [[ -z "$CHECKLIST_ID" ]]; then
  CHECKLIST_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_${CHECKLIST_ID}.md"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] checklist_id=$CHECKLIST_ID"
  echo "[DRY-RUN] gate_summary=$GATE_SUMMARY_FILE"
  echo "[DRY-RUN] hold_review=$HOLD_REVIEW_FILE"
  echo "[DRY-RUN] linkage_report=$LINKAGE_REPORT_FILE"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$GATE_SUMMARY_FILE" "$HOLD_REVIEW_FILE" "$LINKAGE_REPORT_FILE"; do
  if [[ ! -f "$file" ]]; then
    echo "[FAIL] input file not found: $file" >&2
    exit 1
  fi
done

extract_metric() {
  local file="$1"
  local key="$2"
  local value

  value="$(grep -E "^\| ${key} \|" "$file" | head -1 | sed -E 's/^\|[^|]*\|[[:space:]]*//; s/[[:space:]]*\|[[:space:]]*$//' || true)"
  echo "$value"
}

# Gate unknown/missing count from layer snapshot rows
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

gate_unknown_missing_rows="$(extract_section_rows "$GATE_SUMMARY_FILE" "2) Layer Signal Snapshot" | grep -Eci '\|[[:space:]]*(unknown|missing)[[:space:]]*\|' || true)"

hold_overdue="$(extract_metric "$HOLD_REVIEW_FILE" "overdue")"
hold_due_soon="$(extract_metric "$HOLD_REVIEW_FILE" "due_soon")"
hold_missing_expiry="$(extract_metric "$HOLD_REVIEW_FILE" "missing_expiry")"
hold_invalid_expiry="$(extract_metric "$HOLD_REVIEW_FILE" "invalid_expiry")"

linkage_risk="$(extract_metric "$LINKAGE_REPORT_FILE" "sampled_runs_risk")"
linkage_status="$(extract_metric "$LINKAGE_REPORT_FILE" "status")"

# default zeros if missing
for var in gate_unknown_missing_rows hold_overdue hold_due_soon hold_missing_expiry hold_invalid_expiry linkage_risk; do
  value="${!var}"
  if [[ -z "$value" ]] || ! [[ "$value" =~ ^[0-9]+$ ]]; then
    printf -v "$var" '%s' "0"
  fi
done

[[ -z "$linkage_status" ]] && linkage_status="unknown"

check_gate="pass"
check_overdue="pass"
check_expiry_meta="pass"
check_linkage="pass"
check_inputs="pass"

blocking=()

if (( gate_unknown_missing_rows > 0 )); then
  check_gate="fail"
  blocking+=("gate_has_unknown_or_missing")
fi

if (( hold_overdue > 0 )); then
  check_overdue="fail"
  blocking+=("hold_overdue_exists")
fi

if (( hold_missing_expiry + hold_invalid_expiry > 0 )); then
  check_expiry_meta="fail"
  blocking+=("hold_expiry_metadata_incomplete")
fi

if (( linkage_risk > 0 )) || [[ "$linkage_status" != "pass" ]]; then
  check_linkage="fail"
  blocking+=("linkage_risk_present")
fi

readiness="pass"
if (( ${#blocking[@]} > 0 )); then
  readiness="fail"
elif (( hold_due_soon > 0 )); then
  readiness="warn"
fi

blocking_reasons="none"
if (( ${#blocking[@]} > 0 )); then
  blocking_reasons="$(IFS=','; echo "${blocking[*]}")"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Pre-Release Archive Audit Checklist（Draft）

## 1) Metadata

| field | value |
|------|-------|
| checklist_id | $CHECKLIST_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| gate_summary | $GATE_SUMMARY_FILE |
| hold_review | $HOLD_REVIEW_FILE |
| linkage_report | $LINKAGE_REPORT_FILE |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| gate_unknown_or_missing_rows | $gate_unknown_missing_rows |
| hold_overdue | $hold_overdue |
| hold_due_soon | $hold_due_soon |
| hold_missing_or_invalid_expiry | $(( hold_missing_expiry + hold_invalid_expiry )) |
| linkage_sampled_runs_risk | $linkage_risk |
| linkage_status | $linkage_status |

## 3) Minimal Checklist

| check_item | result | evidence |
|------------|--------|----------|
| Gate 摘要不存在 unknown/missing 风险行 | $check_gate | gate_unknown_or_missing_rows=$gate_unknown_missing_rows |
| Hold 记录不存在 overdue | $check_overdue | hold_overdue=$hold_overdue |
| Hold 元数据不存在 missing/invalid expiry | $check_expiry_meta | hold_missing_or_invalid_expiry=$(( hold_missing_expiry + hold_invalid_expiry )) |
| 抽样联动风险为 0 且 linkage_status=pass | $check_linkage | linkage_sampled_runs_risk=$linkage_risk; linkage_status=$linkage_status |
| 关键输入报告文件均可访问 | $check_inputs | gate/hold/linkage files present |

## 4) Release Readiness

| field | value |
|------|-------|
| readiness | $readiness |
| blocking_reasons | $blocking_reasons |

## 5) Actions

- blocking:
  - <blocking_action_1>
- followup:
  - <followup_action_1>
EOF_REPORT

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$readiness" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass readiness: $readiness" >&2
  exit 1
fi

echo "[PASS] pre-release archive audit checklist generated"
