#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

VERSION_ID=""
WRITEBACK_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md"
LINKAGE_REPORT="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_RETEST_APPROVAL_WRITEBACK_LINKAGE_SAMPLE_B44.md"
TARGET_VERSION=""
ROLLBACK_VERSION=""
OPERATOR="codex"
OUTPUT_FILE=""
STRICT=false
DRY_RUN=false

usage() {
  cat <<'USAGE'
归档审计回写载荷版本化与回滚管理脚本（Draft）

用途：
  基于回写载荷与联动一致性结果，生成版本化回写计划与回滚队列。

用法：
  scripts/manage_archive_audit_writeback_payload_versioning_rollback_draft.sh [options]

选项：
  --version-id ID             任务 ID（默认: yyyyMMdd_HHmmss）
  --writeback-report FILE     回写报告（默认: B42 样例）
  --linkage-report FILE       联动一致性报告（默认: B44 样例）
  --target-version VERSION    目标载荷版本（默认: wbv-<id>）
  --rollback-version VERSION  回滚版本（默认: wbv-prev）
  --operator NAME             操作人/作业名（默认: codex）
  --output FILE               输出文件（默认: docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_<id>.md）
  --strict                    versioning_status 非 pass 时返回非 0
  --dry-run                   仅打印计划，不写文件
  --help                      显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --version-id)
      VERSION_ID="$2"
      shift 2
      ;;
    --writeback-report)
      WRITEBACK_REPORT="$2"
      shift 2
      ;;
    --linkage-report)
      LINKAGE_REPORT="$2"
      shift 2
      ;;
    --target-version)
      TARGET_VERSION="$2"
      shift 2
      ;;
    --rollback-version)
      ROLLBACK_VERSION="$2"
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

if [[ -z "$VERSION_ID" ]]; then
  VERSION_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$TARGET_VERSION" ]]; then
  TARGET_VERSION="wbv-${VERSION_ID}"
fi

if [[ -z "$ROLLBACK_VERSION" ]]; then
  ROLLBACK_VERSION="wbv-prev"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ARCHIVE_AUDIT_WRITEBACK_PAYLOAD_VERSIONING_ROLLBACK_${VERSION_ID}.md"
fi

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] version_id=$VERSION_ID"
  echo "[DRY-RUN] writeback_report=$WRITEBACK_REPORT"
  echo "[DRY-RUN] linkage_report=$LINKAGE_REPORT"
  echo "[DRY-RUN] target_version=$TARGET_VERSION"
  echo "[DRY-RUN] rollback_version=$ROLLBACK_VERSION"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  exit 0
fi

for file in "$WRITEBACK_REPORT" "$LINKAGE_REPORT"; do
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
    pass|warn|fail|unknown|done|waived|pending|in-progress) echo "$1" ;;
    inprogress) echo "in-progress" ;;
    *) echo "unknown" ;;
  esac
}

writeback_status="$(normalize_status "$(trim "$(extract_metric "$WRITEBACK_REPORT" "writeback_status")")")"
writeback_signaled_items="$(to_int_or_zero "$(extract_metric "$WRITEBACK_REPORT" "retest_signaled_items")")"
writeback_changed_items="$(to_int_or_zero "$(extract_metric "$WRITEBACK_REPORT" "writeback_changed_items")")"

linkage_status="$(normalize_status "$(trim "$(extract_metric "$LINKAGE_REPORT" "linkage_status")")")"
mismatch_rows="$(to_int_or_zero "$(extract_metric "$LINKAGE_REPORT" "mismatch_rows")")"
missing_payload_rows="$(to_int_or_zero "$(extract_metric "$LINKAGE_REPORT" "missing_payload_rows")")"

rows_file="$(mktemp)"
rollback_file="$(mktemp)"
trap 'rm -f "$rows_file" "$rollback_file"' EXIT

total_payload_items=0
done_items=0
waived_items=0
pending_items=0
inprogress_items=0
unknown_items=0
rollback_candidates=0

while IFS= read -r row; do
  [[ -z "$row" ]] && continue
  IFS='|' read -r _ c1 c2 c3 c4 _ <<< "$row"

  blocker_code="$(trim "$c1")"
  current_status="$(normalize_status "$(trim "$c2")")"
  writeback_note="$(trim "$c3")"
  writeback_evidence="$(trim "$c4")"

  if [[ -z "$blocker_code" || "$blocker_code" == "none" || "$blocker_code" == "<BLK-001>" ]]; then
    continue
  fi

  total_payload_items=$((total_payload_items + 1))

  next_status="$current_status"
  rollback_marker="keep"

  case "$current_status" in
    done)
      done_items=$((done_items + 1))
      ;;
    waived)
      waived_items=$((waived_items + 1))
      rollback_marker="waived"
      ;;
    pending)
      pending_items=$((pending_items + 1))
      rollback_marker="rollback-candidate"
      rollback_candidates=$((rollback_candidates + 1))
      ;;
    in-progress)
      inprogress_items=$((inprogress_items + 1))
      rollback_marker="rollback-candidate"
      rollback_candidates=$((rollback_candidates + 1))
      ;;
    *)
      unknown_items=$((unknown_items + 1))
      rollback_marker="manual-review"
      rollback_candidates=$((rollback_candidates + 1))
      ;;
  esac

  echo "$blocker_code|$current_status|$next_status|$TARGET_VERSION|$rollback_marker|$writeback_note|$writeback_evidence" >> "$rows_file"

  if [[ "$rollback_marker" == "rollback-candidate" || "$rollback_marker" == "manual-review" ]]; then
    echo "$blocker_code|$current_status|$ROLLBACK_VERSION|$rollback_marker|$writeback_note" >> "$rollback_file"
  fi
done < <(extract_section_rows "$WRITEBACK_REPORT" "5) Receipt Writeback Payload")

versioning_status="pass"
release_advice="proceed-with-versioned-writeback"

if (( total_payload_items == 0 )); then
  versioning_status="warn"
  release_advice="insufficient-payload-items"
fi

if [[ "$writeback_status" == "fail" ]] || (( mismatch_rows > 0 || missing_payload_rows > 0 )); then
  versioning_status="fail"
  release_advice="block-release-and-stage-rollback"
elif [[ "$writeback_status" == "warn" || "$linkage_status" == "warn" ]]; then
  versioning_status="warn"
  release_advice="allow-partial-rollout-with-rollback-guard"
fi

if (( writeback_signaled_items > 0 && writeback_changed_items == 0 )); then
  versioning_status="fail"
  release_advice="block-release-until-writeback-version-applied"
fi

if (( rollback_candidates > 0 )) && [[ "$versioning_status" == "pass" ]]; then
  versioning_status="warn"
  release_advice="proceed-with-rollback-watchlist"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Archive Audit Writeback Payload Versioning & Rollback Plan（Draft）

## 1) Metadata

| field | value |
|------|-------|
| version_id | $VERSION_ID |
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| writeback_report | $WRITEBACK_REPORT |
| linkage_report | $LINKAGE_REPORT |
| target_version | $TARGET_VERSION |
| rollback_version | $ROLLBACK_VERSION |
| operator | $OPERATOR |

## 2) Input Snapshot

| metric | value |
|--------|-------|
| writeback_status | $writeback_status |
| writeback_signaled_items | $writeback_signaled_items |
| writeback_changed_items | $writeback_changed_items |
| linkage_status | $linkage_status |
| mismatch_rows | $mismatch_rows |
| missing_payload_rows | $missing_payload_rows |

## 3) Versioning Summary

| metric | value |
|--------|-------|
| total_payload_items | $total_payload_items |
| done_items | $done_items |
| waived_items | $waived_items |
| pending_items | $pending_items |
| inprogress_items | $inprogress_items |
| unknown_items | $unknown_items |
| rollback_candidates | $rollback_candidates |
| versioning_status | $versioning_status |
| release_advice | $release_advice |

## 4) Versioned Payload Rows

| blocker_code | current_status | next_status | target_version | rollback_marker | note | evidence |
|--------------|----------------|-------------|----------------|-----------------|------|----------|
EOF_REPORT

if [[ -s "$rows_file" ]]; then
  while IFS='|' read -r blocker_code current_status next_status target_version rollback_marker note evidence; do
    echo "| $blocker_code | $current_status | $next_status | $target_version | $rollback_marker | $note | $evidence |" >> "$OUTPUT_FILE"
  done < "$rows_file"
else
  echo "| none | unknown | unknown | $TARGET_VERSION | manual-review | no-payload-rows | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 5) Rollback Queue

| blocker_code | current_status | rollback_version | rollback_reason | note |
|--------------|----------------|------------------|-----------------|------|
EOF_APPEND

if [[ -s "$rollback_file" ]]; then
  while IFS='|' read -r blocker_code current_status rollback_version rollback_reason note; do
    echo "| $blocker_code | $current_status | $rollback_version | $rollback_reason | $note |" >> "$OUTPUT_FILE"
  done < "$rollback_file"
else
  echo "| none | n/a | n/a | no-rollback-needed | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<EOF_APPEND

## 6) Suggested Actions

- immediate:
  - $release_advice
- followup:
  - apply-versioned-payload-and-rerun-linkage-validation
EOF_APPEND

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$versioning_status" != "pass" ]]; then
  echo "[FAIL] strict mode detected non-pass versioning status: $versioning_status" >&2
  exit 1
fi

echo "[PASS] writeback payload versioning & rollback plan generated"
