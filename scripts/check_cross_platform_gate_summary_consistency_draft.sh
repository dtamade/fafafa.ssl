#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

SUMMARY_FILE=""
STRICT=false

usage() {
  cat <<'USAGE'
跨平台 Gate 聚合摘要一致性检查脚本（Draft）

用途：
  校验 CROSS_PLATFORM_GATE_SUMMARY 报告中的关键统计字段是否一致。

用法：
  scripts/check_cross_platform_gate_summary_consistency_draft.sh [options]

选项：
  --summary FILE         摘要文件路径（默认: docs/test_reports/CROSS_PLATFORM_GATE_SUMMARY_SAMPLE_B20.md）
  --strict               有异常即返回非 0
  --help                 显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --summary)
      SUMMARY_FILE="$2"
      shift 2
      ;;
    --strict)
      STRICT=true
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

if [[ -z "$SUMMARY_FILE" ]]; then
  SUMMARY_FILE="$PROJECT_ROOT/docs/test_reports/CROSS_PLATFORM_GATE_SUMMARY_SAMPLE_B20.md"
fi

if [[ ! -f "$SUMMARY_FILE" ]]; then
  echo "[FAIL] summary file not found: $SUMMARY_FILE" >&2
  exit 1
fi

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

extract_metadata_value() {
  local file="$1"
  local key="$2"

  grep -E "^- ${key}:" "$file" | head -1 | sed -E "s/^- ${key}:[[:space:]]*//" || true
}

metadata_run_id="$(extract_metadata_value "$SUMMARY_FILE" "run_id")"
metadata_input_reports="$(extract_metadata_value "$SUMMARY_FILE" "input_reports")"

if [[ -z "$metadata_input_reports" ]] || ! [[ "$metadata_input_reports" =~ ^[0-9]+$ ]]; then
  echo "[FAIL] invalid or missing input_reports in metadata" >&2
  exit 1
fi

evidence_rows_count="$(extract_section_rows "$SUMMARY_FILE" "1) Input Evidence Reports" | wc -l | tr -d ' ')"
layer_rows_count="$(extract_section_rows "$SUMMARY_FILE" "2) Layer Signal Snapshot" | wc -l | tr -d ' ')"
platform_rows_count="$(extract_section_rows "$SUMMARY_FILE" "3) Platform Aggregate" | wc -l | tr -d ' ')"

expected_layer_rows=$(( evidence_rows_count * 4 ))

unique_platform_count="$(extract_section_rows "$SUMMARY_FILE" "1) Input Evidence Reports" | awk -F'|' '{gsub(/^[[:space:]]+|[[:space:]]+$/, "", $2); if ($2 != "") seen[$2]=1} END {count=0; for (k in seen) count++; print count+0}')"

unknown_or_missing_count="$(extract_section_rows "$SUMMARY_FILE" "2) Layer Signal Snapshot" | grep -Eci '\|[[:space:]]*(unknown|missing)[[:space:]]*\|' || true)"

status="pass"
issues=()

if [[ "$metadata_input_reports" -ne "$evidence_rows_count" ]]; then
  status="warn"
  issues+=("metadata input_reports=${metadata_input_reports} 与 evidence_rows=${evidence_rows_count} 不一致")
fi

if [[ "$layer_rows_count" -ne "$expected_layer_rows" ]]; then
  status="warn"
  issues+=("layer_rows=${layer_rows_count} 与 evidence_rows*4=${expected_layer_rows} 不一致")
fi

if [[ "$platform_rows_count" -ne "$unique_platform_count" ]]; then
  status="warn"
  issues+=("platform_aggregate_rows=${platform_rows_count} 与 unique_platforms=${unique_platform_count} 不一致")
fi

echo "==============================================="
echo "fafafa.ssl Gate Summary Consistency Check"
echo "==============================================="
echo "summary_file: $SUMMARY_FILE"
echo "run_id: ${metadata_run_id:-unknown}"
echo "input_reports(metadata): $metadata_input_reports"
echo "input_reports(rows): $evidence_rows_count"
echo "layer_rows(actual/expected): $layer_rows_count/$expected_layer_rows"
echo "platform_rows(actual/expected): $platform_rows_count/$unique_platform_count"
echo "unknown_or_missing_status_rows: $unknown_or_missing_count"
echo "status: $status"

if [[ "${#issues[@]}" -gt 0 ]]; then
  for issue in "${issues[@]}"; do
    echo "[WARN] $issue"
  done
fi

if [[ "$status" != "pass" && "$STRICT" == "true" ]]; then
  echo "[FAIL] strict mode detected inconsistency" >&2
  exit 1
fi

echo "[PASS] consistency check finished"
