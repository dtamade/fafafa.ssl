#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

INPUT_GLOB="docs/test_reports/GATE_ARCHIVE_EVIDENCE_*.md"
OUTPUT_FILE=""
RUN_ID=""
DRY_RUN=false
VERBOSE=false

usage() {
  cat <<'USAGE'
跨平台 Gate 聚合摘要生成脚本（Draft）

用途：
  从门禁证据报告（GATE_ARCHIVE_EVIDENCE_*.md）提取 metadata/L0~L3 状态，
  生成跨平台汇总摘要。

用法：
  scripts/generate_cross_platform_gate_summary_draft.sh [options]

选项：
  --input GLOB_OR_FILE   输入模式（默认: docs/test_reports/GATE_ARCHIVE_EVIDENCE_*.md）
  --output FILE          输出文件（默认: docs/test_reports/CROSS_PLATFORM_GATE_SUMMARY_<runid>.md）
  --run-id ID            指定 run_id（默认: yyyyMMdd_HHmmss）
  --dry-run              仅打印解析计划，不写文件
  --verbose              输出更多调试信息
  --help                 显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --input)
      INPUT_GLOB="$2"
      shift 2
      ;;
    --output)
      OUTPUT_FILE="$2"
      shift 2
      ;;
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    --verbose)
      VERBOSE=true
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

if [[ -z "$RUN_ID" ]]; then
  RUN_ID="$(date +"%Y%m%d_%H%M%S")"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/CROSS_PLATFORM_GATE_SUMMARY_${RUN_ID}.md"
fi

mapfile -t REPORT_FILES < <(
  cd "$PROJECT_ROOT"
  shopt -s nullglob
  for file in $INPUT_GLOB; do
    [[ -f "$file" ]] && printf '%s\n' "$file"
  done
)

if [[ "${#REPORT_FILES[@]}" -eq 0 ]]; then
  echo "[WARN] no evidence report matched: $INPUT_GLOB"
  if [[ "$DRY_RUN" == "true" ]]; then
    echo "[PASS] dry-run finished (no input files)"
    exit 0
  fi
fi

extract_field() {
  local file="$1"
  local field="$2"
  local value

  value="$(grep -E "^\| ${field} \|" "$file" | head -1 | sed -E 's/^\|[^|]*\|[[:space:]]*//; s/[[:space:]]*\|[[:space:]]*$//' || true)"
  if [[ -z "$value" ]]; then
    echo "unknown"
  else
    echo "$value"
  fi
}

extract_layer_status() {
  local file="$1"
  local layer="$2"
  local row
  local status

  row="$(grep -E "^\| ${layer} \|" "$file" | head -1 || true)"
  if [[ -z "$row" ]]; then
    echo "missing"
    return 0
  fi

  status="$(echo "$row" | awk -F'|' '{gsub(/^[[:space:]]+|[[:space:]]+$/, "", $7); print $7}')"
  if [[ -z "$status" ]]; then
    echo "unknown"
  elif [[ "$status" == *"<"* || "$status" == *">"* ]]; then
    echo "unknown"
  else
    echo "$status"
  fi
}

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[DRY-RUN] run_id=$RUN_ID"
  echo "[DRY-RUN] output=$OUTPUT_FILE"
  echo "[DRY-RUN] input_count=${#REPORT_FILES[@]}"
  for file in "${REPORT_FILES[@]}"; do
    platform="$(extract_field "$PROJECT_ROOT/$file" "platform")"
    profile="$(extract_field "$PROJECT_ROOT/$file" "workflow_profile")"
    runid_local="$(extract_field "$PROJECT_ROOT/$file" "run_id")"
    echo "[DRY-RUN] $file platform=$platform profile=$profile run_id=$runid_local"
  done
  echo "[PASS] cross-platform gate summary dry-run finished"
  exit 0
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

entries_file="$(mktemp)"
layers_file="$(mktemp)"
trap 'rm -f "$entries_file" "$layers_file"' EXIT

for file in "${REPORT_FILES[@]}"; do
  abs_file="$PROJECT_ROOT/$file"

  platform="$(extract_field "$abs_file" "platform")"
  profile="$(extract_field "$abs_file" "workflow_profile")"
  runid_local="$(extract_field "$abs_file" "run_id")"
  focus_layer="$(extract_field "$abs_file" "focus_layer")"

  echo "$platform,$profile,$runid_local,$focus_layer,$file" >> "$entries_file"

  for layer in L0 L1 L2 L3; do
    status="$(extract_layer_status "$abs_file" "$layer")"
    echo "$platform,$profile,$runid_local,$layer,$status,$file" >> "$layers_file"
  done

done

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Cross-Platform Gate Summary（Draft）

- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')
- run_id: $RUN_ID
- source_pattern: $INPUT_GLOB
- input_reports: ${#REPORT_FILES[@]}

## 1) Input Evidence Reports

| platform | profile | run_id | focus_layer | source |
|----------|---------|--------|-------------|--------|
EOF_REPORT

if [[ -s "$entries_file" ]]; then
  sort "$entries_file" | while IFS=',' read -r platform profile runid_local focus_layer source; do
    echo "| $platform | $profile | $runid_local | $focus_layer | $source |" >> "$OUTPUT_FILE"
  done
else
  echo "| n/a | n/a | n/a | n/a | no input evidence |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 2) Layer Signal Snapshot

| platform | profile | run_id | layer | status | source |
|----------|---------|--------|-------|--------|--------|
EOF_APPEND

if [[ -s "$layers_file" ]]; then
  sort "$layers_file" | while IFS=',' read -r platform profile runid_local layer status source; do
    echo "| $platform | $profile | $runid_local | $layer | $status | $source |" >> "$OUTPUT_FILE"
  done
else
  echo "| n/a | n/a | n/a | n/a | n/a | no input evidence |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 3) Platform Aggregate

| platform | report_count | profile_samples |
|----------|--------------|-----------------|
EOF_APPEND

if [[ -s "$entries_file" ]]; then
  awk -F',' '{cnt[$1]++; profile[$1]=profile[$1] (profile[$1]?"/":"") $2} END {for (p in cnt) printf("| %s | %d | %s |\n", p, cnt[p], profile[p]);}' "$entries_file" | sort >> "$OUTPUT_FILE"
else
  echo "| n/a | 0 | n/a |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 4) Next Actions

- 校验 `unknown/missing` 状态对应的原始报告是否缺字段。
- 如需发布级审阅，补齐 release profile 的 L3 实证记录。
- 将本摘要与 `artifacts/ci/<run_id>/manifest.*` 关联归档。
EOF_APPEND

if [[ "$VERBOSE" == "true" ]]; then
  echo "[INFO] parsed files: ${#REPORT_FILES[@]}"
fi

echo "[PASS] cross-platform gate summary generated: $OUTPUT_FILE"
