#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

DOCS_ROOT="$PROJECT_ROOT/docs"
OUTPUT_FILE=""
STRICT=false
INCLUDE_POLICY=false
PATTERN='TODO|TBD|WIP|FIXME|placeholder|占位|待办'

usage() {
  cat <<'USAGE'
active docs 噪声扫描脚本（Draft）

用途：
  扫描 active docs 中的临时标记关键词，并输出 Markdown 报告。

用法：
  scripts/scan_active_docs_noise_draft.sh [options]

选项：
  --docs-root DIR         docs 根目录（默认: docs）
  --output FILE           输出 Markdown 报告文件（默认: docs/test_reports/ACTIVE_DOCS_NOISE_SCAN_<id>.md）
  --strict                命中 > 0 时返回非 0（报告仍会写出）
  --include-policy        扫描时包含 DOCS_NOISE_GOVERNANCE.md（默认排除）
  --help                  显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --docs-root)
      DOCS_ROOT="$2"
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
    --include-policy)
      INCLUDE_POLICY=true
      shift
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "[FAIL] unknown option: $1" >&2
      usage
      exit 1
      ;;
  esac
done

resolve_input_dir() {
  local path="$1"
  if [[ "$path" == /* ]]; then
    echo "$path"
  elif [[ -d "$path" ]]; then
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

DOCS_ROOT="$(resolve_input_dir "$DOCS_ROOT")"
if [[ ! -d "$DOCS_ROOT" ]]; then
  echo "[FAIL] docs root not found: $DOCS_ROOT" >&2
  exit 1
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  RUN_ID="$(date +"%Y%m%d_%H%M%S")"
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/ACTIVE_DOCS_NOISE_SCAN_${RUN_ID}.md"
fi
OUTPUT_FILE="$(resolve_output_path "$OUTPUT_FILE")"

declare -a RG_ARGS
RG_ARGS=(
  -n
  --no-heading
  --color
  never
  -e
  "$PATTERN"
  .
  --glob
  '!archive/**'
  --glob
  '!plans/**'
  --glob
  '!test_reports/**'
)

if [[ "$INCLUDE_POLICY" != "true" ]]; then
  RG_ARGS+=(
    --glob
    '!DOCS_NOISE_GOVERNANCE.md'
  )
fi

declare -a HITS
mapfile -t HITS < <(
  cd "$DOCS_ROOT"
  rg "${RG_ARGS[@]}" || true
)

HIT_COUNT="${#HITS[@]}"
GENERATED_AT="$(date +%Y-%m-%d\ %H:%M:%S\ %z)"

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF
# Active Docs Noise Scan Report

## Metadata

| Field | Value |
|-------|-------|
| docs_root | $DOCS_ROOT |
| generated_at | $GENERATED_AT |
| strict_mode | $STRICT |
| include_policy | $INCLUDE_POLICY |
| pattern | $PATTERN |

## Summary

| Metric | Value |
|--------|-------|
| total_hits | $HIT_COUNT |

## Hits

EOF

if [[ "$HIT_COUNT" -eq 0 ]]; then
  echo "- none" >> "$OUTPUT_FILE"
else
  for item in "${HITS[@]}"; do
    echo "- \`$item\`" >> "$OUTPUT_FILE"
  done
fi

echo "report: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$HIT_COUNT" -gt 0 ]]; then
  echo "[FAIL] strict mode detected active docs noise hits: $HIT_COUNT" >&2
  exit 1
fi

echo "[PASS] active docs noise scan completed"
