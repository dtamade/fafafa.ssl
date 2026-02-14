#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

INDEX_FILE="$PROJECT_ROOT/docs/DOCUMENTATION_INDEX.md"
SCOPE="archive-evidence"
STRICT=false
OUTPUT_FILE=""

usage() {
  cat <<'USAGE'
文档索引去重检查脚本（Draft）

用途：
  检查 `docs/DOCUMENTATION_INDEX.md` 中链接路径/标题重复情况，优先关注归档与证据相关条目。

用法：
  scripts/check_docs_index_dedup_draft.sh [options]

选项：
  --index FILE           文档索引路径（默认: docs/DOCUMENTATION_INDEX.md）
  --scope NAME           检查范围（archive-evidence|all，默认: archive-evidence）
  --output FILE          输出 Markdown 报告文件
  --strict               发现重复即返回非 0
  --help                 显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --index)
      INDEX_FILE="$2"
      shift 2
      ;;
    --scope)
      SCOPE="$2"
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

INDEX_FILE="$(resolve_input_path "$INDEX_FILE")"
if [[ -n "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$(resolve_output_path "$OUTPUT_FILE")"
fi

case "$SCOPE" in
  archive-evidence|all) ;;
  *)
    echo "[FAIL] unsupported scope: $SCOPE" >&2
    exit 1
    ;;
esac

if [[ ! -f "$INDEX_FILE" ]]; then
  echo "[FAIL] index file not found: $INDEX_FILE" >&2
  exit 1
fi

entries_file="$(mktemp)"
path_dups_file="$(mktemp)"
title_dups_file="$(mktemp)"
trap 'rm -f "$entries_file" "$path_dups_file" "$title_dups_file"' EXIT

while IFS= read -r token; do
  [[ -z "$token" ]] && continue

  title="$(echo "$token" | sed -E 's/^\[([^]]+)\]\(([^)]+)\)$/\1/')"
  path="$(echo "$token" | sed -E 's/^\[([^]]+)\]\(([^)]+)\)$/\2/')"

  if [[ "$SCOPE" == "archive-evidence" ]]; then
    if [[ ! "$path" =~ ^test_reports/ ]] && [[ ! "$path" =~ ^plans/PHASE4_ ]] && [[ ! "$path" =~ ^archive/ ]]; then
      continue
    fi
  fi

  echo "$title|$path" >> "$entries_file"
done < <(grep -oE '\[[^]]+\]\([^)]+\)' "$INDEX_FILE")

total_entries=0
if [[ -s "$entries_file" ]]; then
  total_entries="$(wc -l < "$entries_file" | tr -d ' ')"
fi

if [[ -s "$entries_file" ]]; then
  awk -F'|' '{count[$2]++} END {for (p in count) if (count[p] > 1) printf("%s|%d\n", p, count[p]);}' "$entries_file" | sort > "$path_dups_file"
  awk -F'|' '{count[$1]++} END {for (t in count) if (count[t] > 1) printf("%s|%d\n", t, count[t]);}' "$entries_file" | sort > "$title_dups_file"
fi

path_dup_count=0
title_dup_count=0
if [[ -s "$path_dups_file" ]]; then
  path_dup_count="$(wc -l < "$path_dups_file" | tr -d ' ')"
fi
if [[ -s "$title_dups_file" ]]; then
  title_dup_count="$(wc -l < "$title_dups_file" | tr -d ' ')"
fi

status="pass"
if (( path_dup_count > 0 || title_dup_count > 0 )); then
  status="warn"
fi

echo "=========================================="
echo "fafafa.ssl Docs Index Dedup Check (Draft)"
echo "=========================================="
echo "index_file: $INDEX_FILE"
echo "scope: $SCOPE"
echo "entries_scanned: $total_entries"
echo "duplicate_paths: $path_dup_count"
echo "duplicate_titles: $title_dup_count"
echo "status: $status"

if [[ -s "$path_dups_file" ]]; then
  echo "[WARN] duplicate paths"
  awk -F'|' '{print "  - "$1" ("$2")"}' "$path_dups_file"
fi

if [[ -s "$title_dups_file" ]]; then
  echo "[WARN] duplicate titles"
  awk -F'|' '{print "  - "$1" ("$2")"}' "$title_dups_file"
fi

if [[ -n "$OUTPUT_FILE" ]]; then
  mkdir -p "$(dirname "$OUTPUT_FILE")"

  cat > "$OUTPUT_FILE" <<EOF_REPORT
# Documentation Index Dedup Check（Draft）

## 1) Metadata

| field | value |
|------|-------|
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| index_file | $INDEX_FILE |
| scope | $SCOPE |
| entries_scanned | $total_entries |
| duplicate_paths | $path_dup_count |
| duplicate_titles | $title_dup_count |
| status | $status |

## 2) Duplicate Paths

| path | occurrences |
|------|-------------|
EOF_REPORT

  if [[ -s "$path_dups_file" ]]; then
    while IFS='|' read -r path count; do
      echo "| $path | $count |" >> "$OUTPUT_FILE"
    done < "$path_dups_file"
  else
    echo "| n/a | 0 |" >> "$OUTPUT_FILE"
  fi

  cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 3) Duplicate Titles

| title | occurrences |
|-------|-------------|
EOF_APPEND

  if [[ -s "$title_dups_file" ]]; then
    while IFS='|' read -r title count; do
      echo "| $title | $count |" >> "$OUTPUT_FILE"
    done < "$title_dups_file"
  else
    echo "| n/a | 0 |" >> "$OUTPUT_FILE"
  fi

  cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 4) Next Actions

- 对重复 path/title 条目评估是否应合并或迁移到单一入口。
- 对 archive 与 test_reports 条目保持“一个文档一个主入口”的索引策略。
- 在新增 Phase4 文档时先运行本检查，避免增量重复。
EOF_APPEND

  echo "report: $OUTPUT_FILE"
fi

if [[ "$STRICT" == "true" && "$status" != "pass" ]]; then
  echo "[FAIL] strict mode detected duplicate entries" >&2
  exit 1
fi

echo "[PASS] docs index dedup check finished"
