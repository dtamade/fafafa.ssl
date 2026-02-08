#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

ARTIFACT_ROOT="$PROJECT_ROOT/artifacts/ci"
LOOKAHEAD_DAYS=7
TODAY=""
STRICT=false
OUTPUT_FILE=""

usage() {
  cat <<'USAGE'
hold 到期复核提醒脚本（Draft）

用途：
  扫描 `.hold.meta` 并输出即将到期/已到期的复核提醒清单。

用法：
  scripts/remind_hold_expiry_review_draft.sh [options]

选项：
  --root DIR             归档根目录（默认: artifacts/ci）
  --days N               提前提醒天数（默认: 7）
  --today YYYY-MM-DD     指定参考日期（默认: 系统当天）
  --output FILE          输出 Markdown 报告文件
  --strict               存在 overdue 即返回非 0
  --help                 显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --root)
      ARTIFACT_ROOT="$2"
      shift 2
      ;;
    --days)
      LOOKAHEAD_DAYS="$2"
      shift 2
      ;;
    --today)
      TODAY="$2"
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

if ! [[ "$LOOKAHEAD_DAYS" =~ ^[0-9]+$ ]]; then
  echo "[FAIL] --days must be a non-negative integer" >&2
  exit 1
fi

if [[ -z "$TODAY" ]]; then
  TODAY="$(date +%F)"
fi

today_epoch="$(date -d "$TODAY" +%s 2>/dev/null || true)"
if [[ -z "$today_epoch" ]]; then
  echo "[FAIL] invalid --today date: $TODAY" >&2
  exit 1
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/HOLD_EXPIRY_REVIEW_${TODAY}.md"
fi

tmp_rows="$(mktemp)"
trap 'rm -f "$tmp_rows"' EXIT

if [[ -d "$ARTIFACT_ROOT" ]]; then
  while IFS= read -r meta_file; do
    [[ -z "$meta_file" ]] && continue

    run_dir="$(dirname "$meta_file")"
    run_id="$(basename "$run_dir")"

    owner="$(grep -Ei '^owner:' "$meta_file" | head -1 | sed -E 's/^owner:[[:space:]]*//' || true)"
    reason="$(grep -Ei '^reason:' "$meta_file" | head -1 | sed -E 's/^reason:[[:space:]]*//' || true)"
    expires_on="$(grep -Ei '^expires_on:' "$meta_file" | head -1 | sed -E 's/^expires_on:[[:space:]]*//' || true)"

    [[ -z "$owner" ]] && owner="unknown"
    [[ -z "$reason" ]] && reason="unknown"
    [[ -z "$expires_on" ]] && expires_on="n/a"

    status="ok"
    days_left="n/a"

    if [[ "$expires_on" == "n/a" ]]; then
      status="missing-expiry"
    else
      expires_epoch="$(date -d "$expires_on" +%s 2>/dev/null || true)"
      if [[ -z "$expires_epoch" ]]; then
        status="invalid-expiry"
      else
        days_left=$(( (expires_epoch - today_epoch) / 86400 ))
        if (( days_left < 0 )); then
          status="overdue"
        elif (( days_left <= LOOKAHEAD_DAYS )); then
          status="due-soon"
        else
          status="ok"
        fi
      fi
    fi

    echo "$run_id|$expires_on|$days_left|$status|$owner|$reason|$meta_file" >> "$tmp_rows"
  done < <(find "$ARTIFACT_ROOT" -mindepth 2 -maxdepth 2 -type f -name '.hold.meta' | sort)
fi

total=0
overdue=0
due_soon=0
missing_expiry=0
invalid_expiry=0

if [[ -s "$tmp_rows" ]]; then
  total="$(wc -l < "$tmp_rows" | tr -d ' ')"
  overdue="$(awk -F'|' '$4=="overdue" {c++} END {print c+0}' "$tmp_rows")"
  due_soon="$(awk -F'|' '$4=="due-soon" {c++} END {print c+0}' "$tmp_rows")"
  missing_expiry="$(awk -F'|' '$4=="missing-expiry" {c++} END {print c+0}' "$tmp_rows")"
  invalid_expiry="$(awk -F'|' '$4=="invalid-expiry" {c++} END {print c+0}' "$tmp_rows")"
fi

echo "=========================================="
echo "fafafa.ssl Hold Expiry Review Reminder"
echo "=========================================="
echo "artifact_root: $ARTIFACT_ROOT"
echo "today: $TODAY"
echo "lookahead_days: $LOOKAHEAD_DAYS"
echo "summary: total=$total overdue=$overdue due_soon=$due_soon missing_expiry=$missing_expiry invalid_expiry=$invalid_expiry"

if [[ -s "$tmp_rows" ]]; then
  awk -F'|' 'BEGIN {print "run_id|expires_on|days_left|status|owner|reason|meta_path"}
    {print $1"|"$2"|"$3"|"$4"|"$5"|"$6"|"$7}
  ' "$tmp_rows"
else
  echo "run_id|expires_on|days_left|status|owner|reason|meta_path"
  echo "n/a|n/a|n/a|n/a|n/a|n/a|no hold metadata found"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Hold Expiry Review Reminder（Draft）

## 1) Metadata

| field | value |
|------|-------|
| generated_at | $(date '+%Y-%m-%d %H:%M:%S %z') |
| artifact_root | $ARTIFACT_ROOT |
| today | $TODAY |
| lookahead_days | $LOOKAHEAD_DAYS |

## 2) Summary

| metric | value |
|--------|-------|
| total_holds | $total |
| overdue | $overdue |
| due_soon | $due_soon |
| missing_expiry | $missing_expiry |
| invalid_expiry | $invalid_expiry |

## 3) Hold Review Rows

| run_id | expires_on | days_left | status | owner | reason | meta_path |
|--------|------------|-----------|--------|-------|--------|-----------|
EOF_REPORT

if [[ -s "$tmp_rows" ]]; then
  while IFS='|' read -r run_id expires_on days_left status owner reason meta_path; do
    echo "| $run_id | $expires_on | $days_left | $status | $owner | $reason | $meta_path |" >> "$OUTPUT_FILE"
  done < "$tmp_rows"
else
  echo "| n/a | n/a | n/a | n/a | n/a | n/a | no hold metadata found |" >> "$OUTPUT_FILE"
fi

cat >> "$OUTPUT_FILE" <<'EOF_APPEND'

## 4) Next Actions

- 对 `overdue` 条目执行复核或续期。
- 对 `missing-expiry` 与 `invalid-expiry` 条目补齐规范日期。
- 将复核结果回写到对应 `.hold.meta` 与审计记录。
EOF_APPEND

echo "report: $OUTPUT_FILE"

if (( overdue > 0 )) && [[ "$STRICT" == "true" ]]; then
  echo "[FAIL] strict mode detected overdue hold records" >&2
  exit 1
fi

echo "[PASS] hold expiry review reminder finished"
