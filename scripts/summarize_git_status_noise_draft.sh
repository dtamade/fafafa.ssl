#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$PROJECT_ROOT"
OUTPUT_FILE=""
MAX_SAMPLES=10

usage() {
  cat <<'USAGE'
git status 噪音摘要脚本（Draft）

用途：
  读取 `git status --short --untracked-files=all`，按类别汇总工作区噪音。

用法：
  scripts/summarize_git_status_noise_draft.sh [options]

选项：
  --repo-root DIR      Git 仓库根目录（默认: 当前项目根）
  --output FILE        输出 Markdown 报告路径（默认: tmp/git_status_noise_summary.md）
  --max-samples N      每个类别最多展示的样例条数（默认: 10）
  --help               显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --repo-root)
      REPO_ROOT="$2"
      shift 2
      ;;
    --output)
      OUTPUT_FILE="$2"
      shift 2
      ;;
    --max-samples)
      MAX_SAMPLES="$2"
      shift 2
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

REPO_ROOT="$(resolve_input_dir "$REPO_ROOT")"
[[ -d "$REPO_ROOT" ]] || { echo "[FAIL] repo root not found: $REPO_ROOT" >&2; exit 1; }
git -C "$REPO_ROOT" rev-parse --is-inside-work-tree >/dev/null 2>&1 || {
  echo "[FAIL] repo root is not a git work tree: $REPO_ROOT" >&2
  exit 1
}

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$PROJECT_ROOT/tmp/git_status_noise_summary.md"
fi
OUTPUT_FILE="$(resolve_output_path "$OUTPUT_FILE")"
mkdir -p "$(dirname "$OUTPUT_FILE")"
mkdir -p "$PROJECT_ROOT/tmp"

[[ "$MAX_SAMPLES" =~ ^[0-9]+$ ]] || { echo "[FAIL] max samples should be a non-negative integer" >&2; exit 1; }

CATEGORIES=(
  generated_artifacts_root_bin
  workflow_drift
  docs_drift
  source_edits
  scripts_drift
  tests_drift
  examples_drift
  test_reports_drift
  worktree_meta
  other
)

declare -A COUNTS
TMP_DIR="$(mktemp -d "$PROJECT_ROOT/tmp/git_status_noise_summary.XXXXXX")"
trap 'rm -rf "$TMP_DIR"' EXIT

for cat in "${CATEGORIES[@]}"; do
  COUNTS[$cat]=0
  : > "$TMP_DIR/$cat.txt"
done

categorize_path() {
  local path="$1"
  case "$path" in
    bin/*)
      echo "generated_artifacts_root_bin"
      ;;
    .github/workflows/*)
      echo "workflow_drift"
      ;;
    docs/*)
      echo "docs_drift"
      ;;
    src/*)
      echo "source_edits"
      ;;
    scripts/*)
      echo "scripts_drift"
      ;;
    tests/*)
      echo "tests_drift"
      ;;
    examples/*)
      echo "examples_drift"
      ;;
    test-reports/*)
      echo "test_reports_drift"
      ;;
    .work/*|task_plan.md|findings.md|progress.md)
      echo "worktree_meta"
      ;;
    *)
      echo "other"
      ;;
  esac
}

mapfile -t STATUS_LINES < <(git -C "$REPO_ROOT" status --short --untracked-files=all)
TOTAL_ENTRIES="${#STATUS_LINES[@]}"
GENERATED_AT="$(date +%Y-%m-%d\ %H:%M:%S\ %z)"

for raw in "${STATUS_LINES[@]}"; do
  path="${raw:3}"
  if [[ "$path" == *" -> "* ]]; then
    path="${path##* -> }"
  fi
  category="$(categorize_path "$path")"
  COUNTS[$category]=$((COUNTS[$category] + 1))
  printf '%s\n' "$raw" >> "$TMP_DIR/$category.txt"
done

cat > "$OUTPUT_FILE" <<EOF_REPORT
# Git Status Noise Summary

## Metadata

| field | value |
|------|-------|
| repo_root | $REPO_ROOT |
| generated_at | $GENERATED_AT |
| total_entries | $TOTAL_ENTRIES |
| max_samples | $MAX_SAMPLES |

## Summary

| category | count |
|----------|-------|
EOF_REPORT

for cat in "${CATEGORIES[@]}"; do
  printf '| %s | %s |\n' "$cat" "${COUNTS[$cat]}" >> "$OUTPUT_FILE"
done

echo >> "$OUTPUT_FILE"
echo '## Sample Entries' >> "$OUTPUT_FILE"
echo >> "$OUTPUT_FILE"

for cat in "${CATEGORIES[@]}"; do
  echo "### $cat" >> "$OUTPUT_FILE"
  if [[ "${COUNTS[$cat]}" -eq 0 ]]; then
    echo '- none' >> "$OUTPUT_FILE"
    echo >> "$OUTPUT_FILE"
    continue
  fi

  sample_count="$MAX_SAMPLES"
  if [[ "${COUNTS[$cat]}" -lt "$sample_count" ]]; then
    sample_count="${COUNTS[$cat]}"
  fi

  sed -n "1,${sample_count}p" "$TMP_DIR/$cat.txt" | while IFS= read -r line; do
    printf -- '- `%s`\n' "$line"
  done >> "$OUTPUT_FILE"

  if [[ "${COUNTS[$cat]}" -gt "$sample_count" ]]; then
    printf -- '- ... (%s more entries)\n' "$((COUNTS[$cat] - sample_count))" >> "$OUTPUT_FILE"
  fi
  echo >> "$OUTPUT_FILE"
done

echo "report: $OUTPUT_FILE"
echo "[PASS] git status noise summary completed"
