#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

ARTIFACT_ROOT="$PROJECT_ROOT/artifacts/ci"
PROFILE="pr"
OLDER_THAN_DAYS=""
DRY_RUN=true
VERBOSE=false

usage() {
  cat <<'USAGE'
CI 归档清理脚本（Draft）

安全默认：
  - 默认 dry-run（仅打印计划）
  - 仅在 --apply 时执行删除

用法：
  scripts/cleanup_ci_artifacts_draft.sh [options]

选项：
  --root DIR             归档根目录（默认: artifacts/ci）
  --profile NAME         策略配置（pr|nightly|release，默认: pr）
  --older-than-days N    覆盖 profile 默认天数
  --apply                执行删除（默认 dry-run）
  --dry-run              强制 dry-run
  --verbose              输出更多细节
  --help                 显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --root)
      ARTIFACT_ROOT="$2"
      shift 2
      ;;
    --profile)
      PROFILE="$2"
      shift 2
      ;;
    --older-than-days)
      OLDER_THAN_DAYS="$2"
      shift 2
      ;;
    --apply)
      DRY_RUN=false
      shift
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

case "$PROFILE" in
  pr)
    DEFAULT_DAYS=30
    ;;
  nightly)
    DEFAULT_DAYS=14
    ;;
  release)
    DEFAULT_DAYS=90
    ;;
  *)
    echo "[FAIL] unsupported profile: $PROFILE" >&2
    exit 1
    ;;
esac

if [[ -z "$OLDER_THAN_DAYS" ]]; then
  OLDER_THAN_DAYS="$DEFAULT_DAYS"
fi

if ! [[ "$OLDER_THAN_DAYS" =~ ^[0-9]+$ ]]; then
  echo "[FAIL] --older-than-days must be a non-negative integer" >&2
  exit 1
fi

if [[ ! -d "$ARTIFACT_ROOT" ]]; then
  echo "[WARN] artifact root not found: $ARTIFACT_ROOT"
  echo "[PASS] nothing to clean"
  exit 0
fi

NOW_EPOCH="$(date +%s)"
CANDIDATES=0
SKIPPED_HOLD=0
DELETED=0

is_hold_marked() {
  local run_dir="$1"
  if [[ -f "$run_dir/.hold" ]]; then
    return 0
  fi

  if [[ -f "$run_dir/manifest.md" ]] && grep -qiE 'hold\s*[:=]\s*true' "$run_dir/manifest.md"; then
    return 0
  fi

  return 1
}

echo "========================================"
echo "fafafa.ssl CI Artifact Cleanup (Draft)"
echo "========================================"
echo "root: $ARTIFACT_ROOT"
echo "profile: $PROFILE"
echo "older-than-days: $OLDER_THAN_DAYS"
echo "dry-run: $DRY_RUN"

while IFS= read -r run_dir; do
  [[ -z "$run_dir" ]] && continue

  # 只处理一级目录（每个 run_id）
  if [[ ! -d "$run_dir" ]]; then
    continue
  fi

  run_name="$(basename "$run_dir")"
  mtime_epoch="$(stat -c %Y "$run_dir" 2>/dev/null || echo 0)"
  age_days=$(( (NOW_EPOCH - mtime_epoch) / 86400 ))

  if (( age_days < OLDER_THAN_DAYS )); then
    if [[ "$VERBOSE" == "true" ]]; then
      echo "[KEEP] $run_name age=${age_days}d < ${OLDER_THAN_DAYS}d"
    fi
    continue
  fi

  if is_hold_marked "$run_dir"; then
    SKIPPED_HOLD=$((SKIPPED_HOLD + 1))
    echo "[SKIP-HOLD] $run_name age=${age_days}d"
    continue
  fi

  CANDIDATES=$((CANDIDATES + 1))
  echo "[CANDIDATE] $run_name age=${age_days}d"

  if [[ "$DRY_RUN" == "false" ]]; then
    rm -rf "$run_dir"
    DELETED=$((DELETED + 1))
    echo "[DELETED] $run_name"
  fi
done < <(find "$ARTIFACT_ROOT" -mindepth 1 -maxdepth 1 -type d | sort)

echo "summary: candidates=$CANDIDATES skipped_hold=$SKIPPED_HOLD deleted=$DELETED"

if [[ "$DRY_RUN" == "true" ]]; then
  echo "[PASS] cleanup draft dry-run finished"
else
  echo "[PASS] cleanup draft apply finished"
fi
