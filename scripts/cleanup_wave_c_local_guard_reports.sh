#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
OUTPUT_FILE=""
APPLY=false
KEEP_TIER1=20
KEEP_TIER2=50

usage() {
  cat <<'USAGE'
Wave C B139 Local Guard Cleanup Planner

用途：
  按 local-first 证据分层策略生成清理计划；可选执行清理。

用法：
  scripts/cleanup_wave_c_local_guard_reports.sh [options]

选项：
  --run-id ID        指定 run_id
  --output FILE      输出报告路径
  --keep-tier1 N     Tier1 保留份数（默认 20）
  --keep-tier2 N     Tier2 保留份数（默认 50）
  --apply            执行实际删除（默认仅 dry-run）
  --help             显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --output)
      OUTPUT_FILE="$2"
      shift 2
      ;;
    --keep-tier1)
      KEEP_TIER1="$2"
      shift 2
      ;;
    --keep-tier2)
      KEEP_TIER2="$2"
      shift 2
      ;;
    --apply)
      APPLY=true
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

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="tmp/test-reports/wave_c_b139_local_guard_cleanup_plan_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

tier1_patterns=(
  "tmp/test-reports/wave_c_b129_oncall_check_*.md"
  "tmp/test-reports/wave_c_b125_local_guard_bundle_*.md"
  "test-reports/wave_c_b129_oncall_check_*.md"
  "test-reports/wave_c_b125_local_guard_bundle_*.md"
)

tier2_patterns=(
  "tmp/test-reports/wave_c_b126_local_guard_history_*.md"
  "tmp/test-reports/wave_c_b124_local_drift_watch_*.md"
  "test-reports/wave_c_b126_local_guard_history_*.md"
  "test-reports/wave_c_b124_local_drift_watch_*.md"
)

gather_delete_candidates() {
  local keep_n="$1"
  shift
  local patterns=("$@")
  local candidates=()

  for pattern in "${patterns[@]}"; do
    mapfile -t files < <(ls -1t $pattern 2>/dev/null || true)
    if [[ ${#files[@]} -le $keep_n ]]; then
      continue
    fi
    for ((i=keep_n; i<${#files[@]}; i++)); do
      candidates+=("${files[$i]}")
    done
  done

  printf '%s\n' "${candidates[@]}" | sed '/^$/d' | sort -u
}

mapfile -t tier1_delete < <(gather_delete_candidates "$KEEP_TIER1" "${tier1_patterns[@]}")
mapfile -t tier2_delete < <(gather_delete_candidates "$KEEP_TIER2" "${tier2_patterns[@]}")

deleted=0
if [[ "$APPLY" == "true" ]]; then
  for file in "${tier1_delete[@]}" "${tier2_delete[@]}"; do
    if [[ -n "$file" && -f "$file" ]]; then
      rm -f -- "$file"
      deleted=$((deleted + 1))
    fi
  done
fi

total_tier1=${#tier1_delete[@]}
total_tier2=${#tier2_delete[@]}
total_candidates=$((total_tier1 + total_tier2))

mode="DRY_RUN"
if [[ "$APPLY" == "true" ]]; then
  mode="APPLY"
fi

{
  echo "# Wave C B139 Local Guard Cleanup Plan"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- mode: $mode"
  echo "- keep_tier1: $KEEP_TIER1"
  echo "- keep_tier2: $KEEP_TIER2"
  echo
  echo "## Summary"
  echo
  echo "- tier1_candidates: $total_tier1"
  echo "- tier2_candidates: $total_tier2"
  echo "- total_candidates: $total_candidates"
  echo "- deleted: $deleted"
  echo
  echo "## Tier1 Candidates"
  echo
  if [[ $total_tier1 -eq 0 ]]; then
    echo "- <none>"
  else
    for file in "${tier1_delete[@]}"; do
      echo "- $file"
    done
  fi
  echo
  echo "## Tier2 Candidates"
  echo
  if [[ $total_tier2 -eq 0 ]]; then
    echo "- <none>"
  else
    for file in "${tier2_delete[@]}"; do
      echo "- $file"
    done
  fi
} > "$OUTPUT_FILE"

echo "[INFO] mode=$mode candidates=$total_candidates deleted=$deleted"
echo "[PASS] report generated: $OUTPUT_FILE"

exit 0
