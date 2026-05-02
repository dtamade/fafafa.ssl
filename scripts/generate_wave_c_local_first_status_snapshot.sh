#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
OUTPUT_FILE=""
STRICT=false

usage() {
  cat <<'USAGE'
Wave C B132 Local-first Status Snapshot

用途：
  生成 local-first 当前状态单页快照，便于交接与汇报。

用法：
  scripts/generate_wave_c_local_first_status_snapshot.sh [options]

选项：
  --run-id ID      指定 run_id
  --output FILE    输出报告路径
  --strict         检查失败时返回非 0
  --help           显示帮助
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

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="tmp/test-reports/wave_c_b132_local_first_status_snapshot_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

find_latest_wave_c_report() {
  local pattern="$1"
  local candidate=""
  for root in tmp/test-reports test-reports docs/test_reports; do
    candidate="$(ls -1t "$root"/$pattern 2>/dev/null | head -1 || true)"
    if [[ -n "$candidate" ]]; then
      echo "$candidate"
      return 0
    fi
  done
  echo ""
}

extract_run_suffix() {
  local file="$1"
  local prefix="$2"
  local name

  name="$(basename "$file")"
  name="${name#${prefix}}"
  name="${name%.md}"
  echo "$name"
}

find_latest_complete_local_guard_run() {
  local root=""
  local latest_b129=""
  local run_suffix=""

  for root in tmp/test-reports test-reports docs/test_reports; do
    while IFS= read -r latest_b129; do
      [[ -z "$latest_b129" ]] && continue
      run_suffix="$(extract_run_suffix "$latest_b129" "wave_c_b129_oncall_check_")"

      if [[ -f "$root/wave_c_b123_local_first_continuity_${run_suffix}.md" && \
            -f "$root/wave_c_b124_local_drift_watch_${run_suffix}.md" && \
            -f "$root/wave_c_b125_local_guard_bundle_${run_suffix}.md" && \
            -f "$root/wave_c_b126_local_guard_history_${run_suffix}.md" ]]; then
        echo "$root|$run_suffix"
        return 0
      fi
    done < <(ls -1t "$root"/wave_c_b129_oncall_check_*.md 2>/dev/null || true)
  done

  echo ""
}

workflow_state="UNKNOWN"
if [[ -f ".github/workflows/wave-c-quick-sprint-manual.yml.disabled" && ! -f ".github/workflows/wave-c-quick-sprint-manual.yml" ]]; then
  workflow_state="DISABLED"
elif [[ -f ".github/workflows/wave-c-quick-sprint-manual.yml" ]]; then
  workflow_state="ENABLED"
fi

complete_guard_run="$(find_latest_complete_local_guard_run)"

if [[ -n "$complete_guard_run" ]]; then
  IFS='|' read -r evidence_root evidence_run_suffix <<< "$complete_guard_run"
  latest_b123="$evidence_root/wave_c_b123_local_first_continuity_${evidence_run_suffix}.md"
  latest_b124="$evidence_root/wave_c_b124_local_drift_watch_${evidence_run_suffix}.md"
  latest_b125="$evidence_root/wave_c_b125_local_guard_bundle_${evidence_run_suffix}.md"
  latest_b126="$evidence_root/wave_c_b126_local_guard_history_${evidence_run_suffix}.md"
  latest_b129="$evidence_root/wave_c_b129_oncall_check_${evidence_run_suffix}.md"
else
  latest_b123="$(find_latest_wave_c_report 'wave_c_b123_local_first_continuity_*.md')"
  latest_b124="$(find_latest_wave_c_report 'wave_c_b124_local_drift_watch_*.md')"
  latest_b125="$(find_latest_wave_c_report 'wave_c_b125_local_guard_bundle_*.md')"
  latest_b126="$(find_latest_wave_c_report 'wave_c_b126_local_guard_history_*.md')"
  latest_b129="$(find_latest_wave_c_report 'wave_c_b129_oncall_check_*.md')"
fi

extract_marked_state() {
  local file="$1"
  local key="$2"
  if [[ -z "$file" || ! -f "$file" ]]; then
    echo "MISSING"
    return 0
  fi

  local value
  value="$(rg -o "${key}:[[:space:]]*\*\*[A-Z_]+\*\*" "$file" | head -1 | sed -E 's/.*\*\*([A-Z_]+)\*\*/\1/' || true)"
  echo "${value:-UNKNOWN}"
}

b123_state="$(extract_marked_state "$latest_b123" "local_first_state")"
b124_state="$(extract_marked_state "$latest_b124" "local_drift_state")"
b125_state="$(extract_marked_state "$latest_b125" "overall")"
b126_state="$(extract_marked_state "$latest_b126" "trend_state")"
b129_state="$(extract_marked_state "$latest_b129" "overall")"

snapshot_state="GREEN"
if [[ "$workflow_state" != "DISABLED" || "$b123_state" != "LOCAL_READY" || "$b124_state" != "LOCAL_STABLE" || "$b125_state" != "PASS" || "$b126_state" != "STABLE" || "$b129_state" != "PASS" ]]; then
  snapshot_state="ATTENTION"
fi

{
  echo "# Wave C B132 Local-first Status Snapshot"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- snapshot_state: **$snapshot_state**"
  echo
  echo "## Current Guard Status"
  echo
  echo "| item | state | expected | result |"
  echo "|------|-------|----------|--------|"
  echo "| workflow_mode | $workflow_state | DISABLED | $([[ "$workflow_state" == "DISABLED" ]] && echo PASS || echo FAIL) |"
  echo "| B123 continuity | $b123_state | LOCAL_READY | $([[ "$b123_state" == "LOCAL_READY" ]] && echo PASS || echo FAIL) |"
  echo "| B124 drift watch | $b124_state | LOCAL_STABLE | $([[ "$b124_state" == "LOCAL_STABLE" ]] && echo PASS || echo FAIL) |"
  echo "| B125 guard bundle | $b125_state | PASS | $([[ "$b125_state" == "PASS" ]] && echo PASS || echo FAIL) |"
  echo "| B126 history trend | $b126_state | STABLE | $([[ "$b126_state" == "STABLE" ]] && echo PASS || echo FAIL) |"
  echo "| B129 oncall check | $b129_state | PASS | $([[ "$b129_state" == "PASS" ]] && echo PASS || echo FAIL) |"
  echo
  echo "## Latest Evidence"
  echo
  echo "- B123: ${latest_b123:-<none>}"
  echo "- B124: ${latest_b124:-<none>}"
  echo "- B125: ${latest_b125:-<none>}"
  echo "- B126: ${latest_b126:-<none>}"
  echo "- B129: ${latest_b129:-<none>}"
} > "$OUTPUT_FILE"

echo "[INFO] snapshot_state=$snapshot_state"
echo "[PASS] report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$snapshot_state" != "GREEN" ]]; then
  exit 1
fi

exit 0
