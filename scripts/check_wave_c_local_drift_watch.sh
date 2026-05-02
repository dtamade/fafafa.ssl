#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
STRICT=false
OUTPUT_FILE=""
MAX_BUNDLE_AGE_HOURS=168
MAX_CONTINUITY_AGE_HOURS=24
MAX_DRIFT_CHECK_GAP_HOURS=24

WORKFLOW_ENABLED_FILE=".github/workflows/wave-c-quick-sprint-manual.yml"
WORKFLOW_DISABLED_FILE=".github/workflows/wave-c-quick-sprint-manual.yml.disabled"

REQUIRED_DOCS=(
  "docs/test_reports/WAVE_C_B121_ONE_PAGE_RUNBOOK_2026-02-08.md"
  "docs/test_reports/WAVE_C_B122_CI_DEFERRED_LOCAL_MODE_2026-02-08.md"
  "docs/test_reports/WAVE_C_B123_LOCAL_FIRST_CONTINUITY_RESULT_2026-02-09.md"
)

usage() {
  cat <<'USAGE'
Wave C B124 Local-First Drift Watch

用途：
  在 CI 暂缓期间执行周期性漂移检查，确认 local-first 门禁持续有效。

用法：
  scripts/check_wave_c_local_drift_watch.sh [options]

选项：
  --run-id ID                       指定 run_id
  --output FILE                     输出报告路径
  --max-bundle-age-hours N          latest bundle 最大允许时效（默认 168）
  --max-continuity-age-hours N      B123 continuity 报告最大允许时效（默认 24）
  --max-drift-check-gap-hours N     B124 检查最大间隔（默认 24）
  --strict                          状态非 LOCAL_STABLE 返回非 0
  --help                            显示帮助
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
    --max-bundle-age-hours)
      MAX_BUNDLE_AGE_HOURS="$2"
      shift 2
      ;;
    --max-continuity-age-hours)
      MAX_CONTINUITY_AGE_HOURS="$2"
      shift 2
      ;;
    --max-drift-check-gap-hours)
      MAX_DRIFT_CHECK_GAP_HOURS="$2"
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
  OUTPUT_FILE="tmp/test-reports/wave_c_b124_local_drift_watch_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

now_epoch="$(date +%s)"

file_mtime_epoch() {
  local file="$1"

  if stat -c %Y "$file" >/dev/null 2>&1; then
    stat -c %Y "$file"
    return 0
  fi

  if stat -f %m "$file" >/dev/null 2>&1; then
    stat -f %m "$file"
    return 0
  fi

  return 1
}

file_age_hours() {
  local file="$1"
  local mtime

  if ! mtime="$(file_mtime_epoch "$file")"; then
    echo "-1"
    return 0
  fi

  echo $(( (now_epoch - mtime) / 3600 ))
}

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

find_latest_tmp_wave_c_report() {
  local pattern="$1"
  local candidate=""
  candidate="$(ls -1t tmp/test-reports/$pattern 2>/dev/null | head -1 || true)"
  if [[ -n "$candidate" ]]; then
    echo "$candidate"
    return 0
  fi
  echo ""
}

workflow_mode="MISSING"
workflow_check="FAIL"
if [[ -f "$WORKFLOW_DISABLED_FILE" && ! -f "$WORKFLOW_ENABLED_FILE" ]]; then
  workflow_mode="DISABLED"
  workflow_check="PASS"
elif [[ -f "$WORKFLOW_ENABLED_FILE" && ! -f "$WORKFLOW_DISABLED_FILE" ]]; then
  workflow_mode="ENABLED"
  workflow_check="FAIL"
elif [[ -f "$WORKFLOW_ENABLED_FILE" && -f "$WORKFLOW_DISABLED_FILE" ]]; then
  workflow_mode="AMBIGUOUS"
  workflow_check="FAIL"
fi

latest_continuity="$(find_latest_wave_c_report 'wave_c_b123_local_first_continuity_*.md')"
continuity_exists="FAIL"
continuity_state="UNKNOWN"
continuity_age_check="FAIL"
continuity_age_hours="-1"

if [[ -n "$latest_continuity" ]]; then
  continuity_exists="PASS"
  continuity_age_hours="$(file_age_hours "$latest_continuity")"

  if rg -q "local_first_state:[[:space:]]*\*\*LOCAL_READY\*\*" "$latest_continuity"; then
    continuity_state="LOCAL_READY"
  else
    continuity_state="HOLD_OR_UNKNOWN"
  fi

  if [[ "$continuity_age_hours" -ge 0 && "$continuity_age_hours" -le "$MAX_CONTINUITY_AGE_HOURS" ]]; then
    continuity_age_check="PASS"
  fi
fi

latest_bundle="$(find_latest_wave_c_report 'wave_c_quick_sprint_bundle_*.md')"
bundle_exists="FAIL"
bundle_overall="FAIL"
bundle_age_hours="-1"
bundle_age_check="FAIL"

if [[ -n "$latest_bundle" ]]; then
  bundle_exists="PASS"
  bundle_age_hours="$(file_age_hours "$latest_bundle")"

  if rg -q "overall:[[:space:]]*\*\*PASS\*\*" "$latest_bundle"; then
    bundle_overall="PASS"
  fi

  if [[ "$bundle_age_hours" -ge 0 && "$bundle_age_hours" -le "$MAX_BUNDLE_AGE_HOURS" ]]; then
    bundle_age_check="PASS"
  fi
fi

latest_prev_drift="$(find_latest_tmp_wave_c_report 'wave_c_b124_local_drift_watch_*.md')"
drift_gap_check="BOOTSTRAP"
drift_gap_hours="-1"

if [[ -n "$latest_prev_drift" ]]; then
  drift_gap_hours="$(file_age_hours "$latest_prev_drift")"
  drift_gap_check="FAIL"
  if [[ "$drift_gap_hours" -ge 0 && "$drift_gap_hours" -le "$MAX_DRIFT_CHECK_GAP_HOURS" ]]; then
    drift_gap_check="PASS"
  fi
fi

doc_fail=0
doc_rows=""
for doc in "${REQUIRED_DOCS[@]}"; do
  if [[ -f "$doc" ]]; then
    doc_rows+="| $doc | PASS |\n"
  else
    doc_rows+="| $doc | FAIL |\n"
    doc_fail=1
  fi
done

state="LOCAL_STABLE"
if [[ "$workflow_check" != "PASS" || "$continuity_exists" != "PASS" || "$continuity_state" != "LOCAL_READY" || "$continuity_age_check" != "PASS" || "$bundle_exists" != "PASS" || "$bundle_overall" != "PASS" || "$bundle_age_check" != "PASS" || "$doc_fail" -ne 0 ]]; then
  state="HOLD"
fi

{
  echo "# Wave C B124 Local-First Drift Watch"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- local_drift_state: **$state**"
  echo
  echo "## Workflow Guard"
  echo
  echo "| check | value | result |"
  echo "|------|-------|--------|"
  echo "| workflow_mode | $workflow_mode | $workflow_check |"
  echo "| disabled_file | $WORKFLOW_DISABLED_FILE | $([[ -f "$WORKFLOW_DISABLED_FILE" ]] && echo PASS || echo FAIL) |"
  echo "| enabled_file | $WORKFLOW_ENABLED_FILE | $([[ -f "$WORKFLOW_ENABLED_FILE" ]] && echo PRESENT || echo ABSENT) |"
  echo
  echo "## Freshness Checks"
  echo
  echo "| check | value | threshold | result |"
  echo "|------|-------|-----------|--------|"
  echo "| latest_continuity_file | ${latest_continuity:-<none>} | required | $continuity_exists |"
  echo "| continuity_state | $continuity_state | LOCAL_READY | $([[ "$continuity_state" == "LOCAL_READY" ]] && echo PASS || echo FAIL) |"
  echo "| continuity_age_hours | $continuity_age_hours | <= $MAX_CONTINUITY_AGE_HOURS | $continuity_age_check |"
  echo "| latest_bundle_file | ${latest_bundle:-<none>} | required | $bundle_exists |"
  echo "| bundle_overall | $bundle_overall | PASS | $bundle_overall |"
  echo "| bundle_age_hours | $bundle_age_hours | <= $MAX_BUNDLE_AGE_HOURS | $bundle_age_check |"
  echo "| previous_b124_gap_hours | $drift_gap_hours | <= $MAX_DRIFT_CHECK_GAP_HOURS | $drift_gap_check |"
  echo
  echo "## Documentation Checks"
  echo
  echo "| document | result |"
  echo "|----------|--------|"
  printf "%b" "$doc_rows"
  echo
  echo "## Periodic Checklist (Local-only)"
  echo
  echo "- 每日：执行 B124 strict，确认 local_drift_state=LOCAL_STABLE。"
  echo "- 每日：执行 B123 strict，确认 local_first_state=LOCAL_READY。"
  echo "- 每周：复核 latest bundle 时效，必要时刷新一次 local guard bundle。"
  echo "- 任意时刻：若 workflow 漂移到 enabled，立即执行 disable 并重跑 B123/B124。"
} > "$OUTPUT_FILE"

echo "[INFO] local_drift_state=$state"
echo "[PASS] report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$state" != "LOCAL_STABLE" ]]; then
  exit 1
fi

exit 0
