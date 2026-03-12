#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
STRICT=false
OUTPUT_FILE=""
REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-tmp/wave_c_local_guard_reports}"
QUICK_SPRINT_REPORTS_DIR="${FAFAFA_WAVE_C_QUICK_SPRINT_REPORTS_DIR:-tmp/wave_c_quick_sprint_reports}"

WORKFLOW_ENABLED_FILE=".github/workflows/wave-c-quick-sprint-manual.yml"
WORKFLOW_DISABLED_FILE=".github/workflows/wave-c-quick-sprint-manual.yml.disabled"

REQUIRED_SCRIPTS=(
  "scripts/run_wave_c_quick_sprint_bundle.sh"
  "scripts/evaluate_wave_c_b101_thresholds.sh"
  "scripts/check_wave_c_default_on_readiness.sh"
  "scripts/prepare_wave_c_b109_canary_rollout.sh"
  "scripts/run_wave_c_b110_rollback_drill.sh"
  "scripts/toggle_wave_c_quick_sprint_workflow.sh"
)

REQUIRED_DOCS=(
  "docs/test_reports/WAVE_C_B121_ONE_PAGE_RUNBOOK_2026-02-08.md"
  "docs/test_reports/WAVE_C_B122_CI_DEFERRED_LOCAL_MODE_2026-02-08.md"
)

usage() {
  cat <<'USAGE'
Wave C B123 Local-First Continuity Check

用途：
  在 CI 暂缓（workflow disabled）状态下，验证本地闭环链路是否保持可执行。

用法：
  scripts/check_wave_c_local_first_continuity.sh [options]

选项：
  --run-id ID      指定 run_id
  --output FILE    输出报告路径（默认 tmp/wave_c_local_guard_reports/wave_c_b123_local_first_continuity_<run_id>.md）
  --strict         状态非 LOCAL_READY 返回非 0
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
  OUTPUT_FILE="$REPORTS_DIR/wave_c_b123_local_first_continuity_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

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

script_fail=0
script_rows=""
for script in "${REQUIRED_SCRIPTS[@]}"; do
  if [[ -f "$script" ]]; then
    script_rows+="| $script | PASS |\n"
  else
    script_rows+="| $script | FAIL |\n"
    script_fail=1
  fi
done

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

latest_bundle="$(ls -1t "$QUICK_SPRINT_REPORTS_DIR"/wave_c_quick_sprint_bundle_*.md 2>/dev/null | head -1 || true)"
bundle_exists="FAIL"
bundle_overall="FAIL"

if [[ -n "$latest_bundle" ]]; then
  bundle_exists="PASS"
  if rg -q "overall:[[:space:]]*\*\*PASS\*\*" "$latest_bundle"; then
    bundle_overall="PASS"
  fi
fi

state="LOCAL_READY"
if [[ "$workflow_check" != "PASS" || "$script_fail" -ne 0 || "$doc_fail" -ne 0 || "$bundle_exists" != "PASS" || "$bundle_overall" != "PASS" ]]; then
  state="HOLD"
fi

{
  echo "# Wave C B123 Local-First Continuity Check"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- local_first_state: **$state**"
  echo
  echo "## Workflow Mode"
  echo
  echo "| check | value | result |"
  echo "|------|-------|--------|"
  echo "| workflow_mode | $workflow_mode | $workflow_check |"
  echo "| disabled_file | $WORKFLOW_DISABLED_FILE | $([[ -f "$WORKFLOW_DISABLED_FILE" ]] && echo PASS || echo FAIL) |"
  echo "| enabled_file | $WORKFLOW_ENABLED_FILE | $([[ -f "$WORKFLOW_ENABLED_FILE" ]] && echo PRESENT || echo ABSENT) |"
  echo
  echo "## Local Script Checks"
  echo
  echo "| script | result |"
  echo "|--------|--------|"
  printf "%b" "$script_rows"
  echo
  echo "## Documentation Checks"
  echo
  echo "| document | result |"
  echo "|----------|--------|"
  printf "%b" "$doc_rows"
  echo
  echo "## Latest Bundle Evidence"
  echo
  echo "- latest_bundle: ${latest_bundle:-<none>}"
  echo "- bundle_exists: $bundle_exists"
  echo "- bundle_overall_pass: $bundle_overall"
  echo
  echo "## Decision"
  echo
  if [[ "$state" == "LOCAL_READY" ]]; then
    echo "- 本地优先模式可持续执行，建议继续推进非 CI 交付。"
  else
    echo "- 当前不满足本地连续性条件，需先修复 FAIL 项后再推进。"
  fi
} > "$OUTPUT_FILE"

echo "[INFO] local_first_state=$state"
echo "[PASS] report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$state" != "LOCAL_READY" ]]; then
  exit 1
fi

exit 0
