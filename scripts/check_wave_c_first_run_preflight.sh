#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
STRICT=false
OUTPUT_FILE=""

WORKFLOW_FILE=".github/workflows/wave-c-quick-sprint-manual.yml"
REQUIRED_SCRIPTS=(
  "scripts/run_wave_c_quick_sprint_bundle.sh"
  "scripts/evaluate_wave_c_b101_thresholds.sh"
  "scripts/check_wave_c_default_on_readiness.sh"
  "scripts/prepare_wave_c_b109_canary_rollout.sh"
  "scripts/run_wave_c_b110_rollback_drill.sh"
)

usage() {
  cat <<'USAGE'
Wave C First-Run Preflight

用法：
  scripts/check_wave_c_first_run_preflight.sh [options]

选项：
  --run-id ID      指定 run_id
  --output FILE    输出报告路径
  --strict         非 READY 时返回非 0
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
  OUTPUT_FILE="test-reports/wave_c_b119_first_run_preflight_${RUN_ID}.md"
fi

workflow_status="FAIL"
if [[ -f "$WORKFLOW_FILE" ]]; then
  workflow_status="PASS"
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

latest_bundle="$(ls -1t test-reports/wave_c_quick_sprint_bundle_*.md 2>/dev/null | head -1 || true)"
bundle_status="PASS"
if [[ -z "$latest_bundle" ]]; then
  bundle_status="FAIL"
fi

state="READY"
if [[ "$workflow_status" != "PASS" || "$script_fail" -ne 0 || "$bundle_status" != "PASS" ]]; then
  state="HOLD"
fi

{
  echo "# Wave C B119 First-Run Preflight"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- state: **$state**"
  echo
  echo "## Core Checks"
  echo
  echo "| check | result |"
  echo "|------|--------|"
  echo "| workflow_enabled_file ($WORKFLOW_FILE) | $workflow_status |"
  echo "| latest_bundle_exists | $bundle_status |"
  echo
  echo "- latest_bundle: ${latest_bundle:-<none>}"
  echo
  echo "## Script Checks"
  echo
  echo "| script | result |"
  echo "|--------|--------|"
  printf "%b" "$script_rows"
} > "$OUTPUT_FILE"

echo "[INFO] state=$state"
echo "[PASS] report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$state" != "READY" ]]; then
  exit 1
fi

exit 0
