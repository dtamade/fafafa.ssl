#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
REPORTS_DIR="test-reports"
STRICT=false
OUTPUT_FILE=""

usage() {
  cat <<'USAGE'
Wave C Quick Sprint Bundle

用途：
  一次执行 B107/B108/B109/B110 门禁链路，生成冲刺交付汇总报告。

用法：
  scripts/run_wave_c_quick_sprint_bundle.sh [options]

选项：
  --run-id ID         指定 run_id
  --reports-dir DIR   报告目录（默认 test-reports）
  --output FILE       输出汇总报告
  --strict            任一步骤失败返回非 0
  --help              显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --reports-dir)
      REPORTS_DIR="$2"
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
  OUTPUT_FILE="$REPORTS_DIR/wave_c_quick_sprint_bundle_${RUN_ID}.md"
fi

mkdir -p "$REPORTS_DIR"

threshold_report="$REPORTS_DIR/wave_c_b107_threshold_eval_${RUN_ID}.md"
readiness_report="$REPORTS_DIR/wave_c_b108_default_on_readiness_${RUN_ID}.md"
canary_report="$REPORTS_DIR/wave_c_b109_canary_rollout_${RUN_ID}.md"
rollback_report="$REPORTS_DIR/wave_c_b110_rollback_drill_${RUN_ID}.md"

run_step() {
  local step="$1"
  local cmd="$2"
  local log="$3"

  set +e
  eval "$cmd" > "$log" 2>&1
  local ec=$?
  set -e

  echo "$ec"
}

threshold_log="$REPORTS_DIR/wave_c_b107_threshold_eval_${RUN_ID}.log"
readiness_log="$REPORTS_DIR/wave_c_b108_default_on_readiness_${RUN_ID}.log"
canary_log="$REPORTS_DIR/wave_c_b109_canary_rollout_${RUN_ID}.log"
rollback_log="$REPORTS_DIR/wave_c_b110_rollback_drill_${RUN_ID}.log"

threshold_exit=$(run_step "b107_threshold" \
  "bash scripts/evaluate_wave_c_b101_thresholds.sh --run-id ${RUN_ID} --strict --output ${threshold_report}" \
  "$threshold_log")

readiness_exit=$(run_step "b108_readiness" \
  "bash scripts/check_wave_c_default_on_readiness.sh --run-id ${RUN_ID} --strict --output ${readiness_report}" \
  "$readiness_log")

canary_exit=$(run_step "b109_canary" \
  "bash scripts/prepare_wave_c_b109_canary_rollout.sh --run-id ${RUN_ID} --strict --threshold-report ${threshold_report} --readiness-report ${readiness_report} --output ${canary_report}" \
  "$canary_log")

rollback_exit=$(run_step "b110_rollback_drill" \
  "bash scripts/run_wave_c_b110_rollback_drill.sh --run-id ${RUN_ID} --strict --threshold-report ${threshold_report} --readiness-report ${readiness_report} --rollout-report ${canary_report} --output ${rollback_report}" \
  "$rollback_log")

overall="PASS"
if [[ "$threshold_exit" != "0" || "$readiness_exit" != "0" || "$canary_exit" != "0" || "$rollback_exit" != "0" ]]; then
  overall="FAIL"
fi

{
  echo "# Wave C Quick Sprint Bundle"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- overall: **$overall**"
  echo
  echo "## Step Matrix"
  echo
  echo "| step | exit | report | log |"
  echo "|------|------|--------|-----|"
  echo "| B107 threshold | $threshold_exit | $threshold_report | $threshold_log |"
  echo "| B108 readiness | $readiness_exit | $readiness_report | $readiness_log |"
  echo "| B109 canary | $canary_exit | $canary_report | $canary_log |"
  echo "| B110 rollback drill | $rollback_exit | $rollback_report | $rollback_log |"
} > "$OUTPUT_FILE"

echo "[INFO] overall=$overall"
echo "[PASS] bundle report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$overall" != "PASS" ]]; then
  exit 1
fi

exit 0
