#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
DEFAULT_REPORTS_DIR="tmp/wave_c_quick_sprint_reports"
REPORTS_DIR="${FAFAFA_WAVE_C_QUICK_SPRINT_REPORTS_DIR:-$DEFAULT_REPORTS_DIR}"
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
  --reports-dir DIR   报告目录（默认 tmp/wave_c_quick_sprint_reports）
  --output FILE       输出汇总报告（默认 tmp/wave_c_quick_sprint_reports/wave_c_quick_sprint_bundle_<run_id>.md）
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
  local log="$1"
  shift

  set +e
  "$@" > "$log" 2>&1
  local ec=$?
  set -e

  echo "$ec"
}

threshold_log="$REPORTS_DIR/wave_c_b107_threshold_eval_${RUN_ID}.log"
readiness_log="$REPORTS_DIR/wave_c_b108_default_on_readiness_${RUN_ID}.log"
canary_log="$REPORTS_DIR/wave_c_b109_canary_rollout_${RUN_ID}.log"
rollback_log="$REPORTS_DIR/wave_c_b110_rollback_drill_${RUN_ID}.log"

threshold_exit=$(run_step "$threshold_log" \
  bash scripts/evaluate_wave_c_b101_thresholds.sh \
  --reports-dir "$REPORTS_DIR" \
  --run-id "$RUN_ID" \
  --strict \
  --output "$threshold_report")

readiness_exit=$(run_step "$readiness_log" \
  bash scripts/check_wave_c_default_on_readiness.sh \
  --reports-dir "$REPORTS_DIR" \
  --run-id "$RUN_ID" \
  --strict \
  --output "$readiness_report")

canary_exit=$(run_step "$canary_log" \
  bash scripts/prepare_wave_c_b109_canary_rollout.sh \
  --reports-dir "$REPORTS_DIR" \
  --run-id "$RUN_ID" \
  --strict \
  --threshold-report "$threshold_report" \
  --readiness-report "$readiness_report" \
  --output "$canary_report")

rollback_exit=$(run_step "$rollback_log" \
  bash scripts/run_wave_c_b110_rollback_drill.sh \
  --reports-dir "$REPORTS_DIR" \
  --run-id "$RUN_ID" \
  --strict \
  --threshold-report "$threshold_report" \
  --readiness-report "$readiness_report" \
  --rollout-report "$canary_report" \
  --output "$rollback_report")

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
