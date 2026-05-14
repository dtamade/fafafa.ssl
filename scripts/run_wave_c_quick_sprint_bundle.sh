#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
REPORTS_DIR="test-reports"
REPORT_GLOB="wave_c_b101_validation_*.md"
REQUIRE_FULL_GATE=false
VALIDATION_REPORT=""
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
  --report-glob GLOB  B101 报告匹配模式（默认 wave_c_b101_validation_*.md）
  --require-full-gate B107 仅统计 full-gate B101 报告
  --validation-report FILE 指定 B101 验证报告（默认最新）
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
    --report-glob)
      REPORT_GLOB="$2"
      shift 2
      ;;
    --require-full-gate)
      REQUIRE_FULL_GATE=true
      shift
      ;;
    --validation-report)
      VALIDATION_REPORT="$2"
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

if [[ -z "$VALIDATION_REPORT" ]]; then
  VALIDATION_REPORT="$(ls -1t "$REPORTS_DIR"/wave_c_b101_validation_*.md 2>/dev/null | head -1 || true)"
fi

mkdir -p "$REPORTS_DIR"

threshold_report="$REPORTS_DIR/wave_c_b107_threshold_eval_${RUN_ID}.md"
readiness_report="$REPORTS_DIR/wave_c_b108_default_on_readiness_${RUN_ID}.md"
canary_report="$REPORTS_DIR/wave_c_b109_canary_rollout_${RUN_ID}.md"
rollback_report="$REPORTS_DIR/wave_c_b110_rollback_drill_${RUN_ID}.md"

shell_join() {
  local parts=()
  local part
  for part in "$@"; do
    parts+=("$(printf '%q' "$part")")
  done
  local IFS=' '
  echo "${parts[*]}"
}

run_step() {
  local step_name="$1"
  local log="$2"
  local cmd_desc="$3"
  shift 3

  echo "[wave-c-quick-sprint] [$step_name] $cmd_desc" >&2

  set +e
  "$@" > "$log" 2>&1
  local ec=$?
  set -e

  echo "[wave-c-quick-sprint] [$step_name] exit=$ec log=$log" >&2
  echo "$ec"
}

threshold_log="$REPORTS_DIR/wave_c_b107_threshold_eval_${RUN_ID}.log"
readiness_log="$REPORTS_DIR/wave_c_b108_default_on_readiness_${RUN_ID}.log"
canary_log="$REPORTS_DIR/wave_c_b109_canary_rollout_${RUN_ID}.log"
rollback_log="$REPORTS_DIR/wave_c_b110_rollback_drill_${RUN_ID}.log"

threshold_cmd_words=(
  bash
  scripts/evaluate_wave_c_b101_thresholds.sh
  --reports-dir "$REPORTS_DIR"
  --report-glob "$REPORT_GLOB"
)
if [[ "$REQUIRE_FULL_GATE" == "true" ]]; then
  threshold_cmd_words+=(--require-full-gate)
fi
threshold_cmd_words+=(
  --run-id "$RUN_ID"
  --strict
  --output "$threshold_report"
)

readiness_cmd_words=(
  bash
  scripts/check_wave_c_default_on_readiness.sh
  --reports-dir "$REPORTS_DIR"
  --threshold-report "$threshold_report"
  --validation-report "$VALIDATION_REPORT"
  --run-id "$RUN_ID"
  --strict
  --output "$readiness_report"
)

canary_cmd_words=(
  bash
  scripts/prepare_wave_c_b109_canary_rollout.sh
  --reports-dir "$REPORTS_DIR"
  --run-id "$RUN_ID"
  --strict
  --threshold-report "$threshold_report"
  --readiness-report "$readiness_report"
  --validation-report "$VALIDATION_REPORT"
  --output "$canary_report"
)

rollback_cmd_words=(
  bash
  scripts/run_wave_c_b110_rollback_drill.sh
  --reports-dir "$REPORTS_DIR"
  --run-id "$RUN_ID"
  --strict
  --threshold-report "$threshold_report"
  --readiness-report "$readiness_report"
  --rollout-report "$canary_report"
  --validation-report "$VALIDATION_REPORT"
  --output "$rollback_report"
)

threshold_exit=$(run_step "b107_threshold" "$threshold_log" "$(shell_join "${threshold_cmd_words[@]}")" "${threshold_cmd_words[@]}")
readiness_exit=$(run_step "b108_readiness" "$readiness_log" "$(shell_join "${readiness_cmd_words[@]}")" "${readiness_cmd_words[@]}")
canary_exit=$(run_step "b109_canary" "$canary_log" "$(shell_join "${canary_cmd_words[@]}")" "${canary_cmd_words[@]}")
rollback_exit=$(run_step "b110_rollback_drill" "$rollback_log" "$(shell_join "${rollback_cmd_words[@]}")" "${rollback_cmd_words[@]}")

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
