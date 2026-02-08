#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
STRICT=false
OUTPUT_FILE=""

usage() {
  cat <<'USAGE'
Wave C B144 Local Guard Ops Pack

用途：
  一次执行 B138/B140/B142/B143/B139(dry-run) 并输出运维打包报告。

用法：
  scripts/run_wave_c_local_guard_ops_pack.sh [options]

选项：
  --run-id ID      指定 run_id
  --output FILE    输出报告路径
  --strict         任一步骤失败返回非 0
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
  OUTPUT_FILE="test-reports/wave_c_b144_local_guard_ops_pack_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

b138_report="test-reports/wave_c_b138_pre_ci_reenable_full_gate_${RUN_ID}.md"
b140_report="test-reports/wave_c_b140_local_guard_consistency_${RUN_ID}.md"
b142_json="test-reports/wave_c_b142_local_guard_status_${RUN_ID}.json"
b143_report="test-reports/wave_c_b143_alert_thresholds_${RUN_ID}.md"
b139_report="test-reports/wave_c_b139_local_guard_cleanup_plan_${RUN_ID}.md"

run_step() {
  local cmd="$1"
  local log="$2"

  set +e
  eval "$cmd" > "$log" 2>&1
  local ec=$?
  set -e

  echo "$ec"
}

b138_log="test-reports/wave_c_b138_pre_ci_reenable_full_gate_${RUN_ID}.b144.log"
b140_log="test-reports/wave_c_b140_local_guard_consistency_${RUN_ID}.b144.log"
b142_log="test-reports/wave_c_b142_local_guard_status_${RUN_ID}.b144.log"
b143_log="test-reports/wave_c_b143_alert_thresholds_${RUN_ID}.b144.log"
b139_log="test-reports/wave_c_b139_local_guard_cleanup_plan_${RUN_ID}.b144.log"

b138_exit=$(run_step "bash scripts/run_wave_c_pre_ci_reenable_full_gate.sh --run-id ${RUN_ID} --strict --output ${b138_report}" "$b138_log")
b140_exit=$(run_step "bash scripts/check_wave_c_local_guard_consistency.sh --run-id ${RUN_ID} --strict --output ${b140_report}" "$b140_log")
b142_exit=$(run_step "bash scripts/export_wave_c_local_guard_status_json.sh --run-id ${RUN_ID} --strict --output ${b142_json}" "$b142_log")
b143_exit=$(run_step "bash scripts/check_wave_c_local_guard_alert_thresholds.sh --run-id ${RUN_ID} --strict --input ${b142_json} --output ${b143_report}" "$b143_log")
b139_exit=$(run_step "bash scripts/cleanup_wave_c_local_guard_reports.sh --run-id ${RUN_ID} --output ${b139_report}" "$b139_log")

overall="PASS"
if [[ "$b138_exit" != "0" || "$b140_exit" != "0" || "$b142_exit" != "0" || "$b143_exit" != "0" || "$b139_exit" != "0" ]]; then
  overall="FAIL"
fi

{
  echo "# Wave C B144 Local Guard Ops Pack"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- overall: **$overall**"
  echo
  echo "## Step Matrix"
  echo
  echo "| step | exit | output | log |"
  echo "|------|------|--------|-----|"
  echo "| B138 full gate | $b138_exit | $b138_report | $b138_log |"
  echo "| B140 consistency | $b140_exit | $b140_report | $b140_log |"
  echo "| B142 status json | $b142_exit | $b142_json | $b142_log |"
  echo "| B143 alert thresholds | $b143_exit | $b143_report | $b143_log |"
  echo "| B139 cleanup plan | $b139_exit | $b139_report | $b139_log |"
} > "$OUTPUT_FILE"

echo "[INFO] overall=$overall"
echo "[PASS] ops pack report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$overall" != "PASS" ]]; then
  exit 1
fi

exit 0
