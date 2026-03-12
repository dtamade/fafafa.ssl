#!/usr/bin/env bash

set -euo pipefail

RUN_ID="$(date +%Y%m%d_%H%M%S)"
STRICT=false
DEFAULT_REPORTS_DIR="tmp/wave_c_local_guard_reports"
REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-$DEFAULT_REPORTS_DIR}"
OUTPUT_FILE=""
WITH_PLATFORM_PATH_CHECKS_DRYRUN=true
ONLY_PLATFORM_PATH_CHECK_DRYRUN=false

usage() {
  cat <<'USAGE'
Wave C B144 Local Guard Ops Pack

用途：
  一次执行 B138/B140/B142/B143/B139(dry-run) 并输出运维打包报告。

用法：
  scripts/run_wave_c_local_guard_ops_pack.sh [options]

选项：
  --run-id ID      指定 run_id
  --reports-dir DIR 报告目录（默认 tmp/wave_c_local_guard_reports）
  --output FILE    输出报告路径
  --only-platform-path-check-dryrun  仅执行 B125A 平台路径检查 dry-run batch（透传到 B138/B129/B125）
  --skip-platform-path-checks-dryrun  跳过 B138/B129/B125 中的平台路径检查 dry-run batch
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
    --reports-dir)
      REPORTS_DIR="$2"
      shift 2
      ;;
    --output)
      OUTPUT_FILE="$2"
      shift 2
      ;;
    --only-platform-path-check-dryrun)
      ONLY_PLATFORM_PATH_CHECK_DRYRUN=true
      shift
      ;;
    --skip-platform-path-checks-dryrun)
      WITH_PLATFORM_PATH_CHECKS_DRYRUN=false
      shift
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
  OUTPUT_FILE="$REPORTS_DIR/wave_c_b144_local_guard_ops_pack_${RUN_ID}.md"
fi

mkdir -p "$REPORTS_DIR"
mkdir -p "$(dirname "$OUTPUT_FILE")"

b138_report="$REPORTS_DIR/wave_c_b138_pre_ci_reenable_full_gate_${RUN_ID}.md"
b140_report="$REPORTS_DIR/wave_c_b140_local_guard_consistency_${RUN_ID}.md"
b142_json="$REPORTS_DIR/wave_c_b142_local_guard_status_${RUN_ID}.json"
b143_report="$REPORTS_DIR/wave_c_b143_alert_thresholds_${RUN_ID}.md"
b139_report="$REPORTS_DIR/wave_c_b139_local_guard_cleanup_plan_${RUN_ID}.md"

run_step() {
  local cmd="$1"
  local log="$2"

  set +e
  eval "$cmd" > "$log" 2>&1
  local ec=$?
  set -e

  echo "$ec"
}

b138_log="$REPORTS_DIR/wave_c_b138_pre_ci_reenable_full_gate_${RUN_ID}.b144.log"
b140_log="$REPORTS_DIR/wave_c_b140_local_guard_consistency_${RUN_ID}.b144.log"
b142_log="$REPORTS_DIR/wave_c_b142_local_guard_status_${RUN_ID}.b144.log"
b143_log="$REPORTS_DIR/wave_c_b143_alert_thresholds_${RUN_ID}.b144.log"
b139_log="$REPORTS_DIR/wave_c_b139_local_guard_cleanup_plan_${RUN_ID}.b144.log"

b138_cmd="bash scripts/run_wave_c_pre_ci_reenable_full_gate.sh --run-id ${RUN_ID} --strict --reports-dir ${REPORTS_DIR} --output ${b138_report}"
if [[ "$ONLY_PLATFORM_PATH_CHECK_DRYRUN" == "true" ]]; then
  b138_cmd="$b138_cmd --only-platform-path-check-dryrun"
fi
if [[ "$WITH_PLATFORM_PATH_CHECKS_DRYRUN" == "false" ]]; then
  b138_cmd="$b138_cmd --skip-platform-path-checks-dryrun"
fi

b138_exit=$(run_step "$b138_cmd" "$b138_log")
b140_exit=$(run_step "bash scripts/check_wave_c_local_guard_consistency.sh --run-id ${RUN_ID} --strict --output ${b140_report}" "$b140_log")
b142_exit=$(run_step "bash scripts/export_wave_c_local_guard_status_json.sh --run-id ${RUN_ID} --strict --output ${b142_json}" "$b142_log")
b143_exit=$(run_step "bash scripts/check_wave_c_local_guard_alert_thresholds.sh --run-id ${RUN_ID} --strict --input ${b142_json} --output ${b143_report}" "$b143_log")
b139_exit=$(run_step "bash scripts/cleanup_wave_c_local_guard_reports.sh --run-id ${RUN_ID} --output ${b139_report}" "$b139_log")

overall="PASS"
if [[ "$b138_exit" != "0" || "$b140_exit" != "0" || "$b142_exit" != "0" || "$b143_exit" != "0" || "$b139_exit" != "0" ]]; then
  overall="FAIL"
fi

b138_platform_path_checks_mode="ENABLED"
if [[ "$WITH_PLATFORM_PATH_CHECKS_DRYRUN" == "false" ]]; then
  b138_platform_path_checks_mode="SKIPPED"
fi

b138_local_first_bundle_mode="FULL"
if [[ "$ONLY_PLATFORM_PATH_CHECK_DRYRUN" == "true" ]]; then
  b138_local_first_bundle_mode="PLATFORM_ONLY"
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
  echo
  echo "## Options"
  echo
  echo "| option | value |"
  echo "|--------|-------|"
  echo "| b138_local_first_bundle_mode | $b138_local_first_bundle_mode |"
  echo "| b138_platform_path_checks_mode | $b138_platform_path_checks_mode |"
} > "$OUTPUT_FILE"

echo "[INFO] overall=$overall"
echo "[PASS] ops pack report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$overall" != "PASS" ]]; then
  exit 1
fi

exit 0
