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
  OUTPUT_FILE="tmp/test-reports/wave_c_b144_local_guard_ops_pack_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

OPS_DIR="tmp/test-reports"
b138_report="$OPS_DIR/wave_c_b138_pre_ci_reenable_full_gate_${RUN_ID}.md"
b140_report="$OPS_DIR/wave_c_b140_local_guard_consistency_${RUN_ID}.md"
b142_json="$OPS_DIR/wave_c_b142_local_guard_status_${RUN_ID}.json"
b143_report="$OPS_DIR/wave_c_b143_alert_thresholds_${RUN_ID}.md"
b139_report="$OPS_DIR/wave_c_b139_local_guard_cleanup_plan_${RUN_ID}.md"

mkdir -p "$OPS_DIR"

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

  echo "[wave-c-b144] [$step_name] $cmd_desc" >&2

  set +e
  "$@" > "$log" 2>&1
  local ec=$?
  set -e

  echo "[wave-c-b144] [$step_name] exit=$ec log=$log" >&2
  echo "$ec"
}

b138_log="$OPS_DIR/wave_c_b138_pre_ci_reenable_full_gate_${RUN_ID}.b144.log"
b140_log="$OPS_DIR/wave_c_b140_local_guard_consistency_${RUN_ID}.b144.log"
b142_log="$OPS_DIR/wave_c_b142_local_guard_status_${RUN_ID}.b144.log"
b143_log="$OPS_DIR/wave_c_b143_alert_thresholds_${RUN_ID}.b144.log"
b139_log="$OPS_DIR/wave_c_b139_local_guard_cleanup_plan_${RUN_ID}.b144.log"

b138_cmd_words=(
  bash
  scripts/run_wave_c_pre_ci_reenable_full_gate.sh
  --run-id "$RUN_ID"
  --strict
  --output "$b138_report"
)
b140_cmd_words=(
  bash
  scripts/check_wave_c_local_guard_consistency.sh
  --run-id "$RUN_ID"
  --strict
  --output "$b140_report"
)
b142_cmd_words=(
  bash
  scripts/export_wave_c_local_guard_status_json.sh
  --run-id "$RUN_ID"
  --strict
  --output "$b142_json"
)
b143_cmd_words=(
  bash
  scripts/check_wave_c_local_guard_alert_thresholds.sh
  --run-id "$RUN_ID"
  --strict
  --input "$b142_json"
  --output "$b143_report"
)
b139_cmd_words=(
  bash
  scripts/cleanup_wave_c_local_guard_reports.sh
  --run-id "$RUN_ID"
  --output "$b139_report"
)

b138_exit=$(run_step "b138_full_gate" "$b138_log" "$(shell_join "${b138_cmd_words[@]}")" "${b138_cmd_words[@]}")
b140_exit=$(run_step "b140_consistency" "$b140_log" "$(shell_join "${b140_cmd_words[@]}")" "${b140_cmd_words[@]}")
b142_exit=$(run_step "b142_status_json" "$b142_log" "$(shell_join "${b142_cmd_words[@]}")" "${b142_cmd_words[@]}")
b143_exit=$(run_step "b143_alert_thresholds" "$b143_log" "$(shell_join "${b143_cmd_words[@]}")" "${b143_cmd_words[@]}")
b139_exit=$(run_step "b139_cleanup_plan" "$b139_log" "$(shell_join "${b139_cmd_words[@]}")" "${b139_cmd_words[@]}")

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
