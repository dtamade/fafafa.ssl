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
Wave C B138 Pre-CI Re-enable Full Gate

用途：
  一次执行 oncall + snapshot + B137 packet，生成恢复 CI 前全量门禁报告。

用法：
  scripts/run_wave_c_pre_ci_reenable_full_gate.sh [options]

选项：
  --run-id ID      指定 run_id
  --reports-dir DIR 报告目录（默认 tmp/wave_c_local_guard_reports）
  --output FILE    输出报告路径
  --only-platform-path-check-dryrun  仅执行 B125A 平台路径检查 dry-run batch（透传到 B129/B125）
  --skip-platform-path-checks-dryrun  跳过 B129/B125 中的平台路径检查 dry-run batch
  --strict         overall 非 PASS 返回非 0
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
  OUTPUT_FILE="$REPORTS_DIR/wave_c_b138_pre_ci_reenable_full_gate_${RUN_ID}.md"
fi

mkdir -p "$REPORTS_DIR"
mkdir -p "$(dirname "$OUTPUT_FILE")"

oncall_report="$REPORTS_DIR/wave_c_b129_oncall_check_${RUN_ID}.md"
snapshot_report="$REPORTS_DIR/wave_c_b132_local_first_status_snapshot_${RUN_ID}.md"
packet_report="$REPORTS_DIR/wave_c_b137_pre_ci_reenable_packet_${RUN_ID}.md"

oncall_log="$REPORTS_DIR/wave_c_b129_oncall_check_${RUN_ID}.b138.log"
snapshot_log="$REPORTS_DIR/wave_c_b132_local_first_status_snapshot_${RUN_ID}.b138.log"
packet_log="$REPORTS_DIR/wave_c_b137_pre_ci_reenable_packet_${RUN_ID}.b138.log"

run_step() {
  local cmd="$1"
  local log="$2"

  set +e
  eval "$cmd" > "$log" 2>&1
  local ec=$?
  set -e

  echo "$ec"
}

oncall_cmd="bash scripts/run_wave_c_local_guard_oncall_check.sh --run-id ${RUN_ID} --strict --quiet --reports-dir ${REPORTS_DIR} --output ${oncall_report}"
if [[ "$ONLY_PLATFORM_PATH_CHECK_DRYRUN" == "true" ]]; then
  oncall_cmd="$oncall_cmd --only-platform-path-check-dryrun"
fi
if [[ "$WITH_PLATFORM_PATH_CHECKS_DRYRUN" == "false" ]]; then
  oncall_cmd="$oncall_cmd --skip-platform-path-checks-dryrun"
fi

oncall_exit=$(run_step "$oncall_cmd" "$oncall_log")

snapshot_exit=$(run_step \
  "bash scripts/generate_wave_c_local_first_status_snapshot.sh --run-id ${RUN_ID} --strict --output ${snapshot_report}" \
  "$snapshot_log")

packet_exit=$(run_step \
  "bash scripts/prepare_wave_c_b137_pre_ci_reenable_packet.sh --run-id ${RUN_ID} --strict --oncall-report ${oncall_report} --snapshot-report ${snapshot_report} --output ${packet_report}" \
  "$packet_log")

packet_state="UNKNOWN"
if [[ -f "$packet_report" ]]; then
  packet_state="$(rg -o "packet_state:[[:space:]]*\*\*[A-Z_]+\*\*" "$packet_report" | head -1 | sed -E 's/.*\*\*([A-Z_]+)\*\*/\1/' || true)"
  packet_state="${packet_state:-UNKNOWN}"
fi

overall="PASS"
if [[ "$oncall_exit" != "0" || "$snapshot_exit" != "0" || "$packet_exit" != "0" || "$packet_state" != "READY_FOR_APPROVAL" ]]; then
  overall="FAIL"
fi

{
  echo "# Wave C B138 Pre-CI Re-enable Full Gate"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- overall: **$overall**"
  echo
  echo "## Step Matrix"
  echo
  echo "| step | exit | expected | report | log |"
  echo "|------|------|----------|--------|-----|"
  echo "| B129 oncall | $oncall_exit | 0 | $oncall_report | $oncall_log |"
  echo "| B132 snapshot | $snapshot_exit | 0 | $snapshot_report | $snapshot_log |"
  echo "| B137 packet | $packet_exit | 0 | $packet_report | $packet_log |"
  echo
  echo "## Gate Decision"
  echo
  echo "- packet_state: $packet_state"
  echo "- expected_packet_state: READY_FOR_APPROVAL"
} > "$OUTPUT_FILE"

echo "[INFO] overall=$overall"
echo "[PASS] report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$overall" != "PASS" ]]; then
  exit 1
fi

exit 0
