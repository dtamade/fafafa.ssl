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
Wave C B125 Local-First Guard Bundle

用途：
  一次执行 B123 + B124 本地优先守护门禁，生成汇总报告。

用法：
  scripts/run_wave_c_local_first_guard_bundle.sh [options]

选项：
  --run-id ID       指定 run_id
  --reports-dir DIR 报告目录（默认 tmp/wave_c_local_guard_reports）
  --output FILE     输出汇总报告
  --only-platform-path-check-dryrun  仅执行 B125A 平台路径检查 dry-run batch
  --skip-platform-path-checks-dryrun  跳过四平台路径检查 dry-run batch
  --strict          任一步骤失败返回非 0
  --help            显示帮助
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
  OUTPUT_FILE="$REPORTS_DIR/wave_c_b125_local_guard_bundle_${RUN_ID}.md"
fi

mkdir -p "$REPORTS_DIR"

continuity_report="$REPORTS_DIR/wave_c_b123_local_first_continuity_${RUN_ID}.md"
drift_report="$REPORTS_DIR/wave_c_b124_local_drift_watch_${RUN_ID}.md"

continuity_log="$REPORTS_DIR/wave_c_b123_local_first_continuity_${RUN_ID}.log"
drift_log="$REPORTS_DIR/wave_c_b124_local_drift_watch_${RUN_ID}.log"
platform_path_checks_log="$REPORTS_DIR/wave_c_b125_platform_path_checks_${RUN_ID}.log"

run_step() {
  local cmd="$1"
  local log="$2"

  set +e
  eval "$cmd" > "$log" 2>&1
  local ec=$?
  set -e

  echo "$ec"
}

continuity_exit="SKIP"
drift_exit="SKIP"
continuity_state="SKIPPED"
drift_state="SKIPPED"
continuity_report_display="<none>"
drift_report_display="<none>"
continuity_log_display="<none>"
drift_log_display="<none>"
if [[ "$ONLY_PLATFORM_PATH_CHECK_DRYRUN" != "true" ]]; then
  continuity_exit=$(run_step \
    "bash scripts/check_wave_c_local_first_continuity.sh --run-id ${RUN_ID} --strict --output ${continuity_report}" \
    "$continuity_log")

  drift_exit=$(run_step \
    "bash scripts/check_wave_c_local_drift_watch.sh --run-id ${RUN_ID} --strict --output ${drift_report}" \
    "$drift_log")

  continuity_report_display="$continuity_report"
  drift_report_display="$drift_report"
  continuity_log_display="$continuity_log"
  drift_log_display="$drift_log"
fi

platform_path_checks_exit="SKIP"
if [[ "$WITH_PLATFORM_PATH_CHECKS_DRYRUN" == "true" ]]; then
  platform_path_checks_exit=$(run_step \
    "bash scripts/run_minimal_ci_gate.sh --skip-compile --skip-modules --skip-phase2-dryrun" \
    "$platform_path_checks_log")
fi

overall="PASS"
if [[ "$continuity_exit" != "0" && "$continuity_exit" != "SKIP" ]]; then
  overall="FAIL"
fi
if [[ "$drift_exit" != "0" && "$drift_exit" != "SKIP" ]]; then
  overall="FAIL"
fi
if [[ "$platform_path_checks_exit" != "0" && "$platform_path_checks_exit" != "SKIP" ]]; then
  overall="FAIL"
fi

if [[ "$ONLY_PLATFORM_PATH_CHECK_DRYRUN" != "true" && -f "$continuity_report" ]]; then
  continuity_state="UNKNOWN"
  continuity_state="$(rg -o "local_first_state:[[:space:]]*\*\*[A-Z_]+\*\*" "$continuity_report" | head -1 | sed -E 's/.*\*\*([A-Z_]+)\*\*/\1/' || true)"
  continuity_state="${continuity_state:-UNKNOWN}"
fi

if [[ "$ONLY_PLATFORM_PATH_CHECK_DRYRUN" != "true" && -f "$drift_report" ]]; then
  drift_state="UNKNOWN"
  drift_state="$(rg -o "local_drift_state:[[:space:]]*\*\*[A-Z_]+\*\*" "$drift_report" | head -1 | sed -E 's/.*\*\*([A-Z_]+)\*\*/\1/' || true)"
  drift_state="${drift_state:-UNKNOWN}"
fi

platform_path_checks_state="SKIPPED"
platform_path_checks_log_display="<none>"
if [[ "$WITH_PLATFORM_PATH_CHECKS_DRYRUN" == "true" ]]; then
  platform_path_checks_log_display="$platform_path_checks_log"
  platform_path_checks_state="FAIL"
  if [[ "$platform_path_checks_exit" == "0" ]]; then
    platform_path_checks_state="PASS"
  fi
fi

{
  echo "# Wave C B125 Local-First Guard Bundle"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- overall: **$overall**"
  echo
  echo "## Step Matrix"
  echo
  echo "| step | exit | state | report | log |"
  echo "|------|------|-------|--------|-----|"
  echo "| B123 local continuity | $continuity_exit | $continuity_state | $continuity_report_display | $continuity_log_display |"
  echo "| B124 local drift watch | $drift_exit | $drift_state | $drift_report_display | $drift_log_display |"
  echo "| B125A platform path-check dry-run batch | $platform_path_checks_exit | $platform_path_checks_state | <none> | $platform_path_checks_log_display |"
  echo
  echo "## Decision"
  echo
  if [[ "$overall" == "PASS" ]]; then
    echo "- local-first guard chain is stable."
  else
    echo "- local-first guard chain is blocked; fix failed step before next iteration."
  fi
} > "$OUTPUT_FILE"

echo "[INFO] overall=$overall"
echo "[PASS] bundle report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$overall" != "PASS" ]]; then
  exit 1
fi

exit 0
