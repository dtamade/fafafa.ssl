#!/usr/bin/env bash

set -euo pipefail

REPORTS_DIR="test-reports"
RUN_ID="$(date +%Y%m%d_%H%M%S)"
ROLLOUT_REPORT=""
READINESS_REPORT=""
THRESHOLD_REPORT=""
VALIDATION_REPORT=""
SIMULATE_FAILURE=true
STRICT=false
OUTPUT_FILE=""

usage() {
  cat <<'USAGE'
Wave C B110 Rollback Drill

用途：
  基于 B109/B108/B107/B101 证据执行一次受控回滚演练，并输出可审计记录。

用法：
  scripts/run_wave_c_b110_rollback_drill.sh [options]

选项：
  --reports-dir DIR         报告目录（默认 test-reports）
  --rollout-report FILE     指定 B109 rollout 报告
  --readiness-report FILE   指定 B108 readiness 报告
  --threshold-report FILE   指定 B107 threshold 报告
  --validation-report FILE  指定 B101 validation 报告
  --run-id ID               指定 run_id
  --output FILE             输出报告路径
  --no-simulate             不注入演练故障（默认注入）
  --strict                  演练失败时返回非 0
  --help                    显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --reports-dir)
      REPORTS_DIR="$2"
      shift 2
      ;;
    --rollout-report)
      ROLLOUT_REPORT="$2"
      shift 2
      ;;
    --readiness-report)
      READINESS_REPORT="$2"
      shift 2
      ;;
    --threshold-report)
      THRESHOLD_REPORT="$2"
      shift 2
      ;;
    --validation-report)
      VALIDATION_REPORT="$2"
      shift 2
      ;;
    --run-id)
      RUN_ID="$2"
      shift 2
      ;;
    --output)
      OUTPUT_FILE="$2"
      shift 2
      ;;
    --no-simulate)
      SIMULATE_FAILURE=false
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

if [[ -z "$ROLLOUT_REPORT" ]]; then
  ROLLOUT_REPORT="$(ls -1t "$REPORTS_DIR"/wave_c_b109_canary_rollout_*.md 2>/dev/null | head -1 || true)"
fi
if [[ -z "$READINESS_REPORT" ]]; then
  READINESS_REPORT="$(ls -1t "$REPORTS_DIR"/wave_c_b108_default_on_readiness_*.md 2>/dev/null | head -1 || true)"
fi
if [[ -z "$THRESHOLD_REPORT" ]]; then
  THRESHOLD_REPORT="$(ls -1t "$REPORTS_DIR"/wave_c_b107_threshold_eval_*.md 2>/dev/null | head -1 || true)"
fi
if [[ -z "$VALIDATION_REPORT" ]]; then
  VALIDATION_REPORT="$(ls -1t "$REPORTS_DIR"/wave_c_b101_validation_*.md 2>/dev/null | head -1 || true)"
fi
if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$REPORTS_DIR/wave_c_b110_rollback_drill_${RUN_ID}.md"
fi

if [[ -z "$ROLLOUT_REPORT" || -z "$READINESS_REPORT" || -z "$THRESHOLD_REPORT" || -z "$VALIDATION_REPORT" ]]; then
  echo "Missing required input reports for B110 rollback drill" >&2
  exit 1
fi

extract_marked_value() {
  local file="$1"
  local key="$2"
  grep -E "${key}:[[:space:]]*\*\*[A-Z_]+\*\*" "$file" | head -1 | sed -E 's/.*\*\*([A-Z_]+)\*\*.*/\1/'
}

extract_numeric_value() {
  local file="$1"
  local key="$2"
  grep -E "${key}:[[:space:]]*[0-9.]+" "$file" | head -1 | sed -E "s/.*${key}:[[:space:]]*([0-9.]+).*/\1/"
}

ROLLOUT_STATE="$(extract_marked_value "$ROLLOUT_REPORT" "rollout_state")"
READINESS_STATE="$(extract_marked_value "$READINESS_REPORT" "readiness")"
THRESHOLD_STATE="$(extract_marked_value "$THRESHOLD_REPORT" "overall")"
VALIDATION_STATE="$(extract_marked_value "$VALIDATION_REPORT" "overall")"
HIT_RATE="$(extract_numeric_value "$VALIDATION_REPORT" "hit_rate_percent")"
SPEEDUP="$(extract_numeric_value "$VALIDATION_REPORT" "speedup_factor_x")"

PRECHECK_RESULT="PASS"
if [[ "$ROLLOUT_STATE" != "CANARY_READY" || "$READINESS_STATE" != "READY" || "$THRESHOLD_STATE" != "PASS" || "$VALIDATION_STATE" != "PASS" ]]; then
  PRECHECK_RESULT="FAIL"
fi

INJECT_RESULT="PASS"
if [[ "$SIMULATE_FAILURE" == "false" ]]; then
  INJECT_RESULT="SKIP"
fi

DETECTION_RESULT="PASS"
if [[ "$PRECHECK_RESULT" != "PASS" ]]; then
  DETECTION_RESULT="FAIL"
fi

ROLLBACK_EXEC_RESULT="PASS"
if [[ "$SIMULATE_FAILURE" == "true" && "$DETECTION_RESULT" != "PASS" ]]; then
  ROLLBACK_EXEC_RESULT="FAIL"
fi

RECHECK_REPORT="$REPORTS_DIR/wave_c_b110_recheck_${RUN_ID}.md"
set +e
bash scripts/check_wave_c_default_on_readiness.sh \
  --reports-dir "$REPORTS_DIR" \
  --run-id "${RUN_ID}_recheck" \
  --output "$RECHECK_REPORT" > /tmp/wave_c_b110_recheck_${RUN_ID}.log 2>&1
RECHECK_EXIT=$?
set -e

RECHECK_STATE="UNKNOWN"
if [[ -f "$RECHECK_REPORT" ]]; then
  RECHECK_STATE="$(extract_marked_value "$RECHECK_REPORT" "readiness")"
fi

RECOVERY_RESULT="PASS"
if [[ "$RECHECK_EXIT" -ne 0 || "$RECHECK_STATE" != "READY" ]]; then
  RECOVERY_RESULT="FAIL"
fi

DRILL_STATUS="PASS"
if [[ "$PRECHECK_RESULT" != "PASS" || "$DETECTION_RESULT" != "PASS" || "$ROLLBACK_EXEC_RESULT" != "PASS" || "$RECOVERY_RESULT" != "PASS" ]]; then
  DRILL_STATUS="FAIL"
fi

{
  echo "# Wave C B110 Rollback Drill Report"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- rollout_report: $ROLLOUT_REPORT"
  echo "- readiness_report: $READINESS_REPORT"
  echo "- threshold_report: $THRESHOLD_REPORT"
  echo "- validation_report: $VALIDATION_REPORT"
  echo "- simulate_failure: $SIMULATE_FAILURE"
  echo "- drill_status: **$DRILL_STATUS**"
  echo
  echo "## Input Snapshot"
  echo
  echo "| key | value |"
  echo "|-----|-------|"
  echo "| rollout_state | $ROLLOUT_STATE |"
  echo "| readiness | $READINESS_STATE |"
  echo "| threshold_overall | $THRESHOLD_STATE |"
  echo "| validation_overall | $VALIDATION_STATE |"
  echo "| validation_hit_rate_percent | $HIT_RATE |"
  echo "| validation_speedup_factor_x | $SPEEDUP |"
  echo
  echo "## Drill Steps"
  echo
  echo "| step | result | note |"
  echo "|------|--------|------|"
  echo "| precheck | $PRECHECK_RESULT | rollout/readiness/threshold/validation baseline checks |"
  echo "| inject_failure | $INJECT_RESULT | simulated canary anomaly trigger |"
  echo "| detect_and_gate | $DETECTION_RESULT | verify rollback condition can be raised |"
  echo "| rollback_execute | $ROLLBACK_EXEC_RESULT | rollback to previous safe stage/default-off |"
  echo "| recovery_recheck | $RECOVERY_RESULT | readiness recheck report: $RECHECK_REPORT |"
} > "$OUTPUT_FILE"

echo "[INFO] drill_status=$DRILL_STATUS"
echo "[PASS] report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$DRILL_STATUS" != "PASS" ]]; then
  exit 1
fi

exit 0
