#!/usr/bin/env bash

set -euo pipefail

REPORTS_DIR="test-reports"
RUN_ID="$(date +%Y%m%d_%H%M%S)"
READINESS_REPORT=""
THRESHOLD_REPORT=""
VALIDATION_REPORT=""
STRICT=false
OUTPUT_FILE=""

usage() {
  cat <<'USAGE'
Wave C B109 Controlled Canary Rollout Preparation

用途：
  基于 B108 readiness + B107 threshold + 最新 B101 validation，生成受控灰度启用执行模板。

用法：
  scripts/prepare_wave_c_b109_canary_rollout.sh [options]

选项：
  --reports-dir DIR         报告目录（默认 test-reports）
  --readiness-report FILE   指定 B108 readiness 报告
  --threshold-report FILE   指定 B107 threshold 报告
  --validation-report FILE  指定 B101 validation 报告
  --run-id ID               指定 run_id
  --output FILE             输出路径（默认 test-reports/wave_c_b109_canary_rollout_<run_id>.md）
  --strict                  状态非 READY 时返回非 0
  --help                    显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --reports-dir)
      REPORTS_DIR="$2"
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
  OUTPUT_FILE="$REPORTS_DIR/wave_c_b109_canary_rollout_${RUN_ID}.md"
fi

if [[ -z "$READINESS_REPORT" || -z "$THRESHOLD_REPORT" || -z "$VALIDATION_REPORT" ]]; then
  echo "Missing input reports for B109" >&2
  exit 1
fi

extract_marked_value() {
  local file="$1"
  local key="$2"
  grep -E "${key}:[[:space:]]*\*\*[A-Z]+\*\*" "$file" | head -1 | sed -E 's/.*\*\*([A-Z]+)\*\*.*/\1/'
}

extract_numeric_value() {
  local file="$1"
  local key="$2"
  grep -E "${key}:[[:space:]]*[0-9.]+" "$file" | head -1 | sed -E "s/.*${key}:[[:space:]]*([0-9.]+).*/\1/"
}

READINESS_STATE="$(extract_marked_value "$READINESS_REPORT" "readiness")"
THRESHOLD_STATE="$(extract_marked_value "$THRESHOLD_REPORT" "overall")"
VALIDATION_STATE="$(extract_marked_value "$VALIDATION_REPORT" "overall")"
HIT_RATE="$(extract_numeric_value "$VALIDATION_REPORT" "hit_rate_percent")"
SPEEDUP="$(extract_numeric_value "$VALIDATION_REPORT" "speedup_factor_x")"

ROLLOUT_STATE="CANARY_READY"
if [[ "$READINESS_STATE" != "READY" || "$THRESHOLD_STATE" != "PASS" || "$VALIDATION_STATE" != "PASS" ]]; then
  ROLLOUT_STATE="BLOCKED"
fi

{
  echo "# Wave C B109 Controlled Canary Rollout"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- readiness_report: $READINESS_REPORT"
  echo "- threshold_report: $THRESHOLD_REPORT"
  echo "- validation_report: $VALIDATION_REPORT"
  echo "- rollout_state: **$ROLLOUT_STATE**"
  echo "- default_policy: **DEFAULT_OFF**"
  echo
  echo "## Input Snapshot"
  echo
  echo "| key | value |"
  echo "|-----|-------|"
  echo "| readiness | $READINESS_STATE |"
  echo "| threshold_overall | $THRESHOLD_STATE |"
  echo "| validation_overall | $VALIDATION_STATE |"
  echo "| validation_hit_rate_percent | $HIT_RATE |"
  echo "| validation_speedup_factor_x | $SPEEDUP |"
  echo
  echo "## Canary Stages"
  echo
  echo "| stage | traffic | entry gate | success criteria | rollback trigger |"
  echo "|-------|---------|------------|------------------|------------------|"
  echo "| S0 | 0% | rollout_state=CANARY_READY | readiness=READY | any gate != PASS |"
  echo "| S1 | 5% | S0 passed | validation overall PASS, hit_rate>=99.0, speedup>=3.0 | readiness=HOLD or error burst |"
  echo "| S2 | 25% | S1 stable 30m | same as S1 | same as S1 |"
  echo "| S3 | 50% | S2 stable 60m | same as S1 | same as S1 |"
  echo "| S4 | 100% | S3 stable 120m | same as S1 + no new regression failures | same as S1 |"
  echo
  echo "## Operator Commands"
  echo
  echo "1. 阈值评估"
  echo "   bash scripts/evaluate_wave_c_b101_thresholds.sh --strict"
  echo
  echo "2. readiness 复核"
  echo "   bash scripts/check_wave_c_default_on_readiness.sh --strict"
  echo
  echo "3. 失败时策略"
  echo "   - 保持 default-off"
  echo "   - 停止扩大流量，回退到上一 stage"
  echo "   - 重新执行 B101/B107/B108 校验链路"
} > "$OUTPUT_FILE"

echo "[INFO] rollout_state=$ROLLOUT_STATE"
echo "[PASS] report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$ROLLOUT_STATE" != "CANARY_READY" ]]; then
  exit 1
fi

exit 0
