#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

REPORTS_DIR="${FAFAFA_WAVE_C_QUICK_SPRINT_REPORTS_DIR:-tmp/wave_c_quick_sprint_reports}"
VALIDATION_GLOB="${FAFAFA_WAVE_C_B101_VALIDATION_GLOB:-tmp/wave_c_b101_reports_*/wave_c_b101_validation_*.md}"
MIN_HIT_RATE="99.0"
MIN_SPEEDUP="3.0"
STRICT=false
RUN_ID="$(date +%Y%m%d_%H%M%S)"
OUTPUT_FILE=""
THRESHOLD_REPORT=""
VALIDATION_REPORT=""

usage() {
  cat <<'USAGE'
Wave C Default-On Readiness Check

用途：
  基于 B107 阈值报告 + 最新 B101 全门禁报告，评估 default-on 前置条件是否满足。

用法：
  scripts/check_wave_c_default_on_readiness.sh [options]

选项：
  --reports-dir DIR          报告目录（默认 tmp/wave_c_quick_sprint_reports）
  --threshold-report FILE    指定 B107 阈值报告
  --validation-report FILE   指定 B101 验证报告
  --validation-glob GLOB     B101 验证报告 glob（默认 tmp/wave_c_b101_reports_*/wave_c_b101_validation_*.md）
  --min-hit-rate N           命中率阈值（默认 99.0）
  --min-speedup N            加速比阈值（默认 3.0）
  --run-id ID                指定 run_id
  --output FILE              输出报告路径
  --strict                   HOLD 时返回非 0
  --help                     显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --reports-dir)
      REPORTS_DIR="$2"
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
    --validation-glob)
      VALIDATION_GLOB="$2"
      shift 2
      ;;
    --min-hit-rate)
      MIN_HIT_RATE="$2"
      shift 2
      ;;
    --min-speedup)
      MIN_SPEEDUP="$2"
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

resolve_latest_validation_report() {
  python3 - "$PROJECT_ROOT" "$VALIDATION_GLOB" <<'PY'
import glob
import os
import sys
from pathlib import Path

project_root = Path(sys.argv[1])
validation_glob = sys.argv[2]
glob_pattern = validation_glob if os.path.isabs(validation_glob) else str(project_root / validation_glob)
paths = sorted(glob.glob(glob_pattern), key=lambda p: (os.path.getmtime(p), p), reverse=True)
if paths:
  print(paths[0])
PY
}

if [[ -z "$THRESHOLD_REPORT" ]]; then
  THRESHOLD_REPORT="$(ls -1t "$REPORTS_DIR"/wave_c_b107_threshold_eval_*.md 2>/dev/null | head -1 || true)"
fi

if [[ -z "$VALIDATION_REPORT" ]]; then
  VALIDATION_REPORT="$(resolve_latest_validation_report)"
fi

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$REPORTS_DIR/wave_c_b108_default_on_readiness_${RUN_ID}.md"
fi

mkdir -p "$(dirname "$OUTPUT_FILE")"

if [[ -z "$THRESHOLD_REPORT" || -z "$VALIDATION_REPORT" ]]; then
  echo "Missing input reports" >&2
  exit 1
fi

extract_overall() {
  local file="$1"
  grep -E 'overall:[[:space:]]*\*\*[A-Z]+\*\*' "$file" | head -1 | sed -E 's/.*\*\*([A-Z]+)\*\*.*/\1/'
}

extract_hit() {
  local file="$1"
  grep -E 'hit_rate_percent:[[:space:]]*[0-9.]+' "$file" | head -1 | sed -E 's/.*hit_rate_percent:[[:space:]]*([0-9.]+).*/\1/'
}

extract_speedup() {
  local file="$1"
  grep -E 'speedup_factor_x:[[:space:]]*[0-9.]+' "$file" | head -1 | sed -E 's/.*speedup_factor_x:[[:space:]]*([0-9.]+).*/\1/'
}

threshold_overall="$(extract_overall "$THRESHOLD_REPORT")"
validation_overall="$(extract_overall "$VALIDATION_REPORT")"
hit_rate="$(extract_hit "$VALIDATION_REPORT")"
speedup="$(extract_speedup "$VALIDATION_REPORT")"

check_threshold_report="FAIL"
check_validation_overall="FAIL"
check_hit_rate="FAIL"
check_speedup="FAIL"

if [[ "$threshold_overall" == "PASS" ]]; then
  check_threshold_report="PASS"
fi

if [[ "$validation_overall" == "PASS" ]]; then
  check_validation_overall="PASS"
fi

if awk "BEGIN {exit !($hit_rate >= $MIN_HIT_RATE)}"; then
  check_hit_rate="PASS"
fi

if awk "BEGIN {exit !($speedup >= $MIN_SPEEDUP)}"; then
  check_speedup="PASS"
fi

readiness="READY"
if [[ "$check_threshold_report" != "PASS" ||
      "$check_validation_overall" != "PASS" ||
      "$check_hit_rate" != "PASS" ||
      "$check_speedup" != "PASS" ]]; then
  readiness="HOLD"
fi

{
  echo "# Wave C B108 Default-On Readiness"
  echo
  echo "- run_id: $RUN_ID"
  echo "- generated_at: $(date '+%Y-%m-%d %H:%M:%S %z')"
  echo "- threshold_report: $THRESHOLD_REPORT"
  echo "- validation_report: $VALIDATION_REPORT"
  echo "- min_hit_rate_percent: $MIN_HIT_RATE"
  echo "- min_speedup_factor_x: $MIN_SPEEDUP"
  echo "- readiness: **$readiness**"
  echo
  echo "## Checks"
  echo
  echo "| check | value | result |"
  echo "|------|-------|--------|"
  echo "| threshold_report_overall | $threshold_overall | $check_threshold_report |"
  echo "| validation_overall | $validation_overall | $check_validation_overall |"
  echo "| validation_hit_rate_percent | $hit_rate | $check_hit_rate |"
  echo "| validation_speedup_factor_x | $speedup | $check_speedup |"
} > "$OUTPUT_FILE"

echo "[INFO] readiness=$readiness"
echo "[PASS] report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$readiness" != "READY" ]]; then
  exit 1
fi

exit 0
