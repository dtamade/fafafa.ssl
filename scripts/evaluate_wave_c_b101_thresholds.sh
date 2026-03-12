#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

REPORTS_DIR="${FAFAFA_WAVE_C_QUICK_SPRINT_REPORTS_DIR:-tmp/wave_c_quick_sprint_reports}"
VALIDATION_GLOB="${FAFAFA_WAVE_C_B101_VALIDATION_GLOB:-tmp/wave_c_b101_reports_*/wave_c_b101_validation_*.md}"
MIN_HIT_RATE="99.0"
MIN_SPEEDUP="3.0"
MIN_PASSING_RUNS=3
STRICT=false
RUN_ID="$(date +%Y%m%d_%H%M%S)"
OUTPUT_FILE=""

usage() {
  cat <<'USAGE'
Wave C B107 Threshold Evaluation

用途：
  基于 wave_c_b101_validation_*.md 报告，自动评估命中率与加速比阈值是否达标。

用法：
  scripts/evaluate_wave_c_b101_thresholds.sh [options]

选项：
  --reports-dir DIR       报告目录（默认 tmp/wave_c_quick_sprint_reports）
  --validation-glob GLOB  B101 验证报告 glob（默认 tmp/wave_c_b101_reports_*/wave_c_b101_validation_*.md）
  --min-hit-rate N        命中率阈值（默认 99.0）
  --min-speedup N         加速比阈值（默认 3.0）
  --min-passing-runs N    至少满足阈值的 run 数（默认 3）
  --run-id ID             指定 run_id
  --output FILE           输出报告路径
  --strict                不达标时返回非 0
  --help                  显示帮助
USAGE
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --reports-dir)
      REPORTS_DIR="$2"
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
    --min-passing-runs)
      MIN_PASSING_RUNS="$2"
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

if [[ -z "$OUTPUT_FILE" ]]; then
  OUTPUT_FILE="$REPORTS_DIR/wave_c_b107_threshold_eval_${RUN_ID}.md"
fi

mapfile -t EVAL_OUTPUT < <(python3 - "$PROJECT_ROOT" "$VALIDATION_GLOB" "$MIN_HIT_RATE" "$MIN_SPEEDUP" "$MIN_PASSING_RUNS" "$OUTPUT_FILE" <<'PY'
import glob
import os
import re
import sys
from pathlib import Path

project_root = Path(sys.argv[1])
validation_glob = sys.argv[2]
min_hit = float(sys.argv[3])
min_speedup = float(sys.argv[4])
min_passing_runs = int(sys.argv[5])
output_file = project_root / sys.argv[6]

glob_pattern = validation_glob if os.path.isabs(validation_glob) else str(project_root / validation_glob)
rows = []
for report_path in sorted(glob.glob(glob_pattern)):
  report = Path(report_path)
  text = report.read_text(errors='ignore')
  run_id = re.search(r'run_id:\s*(\S+)', text)
  overall = re.search(r'overall:\s*\*\*(\w+)\*\*', text)
  hit = re.search(r'hit_rate_percent:\s*([0-9.]+|n/a)', text)
  speedup = re.search(r'speedup_factor_x:\s*([0-9.]+|n/a)', text)
  if not (run_id and overall and hit and speedup):
    continue

  hit_raw = hit.group(1)
  speed_raw = speedup.group(1)

  try:
    hit_val = float(hit_raw)
  except ValueError:
    hit_val = None

  try:
    speed_val = float(speed_raw)
  except ValueError:
    speed_val = None

  pass_threshold = (
    overall.group(1) == 'PASS' and
    hit_val is not None and hit_val >= min_hit and
    speed_val is not None and speed_val >= min_speedup
  )

  rows.append({
    'file': report.name,
    'run_id': run_id.group(1),
    'overall': overall.group(1),
    'hit_raw': hit_raw,
    'speed_raw': speed_raw,
    'pass_threshold': pass_threshold,
  })

passing = sum(1 for row in rows if row['pass_threshold'])
overall_status = 'PASS' if passing >= min_passing_runs else 'FAIL'

output_file.parent.mkdir(parents=True, exist_ok=True)
with output_file.open('w', encoding='utf-8') as f:
  f.write('# Wave C B107 Threshold Evaluation Report\n\n')
  f.write(f'- validation_glob: {validation_glob}\n')
  f.write(f'- min_hit_rate_percent: {min_hit}\n')
  f.write(f'- min_speedup_factor_x: {min_speedup}\n')
  f.write(f'- min_passing_runs: {min_passing_runs}\n')
  f.write(f'- passing_runs: {passing}\n')
  f.write(f'- overall: **{overall_status}**\n\n')

  f.write('## Run Matrix\n\n')
  f.write('| run_id | overall | hit_rate_percent | speedup_factor_x | threshold_pass | file |\n')
  f.write('|--------|---------|------------------|------------------|----------------|------|\n')
  for row in rows:
    f.write(
      f"| {row['run_id']} | {row['overall']} | {row['hit_raw']} | {row['speed_raw']} | "
      f"{'YES' if row['pass_threshold'] else 'NO'} | {row['file']} |\n"
    )

print(overall_status)
print(passing)
print(len(rows))
PY
)

OVERALL_STATUS="${EVAL_OUTPUT[0]:-FAIL}"
PASSING_RUNS="${EVAL_OUTPUT[1]:-0}"
TOTAL_RUNS="${EVAL_OUTPUT[2]:-0}"

echo "[INFO] threshold_overall=$OVERALL_STATUS passing_runs=$PASSING_RUNS total_runs=$TOTAL_RUNS"
echo "[PASS] report generated: $OUTPUT_FILE"

if [[ "$STRICT" == "true" && "$OVERALL_STATUS" != "PASS" ]]; then
  exit 1
fi

exit 0
