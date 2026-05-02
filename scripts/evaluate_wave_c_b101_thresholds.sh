#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

REPORTS_DIR="test-reports"
REPORT_GLOB="wave_c_b101_validation_*.md"
REQUIRE_FULL_GATE=false
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
  --reports-dir DIR       报告目录（默认 test-reports）
  --report-glob GLOB      报告匹配模式（默认 wave_c_b101_validation_*.md）
  --require-full-gate     仅统计 `full_gate: true` 的 B101 报告
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
    --report-glob)
      REPORT_GLOB="$2"
      shift 2
      ;;
    --require-full-gate)
      REQUIRE_FULL_GATE=true
      shift
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
  OUTPUT_FILE="test-reports/wave_c_b107_threshold_eval_${RUN_ID}.md"
fi

mapfile -t EVAL_OUTPUT < <(python3 - "$PROJECT_ROOT" "$REPORTS_DIR" "$REPORT_GLOB" "$REQUIRE_FULL_GATE" "$MIN_HIT_RATE" "$MIN_SPEEDUP" "$MIN_PASSING_RUNS" "$OUTPUT_FILE" <<'PY'
import re
import sys
from pathlib import Path

project_root = Path(sys.argv[1])
reports_dir = project_root / sys.argv[2]
report_glob = sys.argv[3]
require_full_gate = sys.argv[4].lower() == 'true'
min_hit = float(sys.argv[5])
min_speedup = float(sys.argv[6])
min_passing_runs = int(sys.argv[7])
output_file = project_root / sys.argv[8]

rows = []
for report in sorted(reports_dir.glob(report_glob)):
  text = report.read_text(errors='ignore')
  run_id = re.search(r'run_id:\s*(\S+)', text)
  overall = re.search(r'overall:\s*\*\*(\w+)\*\*', text)
  full_gate = re.search(r'full_gate:\s*(true|false)', text, re.IGNORECASE)
  hit = re.search(r'hit_rate_percent:\s*([0-9.]+|n/a)', text)
  speedup = re.search(r'speedup_factor_x:\s*([0-9.]+|n/a)', text)
  if not (run_id and overall and hit and speedup):
    continue

  if require_full_gate:
    if not full_gate or full_gate.group(1).lower() != 'true':
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
  f.write(f'- reports_dir: {reports_dir.relative_to(project_root)}\n')
  f.write(f'- report_glob: {report_glob}\n')
  f.write(f'- require_full_gate: {str(require_full_gate).lower()}\n')
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
