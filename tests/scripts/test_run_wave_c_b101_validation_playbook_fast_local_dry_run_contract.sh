#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  echo "[SKIP] not a git worktree"
  exit 0
fi

before_status="$(git status --porcelain)"
RUN_ID="contract_wave_c_b101_$(date +%s)_$$"

set +e
output="$(bash scripts/run_wave_c_b101_validation_playbook.sh --dry-run --fast-local --run-id "$RUN_ID" --full-gate 2>&1)"
exit_code=$?
set -e

if [[ "$exit_code" -ne 0 ]]; then
  echo "[FAIL] run_wave_c_b101_validation_playbook --dry-run should exit 0 (got: $exit_code)"
  printf '%s\n' "$output"
  exit 1
fi

after_status="$(git status --porcelain)"
if [[ "$before_status" != "$after_status" ]]; then
  echo "[FAIL] dry-run changed git status output"
  echo "[INFO] before:"
  printf '%s\n' "$before_status"
  echo "[INFO] after:"
  printf '%s\n' "$after_status"
  exit 1
fi

reports_dir="$(printf '%s\n' "$output" | awk -F': ' 'index($0, "[INFO] reports_dir: ") == 1 {print $2; exit}')"
output_file="$(printf '%s\n' "$output" | awk -F': ' 'index($0, "[INFO] output_file: ") == 1 {print $2; exit}')"
bench_bin_dir="$(printf '%s\n' "$output" | awk -F': ' 'index($0, "[INFO] bench_bin_dir: ") == 1 {print $2; exit}')"
compile_unit_output_dir="$(printf '%s\n' "$output" | awk -F': ' 'index($0, "[INFO] compile_unit_output_dir: ") == 1 {print $2; exit}')"
module_reports_dir="$(printf '%s\n' "$output" | awk -F': ' 'index($0, "[INFO] module_reports_dir: ") == 1 {print $2; exit}')"
module_bin_dir="$(printf '%s\n' "$output" | awk -F': ' 'index($0, "[INFO] module_bin_dir: ") == 1 {print $2; exit}')"
module_unit_dir="$(printf '%s\n' "$output" | awk -F': ' 'index($0, "[INFO] module_unit_dir: ") == 1 {print $2; exit}')"

if [[ -z "$reports_dir" || -z "$output_file" || -z "$bench_bin_dir" || -z "$compile_unit_output_dir" || -z "$module_reports_dir" || -z "$module_bin_dir" || -z "$module_unit_dir" ]]; then
  echo "[FAIL] missing config lines in dry-run output"
  printf '%s\n' "$output"
  exit 1
fi

[[ "$reports_dir" == "$PROJECT_ROOT/tmp/test-reports" ]] || { echo "[FAIL] reports_dir mismatch: $reports_dir"; exit 1; }
[[ "$output_file" == "$PROJECT_ROOT/tmp/test-reports/wave_c_b101_validation_${RUN_ID}.md" ]] || { echo "[FAIL] output_file mismatch: $output_file"; exit 1; }
[[ "$bench_bin_dir" == "$PROJECT_ROOT/tmp/wave_c_b101_bench_bin_${RUN_ID}" ]] || { echo "[FAIL] bench_bin_dir mismatch: $bench_bin_dir"; exit 1; }
[[ "$compile_unit_output_dir" == "$PROJECT_ROOT/tmp/wave_c_b101_compile_units_${RUN_ID}" ]] || { echo "[FAIL] compile_unit_output_dir mismatch: $compile_unit_output_dir"; exit 1; }
[[ "$module_reports_dir" == "$PROJECT_ROOT/tmp/wave_c_b101_module_reports_${RUN_ID}" ]] || { echo "[FAIL] module_reports_dir mismatch: $module_reports_dir"; exit 1; }
[[ "$module_bin_dir" == "$PROJECT_ROOT/tmp/wave_c_b101_module_bin_${RUN_ID}" ]] || { echo "[FAIL] module_bin_dir mismatch: $module_bin_dir"; exit 1; }
[[ "$module_unit_dir" == "$PROJECT_ROOT/tmp/wave_c_b101_module_units_${RUN_ID}" ]] || { echo "[FAIL] module_unit_dir mismatch: $module_unit_dir"; exit 1; }

echo "[PASS] run_wave_c_b101_validation_playbook fast-local dry-run keeps workspace clean and uses tmp paths"
