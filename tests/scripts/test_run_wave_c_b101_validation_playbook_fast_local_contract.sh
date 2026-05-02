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
RUN_ID="contract_wave_c_b101_exec_$(date +%s)_$$"

set +e
output="$(bash scripts/run_wave_c_b101_validation_playbook.sh --fast-local --run-id "$RUN_ID" --strict 2>&1)"
exit_code=$?
set -e

if [[ "$exit_code" -ne 0 ]]; then
  echo "[FAIL] run_wave_c_b101_validation_playbook fast-local should exit 0 (got: $exit_code)"
  printf '%s\n' "$output"
  exit 1
fi

report_file="$PROJECT_ROOT/tmp/test-reports/wave_c_b101_validation_${RUN_ID}.md"
bench_compile_log="$PROJECT_ROOT/tmp/test-reports/wave_c_b101_bench_compile_${RUN_ID}.log"
bench_run_log="$PROJECT_ROOT/tmp/test-reports/wave_c_b101_bench_run_${RUN_ID}.log"

for file in "$report_file" "$bench_compile_log" "$bench_run_log"; do
  if [[ ! -f "$file" ]]; then
    echo "[FAIL] expected output file missing: $file"
    exit 1
  fi
done

if ! rg -F --quiet -- "- overall: **PASS**" "$report_file"; then
  echo "[FAIL] report should record overall PASS"
  sed -n '1,160p' "$report_file" || true
  exit 1
fi

if ! rg -F --quiet -- "- hit_rate_percent:" "$report_file"; then
  echo "[FAIL] report missing hit rate snapshot"
  sed -n '1,160p' "$report_file" || true
  exit 1
fi

if ! rg -F --quiet "Speedup Factor:" "$bench_run_log"; then
  echo "[FAIL] benchmark run log missing speedup output"
  sed -n '1,200p' "$bench_run_log" || true
  exit 1
fi

after_status="$(git status --porcelain)"
if [[ "$before_status" != "$after_status" ]]; then
  echo "[FAIL] fast-local execution changed git status output"
  echo "[INFO] before:"
  printf '%s\n' "$before_status"
  echo "[INFO] after:"
  printf '%s\n' "$after_status"
  exit 1
fi

echo "[PASS] run_wave_c_b101_validation_playbook fast-local execution stays clean and writes reports under tmp"
