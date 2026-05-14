#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_b_ci_gate.sh"
WORK_REL="tmp/test_wave_b_ci_gate_dry_run_truth_$(date +%s)"
SUMMARY_REL="$WORK_REL/wave_b_ci_gate_summary_dryrun_truth.md"

rm -rf "$ROOT_DIR/$WORK_REL"
mkdir -p "$ROOT_DIR/$WORK_REL"
trap 'rm -rf "$ROOT_DIR/$WORK_REL"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

bash "$SCRIPT" \
  --dry-run \
  --run-id "dryrun_truth" \
  --reports-dir "$WORK_REL" \
  --summary-out "$SUMMARY_REL" \
  --with-tls13-sign-purity-check \
  --with-tls13-sign-bench >/dev/null

SUMMARY_ABS="$ROOT_DIR/$SUMMARY_REL"
if [[ ! -f "$SUMMARY_ABS" ]]; then
  fail "expected dry-run summary to be generated"
fi

if ! rg -n "^- Overall Status: \\*\\*DRY_RUN\\*\\*$" "$SUMMARY_ABS" >/dev/null; then
  fail "dry-run linux gate summary should record Overall Status DRY_RUN instead of pretending the gate passed"
fi

if ! rg -n "^- Mode: \`dry-run\`$" "$SUMMARY_ABS" >/dev/null; then
  fail "dry-run linux gate summary should record an explicit dry-run mode"
fi

for step in compile_all_modules run_all_module_tests verify_examples_compile tls13_signer_purity tls13_servercertverify_bench; do
  if ! rg -n "^\\| $step \\| \`0\` \\| \\*\\*DRY_RUN\\*\\* \\|" "$SUMMARY_ABS" >/dev/null; then
    fail "dry-run linux gate summary should mark enabled step '$step' as DRY_RUN"
  fi
done

if rg -n "\\*\\*PASS\\*\\*" "$SUMMARY_ABS" >/dev/null; then
  fail "dry-run linux gate summary should not leak PASS statuses into the report"
fi

echo "[PASS] wave_b_ci_gate dry-run truth contract passed"
