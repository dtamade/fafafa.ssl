#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_minimal_ci_gate.sh"
PLATFORM_CMD="[GATE] cd '$ROOT_DIR' && bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate only-platform mode contract"

OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --only-platform-path-check-dryrun 2>&1)"

if [[ "$OUT" != *"$PLATFORM_CMD"* ]]; then
  echo "$OUT"
  fail "only mode should invoke platform path-check dry-run batch"
fi

if [[ "$OUT" == *"compile_all_modules.py"* ]]; then
  echo "$OUT"
  fail "only mode should not run compile step"
fi

if [[ "$OUT" == *"run_all_module_tests.sh"* ]]; then
  echo "$OUT"
  fail "only mode should not run module tests step"
fi

if [[ "$OUT" == *"run_phase2_performance_baseline.sh"* ]]; then
  echo "$OUT"
  fail "only mode should not run phase2 dry-run step"
fi

if [[ "$OUT" == *"test_docs_active_noise_and_index_dedup_strict_batch.sh"* ]]; then
  echo "$OUT"
  fail "only mode should not run docs governance strict batch by default"
fi

if [[ "$OUT" == *"test_warning_noise_governance_contract_batch.sh"* ]]; then
  echo "$OUT"
  fail "only mode should not run warning-noise governance batch"
fi

if [[ "$OUT" == *"check_tls13_signer_pure_pascal.sh"* ]]; then
  echo "$OUT"
  fail "only mode should not run tls13 sign purity check"
fi

if [[ "$OUT" == *"run_freepascal_tls13_servercertverify_bench.sh"* ]]; then
  echo "$OUT"
  fail "only mode should not run tls13 sign bench"
fi

SKIP_OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --only-platform-path-check-dryrun --skip-platform-path-checks-dryrun 2>&1)"
if [[ "$SKIP_OUT" == *"test_linux_multi_platform_path_checks_dryrun_batch.sh"* ]]; then
  echo "$SKIP_OUT"
  fail "skip flag should disable platform path-check batch even in only mode"
fi

echo "[PASS] minimal ci gate only-platform mode contract passed"
