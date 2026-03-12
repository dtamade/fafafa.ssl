#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_minimal_ci_gate.sh"
WARNING_CMD="[GATE] cd '$ROOT_DIR' && bash tests/scripts/test_warning_noise_governance_contract_batch.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate fast-local preset contract"

set +e
OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --fast-local 2>&1)"
STATUS=$?
set -e

if [[ "$STATUS" -ne 0 ]]; then
  echo "$OUT"
  fail "fast-local preset should be accepted and keep dry-run success"
fi

if [[ "$OUT" != *"$WARNING_CMD"* ]]; then
  echo "$OUT"
  fail "fast-local preset should include warning-noise governance batch"
fi

if [[ "$OUT" == *"compile_all_modules.py"* ]]; then
  echo "$OUT"
  fail "fast-local preset should not run compile step"
fi

if [[ "$OUT" == *"run_all_module_tests.sh"* ]]; then
  echo "$OUT"
  fail "fast-local preset should not run module tests step"
fi

if [[ "$OUT" == *"run_phase2_performance_baseline.sh"* ]]; then
  echo "$OUT"
  fail "fast-local preset should not run phase2 dry-run step"
fi

if [[ "$OUT" == *"test_linux_multi_platform_path_checks_dryrun_batch.sh"* ]]; then
  echo "$OUT"
  fail "fast-local preset should not run platform path-check dry-run batch"
fi

if [[ "$OUT" == *"test_docs_active_noise_and_index_dedup_strict_batch.sh"* ]]; then
  echo "$OUT"
  fail "fast-local preset should not run docs governance strict batch"
fi

if [[ "$OUT" == *"check_tls13_signer_pure_pascal.sh"* ]]; then
  echo "$OUT"
  fail "fast-local preset should not run tls13 sign purity check"
fi

if [[ "$OUT" == *"run_freepascal_tls13_servercertverify_bench.sh"* ]]; then
  echo "$OUT"
  fail "fast-local preset should not run tls13 sign bench"
fi

if [[ "$OUT" == *"test_openssl_cert_verify_cache_policy_runtime.pas"* ]]; then
  echo "$OUT"
  fail "fast-local preset should not run openssl cache runtime regression"
fi

set +e
SKIP_OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --fast-local --skip-warning-noise-governance-batch 2>&1)"
SKIP_STATUS=$?
set -e

if [[ "$SKIP_STATUS" -ne 0 ]]; then
  echo "$SKIP_OUT"
  fail "fast-local preset with skip-warning should keep dry-run success"
fi

if [[ "$SKIP_OUT" == *"test_warning_noise_governance_contract_batch.sh"* ]]; then
  echo "$SKIP_OUT"
  fail "skip-warning flag should disable warning-noise governance in fast-local preset"
fi

echo "[PASS] minimal ci gate fast-local preset contract passed"
