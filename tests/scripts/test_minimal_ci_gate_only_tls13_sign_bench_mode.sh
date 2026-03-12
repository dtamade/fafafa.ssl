#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_minimal_ci_gate.sh"
TLS13_BENCH_CMD="[GATE] cd '$ROOT_DIR' && FAFAFA_TLS13_SIGN_BENCH_ITERATIONS='3'"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate only-tls13-sign-bench mode contract"

OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --only-tls13-sign-bench 2>&1)"

if [[ "$OUT" != *"$TLS13_BENCH_CMD"* ]] || [[ "$OUT" != *"run_freepascal_tls13_servercertverify_bench.sh"* ]]; then
  echo "$OUT"
  fail "only-tls13 mode should invoke tls13 sign bench command"
fi

if [[ "$OUT" == *"compile_all_modules.py"* ]]; then
  echo "$OUT"
  fail "only-tls13 mode should not run compile step"
fi

if [[ "$OUT" == *"run_all_module_tests.sh"* ]]; then
  echo "$OUT"
  fail "only-tls13 mode should not run module tests step"
fi

if [[ "$OUT" == *"run_phase2_performance_baseline.sh"* ]]; then
  echo "$OUT"
  fail "only-tls13 mode should not run phase2 dry-run step"
fi

if [[ "$OUT" == *"test_linux_multi_platform_path_checks_dryrun_batch.sh"* ]]; then
  echo "$OUT"
  fail "only-tls13 mode should not run platform path-check dry-run batch"
fi

if [[ "$OUT" == *"test_docs_active_noise_and_index_dedup_strict_batch.sh"* ]]; then
  echo "$OUT"
  fail "only-tls13 mode should not run docs governance strict batch"
fi

if [[ "$OUT" == *"test_warning_noise_governance_contract_batch.sh"* ]]; then
  echo "$OUT"
  fail "only-tls13 mode should not run warning-noise governance batch"
fi

if [[ "$OUT" == *"check_tls13_signer_pure_pascal.sh"* ]]; then
  echo "$OUT"
  fail "only-tls13 mode should not run tls13 sign purity check"
fi

if [[ "$OUT" == *"test_openssl_cert_verify_cache_policy_runtime.pas"* ]]; then
  echo "$OUT"
  fail "only-tls13 mode should not run openssl cache runtime regression"
fi

echo "[PASS] minimal ci gate only-tls13-sign-bench mode contract passed"
