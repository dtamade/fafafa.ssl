#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_minimal_ci_gate.sh"

PLATFORM_CMD="[GATE] cd '$ROOT_DIR' && bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh"
WARNING_CMD="[GATE] cd '$ROOT_DIR' && bash tests/scripts/test_warning_noise_governance_contract_batch.sh"
TLS13_BENCH_SNIPPET="run_freepascal_tls13_servercertverify_bench.sh"
RUNTIME_SNIPPET="test_openssl_cert_verify_cache_policy_runtime.pas"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate preset precedence last-flag-wins contract"

# Case 1: last preset = only-platform
OUT_1="$(cd /tmp && bash "$SCRIPT" --dry-run --fast-local --only-platform-path-check-dryrun 2>&1)"
if [[ "$OUT_1" != *"$PLATFORM_CMD"* ]]; then
  echo "$OUT_1"
  fail "case1: last preset only-platform should include platform batch"
fi
if [[ "$OUT_1" == *"test_warning_noise_governance_contract_batch.sh"* ]]; then
  echo "$OUT_1"
  fail "case1: last preset only-platform should disable warning-noise batch"
fi
if [[ "$OUT_1" == *"$TLS13_BENCH_SNIPPET"* ]]; then
  echo "$OUT_1"
  fail "case1: last preset only-platform should disable tls13 bench"
fi
if [[ "$OUT_1" == *"$RUNTIME_SNIPPET"* ]]; then
  echo "$OUT_1"
  fail "case1: last preset only-platform should disable runtime cache regression"
fi

# Case 2: last preset = fast-local
OUT_2="$(cd /tmp && bash "$SCRIPT" --dry-run --only-platform-path-check-dryrun --fast-local 2>&1)"
if [[ "$OUT_2" != *"$WARNING_CMD"* ]]; then
  echo "$OUT_2"
  fail "case2: last preset fast-local should include warning-noise batch"
fi
if [[ "$OUT_2" == *"test_linux_multi_platform_path_checks_dryrun_batch.sh"* ]]; then
  echo "$OUT_2"
  fail "case2: last preset fast-local should disable platform batch"
fi
if [[ "$OUT_2" == *"$TLS13_BENCH_SNIPPET"* ]]; then
  echo "$OUT_2"
  fail "case2: last preset fast-local should disable tls13 bench"
fi
if [[ "$OUT_2" == *"$RUNTIME_SNIPPET"* ]]; then
  echo "$OUT_2"
  fail "case2: last preset fast-local should disable runtime cache regression"
fi

# Case 3: last preset = only-tls13
OUT_3="$(cd /tmp && bash "$SCRIPT" --dry-run --fast-local --only-tls13-sign-bench 2>&1)"
if [[ "$OUT_3" != *"$TLS13_BENCH_SNIPPET"* ]]; then
  echo "$OUT_3"
  fail "case3: last preset only-tls13 should include tls13 bench"
fi
if [[ "$OUT_3" == *"test_warning_noise_governance_contract_batch.sh"* ]]; then
  echo "$OUT_3"
  fail "case3: last preset only-tls13 should disable warning-noise batch"
fi
if [[ "$OUT_3" == *"test_linux_multi_platform_path_checks_dryrun_batch.sh"* ]]; then
  echo "$OUT_3"
  fail "case3: last preset only-tls13 should disable platform batch"
fi
if [[ "$OUT_3" == *"$RUNTIME_SNIPPET"* ]]; then
  echo "$OUT_3"
  fail "case3: last preset only-tls13 should disable runtime cache regression"
fi

# Case 4: last preset = fast-local (after only-tls13)
OUT_4="$(cd /tmp && bash "$SCRIPT" --dry-run --only-tls13-sign-bench --fast-local 2>&1)"
if [[ "$OUT_4" != *"$WARNING_CMD"* ]]; then
  echo "$OUT_4"
  fail "case4: last preset fast-local should include warning-noise batch"
fi
if [[ "$OUT_4" == *"$TLS13_BENCH_SNIPPET"* ]]; then
  echo "$OUT_4"
  fail "case4: last preset fast-local should disable tls13 bench"
fi
if [[ "$OUT_4" == *"test_linux_multi_platform_path_checks_dryrun_batch.sh"* ]]; then
  echo "$OUT_4"
  fail "case4: last preset fast-local should disable platform batch"
fi
if [[ "$OUT_4" == *"$RUNTIME_SNIPPET"* ]]; then
  echo "$OUT_4"
  fail "case4: last preset fast-local should disable runtime cache regression"
fi

# Case 5: only-platform should override prior runtime opt-in
OUT_5="$(cd /tmp && bash "$SCRIPT" --dry-run --with-openssl-cert-verify-cache-runtime --only-platform-path-check-dryrun 2>&1)"
if [[ "$OUT_5" == *"$RUNTIME_SNIPPET"* ]]; then
  echo "$OUT_5"
  fail "case5: only-platform should suppress runtime cache regression step"
fi
if [[ "$OUT_5" != *"$PLATFORM_CMD"* ]]; then
  echo "$OUT_5"
  fail "case5: only-platform should still include platform batch"
fi

echo "[PASS] minimal ci gate preset precedence last-flag-wins contract passed"
