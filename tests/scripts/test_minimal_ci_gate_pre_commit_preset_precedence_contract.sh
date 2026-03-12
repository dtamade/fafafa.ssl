#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_minimal_ci_gate.sh"

PLATFORM_CMD="[GATE] cd '$ROOT_DIR' && bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh"
WARNING_CMD="[GATE] cd '$ROOT_DIR' && bash tests/scripts/test_warning_noise_governance_contract_batch.sh"
CONTRACT_BATCH_CMD="[GATE] cd '$ROOT_DIR' && bash tests/scripts/test_minimal_ci_gate_contract_batch.sh"
TLS13_BENCH_SNIPPET="run_freepascal_tls13_servercertverify_bench.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate pre-commit preset precedence contract"

# Case 1: last preset = only-platform
OUT_1="$(cd /tmp && bash "$SCRIPT" --dry-run --pre-commit-minimal --only-platform-path-check-dryrun 2>&1)"
if [[ "$OUT_1" != *"$PLATFORM_CMD"* ]]; then
  echo "$OUT_1"
  fail "case1: last preset only-platform should include platform batch"
fi
if [[ "$OUT_1" == *"$CONTRACT_BATCH_CMD"* ]]; then
  echo "$OUT_1"
  fail "case1: last preset only-platform should disable minimal gate contract batch"
fi

# Case 2: last preset = pre-commit
OUT_2="$(cd /tmp && bash "$SCRIPT" --dry-run --only-platform-path-check-dryrun --pre-commit-minimal 2>&1)"
if [[ "$OUT_2" != *"$CONTRACT_BATCH_CMD"* ]]; then
  echo "$OUT_2"
  fail "case2: last preset pre-commit should include minimal gate contract batch"
fi
if [[ "$OUT_2" == *"$PLATFORM_CMD"* ]]; then
  echo "$OUT_2"
  fail "case2: last preset pre-commit should disable platform batch"
fi
if [[ "$OUT_2" == *"$WARNING_CMD"* ]]; then
  echo "$OUT_2"
  fail "case2: pre-commit preset should keep warning-noise governance disabled"
fi

# Case 3: last preset = only-tls13
OUT_3="$(cd /tmp && bash "$SCRIPT" --dry-run --pre-commit-minimal --only-tls13-sign-bench 2>&1)"
if [[ "$OUT_3" != *"$TLS13_BENCH_SNIPPET"* ]]; then
  echo "$OUT_3"
  fail "case3: last preset only-tls13 should include tls13 bench"
fi
if [[ "$OUT_3" == *"$CONTRACT_BATCH_CMD"* ]]; then
  echo "$OUT_3"
  fail "case3: last preset only-tls13 should disable minimal gate contract batch"
fi

# Case 4: last preset = pre-commit (after only-tls13)
OUT_4="$(cd /tmp && bash "$SCRIPT" --dry-run --only-tls13-sign-bench --pre-commit-minimal 2>&1)"
if [[ "$OUT_4" != *"$CONTRACT_BATCH_CMD"* ]]; then
  echo "$OUT_4"
  fail "case4: last preset pre-commit should include minimal gate contract batch"
fi
if [[ "$OUT_4" == *"$TLS13_BENCH_SNIPPET"* ]]; then
  echo "$OUT_4"
  fail "case4: last preset pre-commit should disable tls13 bench"
fi

# Case 5: last preset = fast-local
OUT_5="$(cd /tmp && bash "$SCRIPT" --dry-run --pre-commit-minimal --fast-local 2>&1)"
if [[ "$OUT_5" != *"$WARNING_CMD"* ]]; then
  echo "$OUT_5"
  fail "case5: last preset fast-local should include warning-noise governance batch"
fi
if [[ "$OUT_5" == *"$CONTRACT_BATCH_CMD"* ]]; then
  echo "$OUT_5"
  fail "case5: last preset fast-local should disable minimal gate contract batch"
fi

# Case 6: explicit opt-in after only-platform should re-enable contract batch
OUT_6="$(cd /tmp && bash "$SCRIPT" --dry-run --pre-commit-minimal --only-platform-path-check-dryrun --with-minimal-gate-contract-batch 2>&1)"
if [[ "$OUT_6" != *"$PLATFORM_CMD"* ]]; then
  echo "$OUT_6"
  fail "case6: only-platform should keep platform batch enabled"
fi
if [[ "$OUT_6" != *"$CONTRACT_BATCH_CMD"* ]]; then
  echo "$OUT_6"
  fail "case6: explicit contract-batch opt-in after only-platform should re-enable contract batch"
fi

echo "[PASS] minimal ci gate pre-commit preset precedence contract passed"
