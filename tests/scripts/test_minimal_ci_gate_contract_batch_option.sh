#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_minimal_ci_gate.sh"
BATCH_CMD="[GATE] cd '$ROOT_DIR' && bash tests/scripts/test_minimal_ci_gate_contract_batch.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate contract-batch option contract"

DEFAULT_OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --skip-compile --skip-modules --skip-phase2-dryrun --skip-platform-path-checks-dryrun --skip-warning-noise-governance-batch 2>&1)"
if [[ "$DEFAULT_OUT" == *"test_minimal_ci_gate_contract_batch.sh"* ]]; then
  echo "$DEFAULT_OUT"
  fail "default dry-run should not invoke minimal gate contract batch"
fi

set +e
WITH_OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --skip-compile --skip-modules --skip-phase2-dryrun --skip-platform-path-checks-dryrun --skip-warning-noise-governance-batch --with-minimal-gate-contract-batch 2>&1)"
STATUS=$?
set -e

if [[ "$STATUS" -ne 0 ]]; then
  echo "$WITH_OUT"
  fail "opt-in flag should be accepted and keep dry-run success"
fi

if [[ "$WITH_OUT" != *"$BATCH_CMD"* ]]; then
  echo "$WITH_OUT"
  fail "opt-in flag should invoke minimal gate contract batch"
fi

OUT_ONLY="$(cd /tmp && bash "$SCRIPT" --dry-run --with-minimal-gate-contract-batch --only-platform-path-check-dryrun 2>&1)"
if [[ "$OUT_ONLY" == *"test_minimal_ci_gate_contract_batch.sh"* ]]; then
  echo "$OUT_ONLY"
  fail "only-platform preset should disable contract batch when it appears later"
fi

OUT_OVERRIDE="$(cd /tmp && bash "$SCRIPT" --dry-run --only-platform-path-check-dryrun --with-minimal-gate-contract-batch 2>&1)"
if [[ "$OUT_OVERRIDE" != *"test_minimal_ci_gate_contract_batch.sh"* ]]; then
  echo "$OUT_OVERRIDE"
  fail "last flag should win: explicit opt-in after only-platform should re-enable contract batch"
fi

echo "[PASS] minimal ci gate contract-batch option contract passed"
