#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_minimal_ci_gate.sh"
WARNING_NOISE_BATCH_CMD="[GATE] cd '$ROOT_DIR' && bash tests/scripts/test_warning_noise_governance_contract_batch.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate warning-noise governance batch option contract"

DEFAULT_OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --skip-compile --skip-modules --skip-phase2-dryrun --skip-platform-path-checks-dryrun 2>&1)"
if [[ "$DEFAULT_OUT" != *"$WARNING_NOISE_BATCH_CMD"* ]]; then
  echo "$DEFAULT_OUT"
  fail "default dry-run should invoke warning-noise governance batch"
fi

SKIP_OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --skip-compile --skip-modules --skip-phase2-dryrun --skip-platform-path-checks-dryrun --skip-warning-noise-governance-batch 2>&1)"
if [[ "$SKIP_OUT" == *"test_warning_noise_governance_contract_batch.sh"* ]]; then
  echo "$SKIP_OUT"
  fail "skip flag should disable warning-noise governance batch"
fi

set +e
WITH_OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --skip-compile --skip-modules --skip-phase2-dryrun --skip-platform-path-checks-dryrun --with-warning-noise-governance-batch 2>&1)"
STATUS=$?
set -e

if [[ "$STATUS" -ne 0 ]]; then
  echo "$WITH_OUT"
  fail "enabled option should be accepted and keep dry-run success"
fi

if [[ "$WITH_OUT" != *"$WARNING_NOISE_BATCH_CMD"* ]]; then
  echo "$WITH_OUT"
  fail "opt-in flag should invoke warning-noise governance batch"
fi

echo "[PASS] minimal ci gate warning-noise governance batch option contract passed"
