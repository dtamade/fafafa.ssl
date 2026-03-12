#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_minimal_ci_gate.sh"
TRIPLET_CMD="[GATE] cd '$ROOT_DIR' && bash tests/scripts/test_minimal_ci_gate_pre_commit_triplet_contract_batch.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate pre-commit triplet batch option contract"

DEFAULT_OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --skip-compile --skip-modules --skip-phase2-dryrun --skip-platform-path-checks-dryrun --skip-warning-noise-governance-batch 2>&1)"
if [[ "$DEFAULT_OUT" == *"test_minimal_ci_gate_pre_commit_triplet_contract_batch.sh"* ]]; then
  echo "$DEFAULT_OUT"
  fail "default dry-run should not invoke pre-commit triplet contract batch"
fi

set +e
WITH_OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --skip-compile --skip-modules --skip-phase2-dryrun --skip-platform-path-checks-dryrun --skip-warning-noise-governance-batch --with-pre-commit-triplet-contract-batch 2>&1)"
STATUS=$?
set -e

if [[ "$STATUS" -ne 0 ]]; then
  echo "$WITH_OUT"
  fail "opt-in flag should be accepted and keep dry-run success"
fi

if [[ "$WITH_OUT" != *"$TRIPLET_CMD"* ]]; then
  echo "$WITH_OUT"
  fail "opt-in flag should invoke pre-commit triplet contract batch"
fi

OUT_ONLY="$(cd /tmp && bash "$SCRIPT" --dry-run --with-pre-commit-triplet-contract-batch --only-platform-path-check-dryrun 2>&1)"
if [[ "$OUT_ONLY" == *"test_minimal_ci_gate_pre_commit_triplet_contract_batch.sh"* ]]; then
  echo "$OUT_ONLY"
  fail "only-platform preset should disable pre-commit triplet batch when it appears later"
fi

OUT_OVERRIDE="$(cd /tmp && bash "$SCRIPT" --dry-run --only-platform-path-check-dryrun --with-pre-commit-triplet-contract-batch 2>&1)"
if [[ "$OUT_OVERRIDE" != *"test_minimal_ci_gate_pre_commit_triplet_contract_batch.sh"* ]]; then
  echo "$OUT_OVERRIDE"
  fail "last flag should win: explicit opt-in after only-platform should re-enable pre-commit triplet batch"
fi

echo "[PASS] minimal ci gate pre-commit triplet batch option contract passed"
