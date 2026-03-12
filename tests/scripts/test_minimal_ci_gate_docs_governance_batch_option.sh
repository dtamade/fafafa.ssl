#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_minimal_ci_gate.sh"
DOCS_BATCH_CMD="[GATE] cd '$ROOT_DIR' && bash tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate docs governance strict batch option contract"

DEFAULT_OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --skip-compile --skip-modules --skip-phase2-dryrun --skip-platform-path-checks-dryrun 2>&1)"
if [[ "$DEFAULT_OUT" == *"test_docs_active_noise_and_index_dedup_strict_batch.sh"* ]]; then
  echo "$DEFAULT_OUT"
  fail "default dry-run should not invoke docs governance strict batch"
fi

WITH_OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --skip-compile --skip-modules --skip-phase2-dryrun --skip-platform-path-checks-dryrun --with-docs-governance-strict-batch 2>&1)"
if [[ "$WITH_OUT" != *"$DOCS_BATCH_CMD"* ]]; then
  echo "$WITH_OUT"
  fail "opt-in flag should invoke docs governance strict batch"
fi

echo "[PASS] minimal ci gate docs governance strict batch option contract passed"
