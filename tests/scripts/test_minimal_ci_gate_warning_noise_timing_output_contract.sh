#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_minimal_ci_gate.sh"
WARNING_NOISE_BATCH_CMD="[GATE] cd '$ROOT_DIR' && bash tests/scripts/test_warning_noise_governance_contract_batch.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate warning-noise timing output contract"

set +e
OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --skip-compile --skip-modules --skip-phase2-dryrun --skip-platform-path-checks-dryrun 2>&1)"
STATUS=$?
set -e

if [[ "$STATUS" -ne 0 ]]; then
  echo "$OUT"
  fail "dry-run should succeed when warning-noise governance batch is enabled"
fi

if [[ "$OUT" != *"$WARNING_NOISE_BATCH_CMD"* ]]; then
  echo "$OUT"
  fail "expected warning-noise governance batch command in default lightweight dry-run"
fi

if [[ ! "$OUT" =~ \[INFO\]\ warning-noise\ governance\ elapsed_ms=[0-9]+ ]]; then
  echo "$OUT"
  fail "expected warning-noise timing observability line"
fi

set +e
SKIP_OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --skip-compile --skip-modules --skip-phase2-dryrun --skip-platform-path-checks-dryrun --skip-warning-noise-governance-batch 2>&1)"
SKIP_STATUS=$?
set -e

if [[ "$SKIP_STATUS" -ne 0 ]]; then
  echo "$SKIP_OUT"
  fail "skip-warning variant should keep dry-run success"
fi

if [[ "$SKIP_OUT" == *"warning-noise governance elapsed_ms="* ]]; then
  echo "$SKIP_OUT"
  fail "timing line should not appear when warning-noise governance batch is skipped"
fi

echo "[PASS] minimal ci gate warning-noise timing output contract passed"
