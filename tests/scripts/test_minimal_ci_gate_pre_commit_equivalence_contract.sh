#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_minimal_ci_gate.sh"
CONTRACT_BATCH_CMD="[GATE] cd '$ROOT_DIR' && bash tests/scripts/test_minimal_ci_gate_contract_batch.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate pre-commit equivalence contract"

set +e
OUT_PRECOMMIT="$(cd /tmp && bash "$SCRIPT" --dry-run --pre-commit-minimal 2>&1)"
STATUS_PRECOMMIT=$?
OUT_EXPLICIT="$(cd /tmp && bash "$SCRIPT" --dry-run --fast-local --skip-warning-noise-governance-batch --with-minimal-gate-contract-batch 2>&1)"
STATUS_EXPLICIT=$?
set -e

if [[ "$STATUS_PRECOMMIT" -ne 0 ]]; then
  echo "$OUT_PRECOMMIT"
  fail "pre-commit-minimal command should keep dry-run success"
fi

if [[ "$STATUS_EXPLICIT" -ne 0 ]]; then
  echo "$OUT_EXPLICIT"
  fail "explicit equivalent command should keep dry-run success"
fi

GATES_PRECOMMIT="$(printf '%s\n' "$OUT_PRECOMMIT" | grep '^\[GATE\]' || true)"
GATES_EXPLICIT="$(printf '%s\n' "$OUT_EXPLICIT" | grep '^\[GATE\]' || true)"

if [[ -z "$GATES_PRECOMMIT" ]]; then
  echo "$OUT_PRECOMMIT"
  fail "pre-commit-minimal should emit at least one gate command"
fi

if [[ "$GATES_PRECOMMIT" != "$GATES_EXPLICIT" ]]; then
  echo "[INFO] pre-commit gate sequence:"
  printf '%s\n' "$GATES_PRECOMMIT"
  echo "[INFO] explicit gate sequence:"
  printf '%s\n' "$GATES_EXPLICIT"
  fail "pre-commit-minimal should be behaviorally equivalent to explicit 3-flag command"
fi

if [[ "$GATES_PRECOMMIT" != *"$CONTRACT_BATCH_CMD"* ]]; then
  echo "$OUT_PRECOMMIT"
  fail "equivalent command sequence should include minimal gate contract batch"
fi

echo "[PASS] minimal ci gate pre-commit equivalence contract passed"
