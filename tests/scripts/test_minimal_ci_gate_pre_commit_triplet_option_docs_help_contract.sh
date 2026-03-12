#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_minimal_ci_gate.sh"
TRIPLET_CMD="bash scripts/run_minimal_ci_gate.sh --fast-local --skip-warning-noise-governance-batch --with-pre-commit-triplet-contract-batch"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate pre-commit triplet option docs+help contract"

HELP_OUT="$(bash "$SCRIPT" --help 2>&1)"
if [[ "$HELP_OUT" != *"--with-pre-commit-triplet-contract-batch"* ]]; then
  echo "$HELP_OUT"
  fail "help should include --with-pre-commit-triplet-contract-batch option"
fi

for doc in "README.md" "docs/AGENTS.md"; do
  TARGET="$ROOT_DIR/$doc"
  if [[ ! -f "$TARGET" ]]; then
    fail "missing doc file: $doc"
  fi

  if ! grep -Fq "$TRIPLET_CMD" "$TARGET"; then
    fail "doc missing triplet quick command: $doc"
  fi
done

echo "[PASS] minimal ci gate pre-commit triplet option docs+help contract passed"
