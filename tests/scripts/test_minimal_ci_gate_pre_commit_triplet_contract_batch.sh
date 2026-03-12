#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate pre-commit triplet contract batch"

SCRIPTS=(
  "tests/scripts/test_minimal_ci_gate_pre_commit_minimal_preset_contract.sh"
  "tests/scripts/test_minimal_ci_gate_pre_commit_docs_contract.sh"
  "tests/scripts/test_minimal_ci_gate_help_pre_commit_minimal_contract.sh"
)

for script in "${SCRIPTS[@]}"; do
  if [[ ! -f "$ROOT_DIR/$script" ]]; then
    fail "missing contract script: $script"
  fi

  if ! bash "$ROOT_DIR/$script"; then
    fail "contract failed: $script"
  fi
done

echo "[PASS] minimal ci gate pre-commit triplet contract batch passed"
