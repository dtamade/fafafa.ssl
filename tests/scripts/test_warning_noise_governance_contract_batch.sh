#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] warning-noise governance contract batch"

SCRIPTS=(
  "tests/scripts/test_focused_compile_zero_noise_contract.sh"
  "tests/scripts/test_focused_compile_zero_noise_ocsp_regression_contract.sh"
  "tests/scripts/test_deprecated_warning_scope_whitelist_contract.sh"
)

for script in "${SCRIPTS[@]}"; do
  if [[ ! -f "$ROOT_DIR/$script" ]]; then
    fail "missing contract script: $script"
  fi

  if ! bash "$ROOT_DIR/$script"; then
    fail "contract failed: $script"
  fi

done

echo "[PASS] warning-noise governance contract batch passed"
