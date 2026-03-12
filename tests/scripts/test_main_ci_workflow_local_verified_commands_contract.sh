#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
CI_YML="$ROOT_DIR/.github/workflows/ci.yml"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] main ci workflow local verified commands contract"

if ! grep -Fq 'python3 scripts/compile_all_modules.py' "$CI_YML"; then
  fail "ci.yml should run python3 scripts/compile_all_modules.py"
fi

if ! grep -Fq 'bash scripts/run_minimal_ci_gate.sh --fast-local' "$CI_YML"; then
  fail "ci.yml should run bash scripts/run_minimal_ci_gate.sh --fast-local"
fi

if grep -Fq './scripts/run_all_module_tests.sh --verbose' "$CI_YML"; then
  fail "ci.yml should not use run_all_module_tests.sh as the main default verification command"
fi

echo "[PASS] main ci workflow local verified commands contract passed"
