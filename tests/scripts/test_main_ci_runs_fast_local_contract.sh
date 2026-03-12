#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORKFLOW="$ROOT_DIR/.github/workflows/ci.yml"

fail() {
  echo "[FAIL] $1"
  exit 1
}

rg -F --quiet -- 'python3 scripts/compile_all_modules.py' "$WORKFLOW" ||
  fail 'Main CI must compile core modules'

rg -F --quiet -- 'bash scripts/run_minimal_ci_gate.sh --fast-local' "$WORKFLOW" ||
  fail 'Main CI must run a real fast-local smoke step'

echo '[PASS] main CI runs compile gate and fast-local smoke'
