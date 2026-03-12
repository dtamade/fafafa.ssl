#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

[[ -f docs/testing/CURRENT_HEALTH.md ]] || fail 'Missing current-health doc'
rg -F --quiet -- 'python3 scripts/compile_all_modules.py' docs/testing/CURRENT_HEALTH.md ||
  fail 'Current-health doc must include compile gate'
rg -F --quiet -- 'bash scripts/run_minimal_ci_gate.sh --fast-local' docs/testing/CURRENT_HEALTH.md ||
  fail 'Current-health doc must include fast-local gate'
rg -F --quiet -- 'CURRENT_HEALTH.md' docs/README.md ||
  fail 'Docs homepage must link to the current-health doc'

echo '[PASS] current-health doc is present and linked'
