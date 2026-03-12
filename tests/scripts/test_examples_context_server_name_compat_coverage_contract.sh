#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

FILES=(
  "tests/examples/test_basic.pas"
  "tests/examples/test_lib_core_functionality.pas"
)

for f in "${FILES[@]}"; do
  rg -F --quiet -- "Deprecated compatibility coverage:" "$f" || {
    echo "[FAIL] missing explicit compatibility marker in $f"
    exit 1
  }

  rg -F --quiet -- "SetServerName(" "$f" || {
    echo "[FAIL] expected context-level SetServerName coverage in $f"
    exit 1
  }
done

echo "[PASS] example tests intentionally retain context-level ServerName compatibility coverage"
