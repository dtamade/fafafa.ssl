#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

FILES=(tests/test_core_modules_only.pas tests/test_headers_validation.pas)

if rg -n '\bIsCryptoLibraryLoaded\b' "${FILES[@]}"; then
  echo '[FAIL] active validation programs should use TOpenSSLLoader.IsModuleLoaded(osmCore) instead of deprecated IsCryptoLibraryLoaded'
  exit 1
fi

if rg -n '\bGetOpenSSLVersion\b' "${FILES[@]}"; then
  echo '[FAIL] active validation programs should print GetOpenSSLVersionString instead of ambiguous GetOpenSSLVersion'
  exit 1
fi

for f in "${FILES[@]}"; do
  rg -F --quiet -- 'TOpenSSLLoader.IsModuleLoaded(osmCore)' "$f" || {
    echo "[FAIL] missing current loader state check in $f"
    exit 1
  }
  rg -F --quiet -- 'GetOpenSSLVersionString' "$f" || {
    echo "[FAIL] missing version string call in $f"
    exit 1
  }
done

echo '[PASS] active validation programs use current loader/version APIs'
