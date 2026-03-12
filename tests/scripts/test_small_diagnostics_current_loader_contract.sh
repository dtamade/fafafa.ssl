#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

FILES=(tests/test_handle.pas tests/crypto/test_load_rand_detailed.pas)

if rg -n '\bIsCryptoLibraryLoaded\b' "${FILES[@]}"; then
  echo '[FAIL] small diagnostic programs should use TOpenSSLLoader.IsModuleLoaded(osmCore) instead of deprecated IsCryptoLibraryLoaded'
  exit 1
fi

echo '[PASS] small diagnostic programs use current loader API'
