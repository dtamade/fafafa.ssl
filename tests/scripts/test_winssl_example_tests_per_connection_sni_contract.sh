#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

FILES=(
  "tests/examples/test_winssl_simple.pas"
  "tests/examples/test_winssl_debug.pas"
  "tests/examples/test_winssl.pas"
  "tests/examples/test_performance.pas"
  "tests/examples/test_certchain.pas"
)

for f in "${FILES[@]}"; do
  if rg -n "Context\\.SetServerName\\(" "$f"; then
    echo "[FAIL] deprecated context-level SetServerName should not appear in $f"
    exit 1
  fi

  rg -F --quiet -- "ISSLClientConnection" "$f" || {
    echo "[FAIL] missing ISSLClientConnection path in $f"
    exit 1
  }

  rg -F --quiet -- "Supports(Connection, ISSLClientConnection" "$f" || {
    echo "[FAIL] missing per-connection cast path in $f"
    exit 1
  }

  if [[ "$f" == *"test_certchain.pas" ]]; then
    rg -F --quiet -- "ClientConn.SetServerName('www.google.com');" "$f" || {
      echo "[FAIL] missing per-connection SetServerName in $f"
      exit 1
    }
    continue
  fi

  rg -F --quiet -- "ClientConn.SetServerName(TEST_HOST);" "$f" || {
    echo "[FAIL] missing per-connection SetServerName in $f"
    exit 1
  }
done

echo "[PASS] WinSSL example-style tests prefer per-connection SNI"
