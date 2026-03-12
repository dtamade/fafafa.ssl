#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

FILES=(
  "examples/winssl_https_downloader.pas"
  "examples/winssl_rest_client.pas"
  "examples/winssl_health_checker.pas"
)

for f in "${FILES[@]}"; do
  if rg -n "\bL(Ctx|Context|SSLContext)\.SetServerName\(" "$f"; then
    echo "[FAIL] deprecated context-level SetServerName should not appear in $f"
    exit 1
  fi

  rg -F --quiet -- "Supports(LConn, ISSLClientConnection" "$f" || {
    echo "[FAIL] missing per-connection ISSLClientConnection path in $f"
    exit 1
  }

  rg -F --quiet -- "LClientConn.SetServerName(" "$f" || {
    echo "[FAIL] missing per-connection SetServerName call in $f"
    exit 1
  }
done

echo '[PASS] WinSSL network examples prefer per-connection SNI'
