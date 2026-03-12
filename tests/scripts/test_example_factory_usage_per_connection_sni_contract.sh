#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

FILE="examples/example_factory_usage.pas"

if rg -n "Ctx\\.SetServerName\\(" "$FILE"; then
  echo "[FAIL] example_factory_usage should not teach deprecated context-level SetServerName"
  exit 1
fi

rg -F --quiet -- "ISSLClientConnection" "$FILE" || {
  echo "[FAIL] example_factory_usage should mention ISSLClientConnection in the recommended SNI path"
  exit 1
}

rg -F --quiet -- "ISSLClientConnection).SetServerName(" "$FILE" || {
  echo "[FAIL] example_factory_usage should show per-connection SetServerName usage"
  exit 1
}

echo "[PASS] example_factory_usage prefers per-connection SNI"
