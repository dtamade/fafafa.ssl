#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
FILE="$ROOT_DIR/src/fafafa.ssl.winssl.context.pas"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] WinSSL session-cache runtime flag contract"

python3 - "$FILE" <<'PY'
from pathlib import Path
import re
import sys

path = Path(sys.argv[1])
text = path.read_text(encoding="utf-8")

def require(condition: bool, message: str) -> None:
    if not condition:
        print(f"[FAIL] {message}")
        raise SystemExit(1)
    print(f"[PASS] {message}")

def block(name: str) -> str:
    m = re.search(rf"procedure TWinSSLContext\.{name}(?:\([^)]*\))?;(.*?)^end;", text, re.S | re.M)
    require(m is not None, f"WinSSL context implements {name}")
    return m.group(1)

set_cache_mode = block("SetSessionCacheMode")
set_options = block("SetOptions")
ensure_credentials = block("EnsureCredentialsAcquired")

require("FCredentialsNeedRebuild := True;" in set_cache_mode,
        "SetSessionCacheMode must force credential rebuild when session-cache mode changes")

require("FCredentialsNeedRebuild := True;" in set_options,
        "SetOptions must force credential rebuild when session/ticket-related options change")

require("SCH_CRED_DISABLE_RECONNECTS" in ensure_credentials,
        "EnsureCredentialsAcquired must map WinSSL session-cache/ticket disablement to SCH_CRED_DISABLE_RECONNECTS")

require(("not FSessionCacheEnabled" in ensure_credentials) and
        ("ssoEnableSessionTickets in FOptions" in ensure_credentials),
        "EnsureCredentialsAcquired must derive reconnect disablement from session-cache mode and session-ticket option truth")
PY

echo "[PASS] WinSSL session-cache runtime flag contract passed"
