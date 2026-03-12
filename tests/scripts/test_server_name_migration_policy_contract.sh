#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

BASE_FILE="src/fafafa.ssl.base.pas"
ARCH_FILE="docs/reference/ARCHITECTURE.md"
README_FILE="README.md"

rg -F --quiet -- 'ISSLContext.CreateConnection(...) 之后，通过 ISSLClientConnection.SetServerName(...) 设置' "$BASE_FILE" || {
  echo '[FAIL] base interface docs should describe the preferred per-connection migration path'
  exit 1
}

rg -F --quiet -- '仅作为后续 client connection 的默认 fallback' "$BASE_FILE" || {
  echo '[FAIL] base interface docs should describe context ServerName as a client fallback only'
  exit 1
}

rg -F --quiet -- 'server connection 不继承' "$BASE_FILE" || {
  echo '[FAIL] base interface docs should document that server connections do not inherit context ServerName'
  exit 1
}

rg -F --quiet -- '### 3.3.2 ServerName 迁移策略（2026-03-09）' "$ARCH_FILE" || {
  echo '[FAIL] architecture doc should include a dedicated ServerName migration policy section'
  exit 1
}

rg -F --quiet -- 'connection override > context default > empty' "$ARCH_FILE" || {
  echo '[FAIL] architecture doc should record the ServerName precedence rule'
  exit 1
}

rg -F --quiet -- '仅 client connection 会读取这个 fallback；server connection 不继承' "$ARCH_FILE" || {
  echo '[FAIL] architecture doc should record the client-only fallback boundary'
  exit 1
}

rg -F --quiet -- 'Prefer per-connection SNI' "$README_FILE" || {
  echo '[FAIL] README should call out the preferred per-connection SNI usage'
  exit 1
}

rg -F --quiet -- 'Ctx.SetServerName(...) remains deprecated compatibility' "$README_FILE" || {
  echo '[FAIL] README should describe context ServerName as deprecated compatibility only'
  exit 1
}

echo '[PASS] ServerName migration policy is documented and discoverable'
