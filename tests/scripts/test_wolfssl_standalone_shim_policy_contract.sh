#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

WOLF_FILE="src/fafafa.ssl.wolfssl.connection.pas"
ARCH_FILE="docs/reference/ARCHITECTURE.md"

rg -F --quiet -- '兼容策略：保留公开类名作为历史入口，但 runtime 真相源固定在 `fafafa.ssl.wolfssl.context`。' "$WOLF_FILE" || {
  echo '[FAIL] wolfssl standalone unit header should document the compatibility-shim policy'
  exit 1
}

rg -F --quiet -- 'FInner := AContext.CreateConnection(ASocket);' "$WOLF_FILE" || {
  echo '[FAIL] wolfssl standalone shim should delegate socket creation to context runtime path'
  exit 1
}

rg -F --quiet -- 'FInner := AContext.CreateConnection(AStream);' "$WOLF_FILE" || {
  echo '[FAIL] wolfssl standalone shim should delegate stream creation to context runtime path'
  exit 1
}

rg -F --quiet -- '### 3.4.1 WolfSSL standalone shim policy（2026-03-09）' "$ARCH_FILE" || {
  echo '[FAIL] architecture doc should include a dedicated WolfSSL shim policy section'
  exit 1
}

rg -F --quiet -- 'standalone `fafafa.ssl.wolfssl.connection` 保留为兼容入口，不再承载第二套运行时实现。' "$ARCH_FILE" || {
  echo '[FAIL] architecture doc should declare the standalone WolfSSL unit as compatibility entry only'
  exit 1
}

rg -F --quiet -- 'runtime 真相源固定在 `fafafa.ssl.wolfssl.context` 的 `TWolfSSLContext.CreateConnection(...)` 路径。' "$ARCH_FILE" || {
  echo '[FAIL] architecture doc should point to the runtime source of truth'
  exit 1
}

echo '[PASS] WolfSSL standalone shim policy is explicit and enforced'
