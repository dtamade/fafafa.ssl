#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

guide="docs/guides/EARLY_DATA_GUIDE.md"

require_fixed() {
  local needle="$1"
  local file="$2"
  local message="$3"

  if ! rg -F -n --quiet -- "$needle" "$file"; then
    echo "[FAIL] $message"
    exit 1
  fi
}

require_fixed '这里直接回到 `CreateConnection(...)`，是因为 `ISSLEarlyDataContext` / `ISSLEarlyDataConnection` 这组 early-data owner surface 分别挂在 context / connection 对象上；如果你只是普通客户端接入而不需要 early-data owner surface，握手入口仍可保持在 `TSSLConnector` / `TSSLStream`。' \
  "$guide" \
  "EARLY_DATA_GUIDE must explain why it intentionally uses context/connection owner surfaces"

echo "[PASS] early-data guide explains why it intentionally uses context/connection owner surfaces"
