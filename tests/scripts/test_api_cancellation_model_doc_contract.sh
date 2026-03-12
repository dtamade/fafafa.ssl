#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
DOC_FILE="$ROOT_DIR/docs/reference/API_CANCELLATION_MODEL.md"
INDEX_FILE="$ROOT_DIR/docs/reference/API_CONTRACT_CURRENT_INDEX.md"
ARCH_FILE="$ROOT_DIR/docs/reference/ARCHITECTURE.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] API cancellation model doc contract"

[[ -f "$DOC_FILE" ]] || fail "missing docs/reference/API_CANCELLATION_MODEL.md"

rg -F --quiet -- "当前没有独立的 connection-level Cancel API" "$DOC_FILE" || {
  fail "cancellation model doc must state the absence of a dedicated Cancel API"
}

rg -F --quiet -- '`Close` 是当前唯一的强制中断原语' "$DOC_FILE" || {
  fail "cancellation model doc must explain Close as the current force-abort primitive"
}

rg -F --quiet -- '`Shutdown` 是 graceful close，不是 cancel' "$DOC_FILE" || {
  fail "cancellation model doc must distinguish Shutdown from cancellation"
}

rg -F --quiet -- '`SetTimeout(...)` 负责 deadline / budget，不等于 cancel' "$DOC_FILE" || {
  fail "cancellation model doc must distinguish timeout from cancellation"
}

rg -F --quiet -- '`docs/reference/API_CANCELLATION_MODEL.md`' "$INDEX_FILE" || {
  fail "API contract index must link the cancellation model doc"
}

rg -F --quiet -- "取消 / timeout / close" "$ARCH_FILE" || {
  fail "architecture doc should mention cancellation model alongside timeout/close"
}

echo "[PASS] API cancellation model doc contract passed"
