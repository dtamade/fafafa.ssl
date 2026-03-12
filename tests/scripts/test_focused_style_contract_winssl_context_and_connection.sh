#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
OUT_FILE="$ROOT_DIR/tmp/test_focused_style_contract_winssl_context_and_connection.out"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] focused style contract for winssl context + connection"

mkdir -p "$ROOT_DIR/tmp"

if python3 "$ROOT_DIR/scripts/check_code_style.py" "$ROOT_DIR/src" >"$OUT_FILE" 2>&1; then
  :
fi

targets=(
  "src/fafafa.ssl.winssl.context.pas"
  "src/fafafa.ssl.winssl.connection.pas"
)

for target in "${targets[@]}"; do
  if grep -Fq "$target" "$OUT_FILE"; then
    echo "[INFO] remaining style issue(s) for: $target"
    grep -F "$target" "$OUT_FILE"
    fail "targeted style issues should be cleared for $target"
  fi
done

echo "[PASS] focused style contract for winssl context + connection passed"
