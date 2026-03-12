#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
OUT_FILE="$ROOT_DIR/tmp/test_focused_style_contract_cert_rotation.out"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] focused style contract for cert rotation"

mkdir -p "$ROOT_DIR/tmp"

if python3 "$ROOT_DIR/scripts/check_code_style.py" "$ROOT_DIR/src" >"$OUT_FILE" 2>&1; then
  :
fi

target='src/fafafa.ssl.cert.rotation.pas'

if grep -Fq "$target" "$OUT_FILE"; then
  echo "[INFO] remaining style issue(s) for: $target"
  grep -F "$target" "$OUT_FILE"
  fail "targeted style issues should be cleared for $target"
fi

echo "[PASS] focused style contract for cert rotation passed"
