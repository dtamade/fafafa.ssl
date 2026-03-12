#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
OUT_FILE="$ROOT_DIR/tmp/test_focused_style_contract_capability_diff_and_sha3.out"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] focused style contract for capability diff + api sha3"

mkdir -p "$ROOT_DIR/tmp"

if python3 "$ROOT_DIR/scripts/check_code_style.py" "$ROOT_DIR/src" >"$OUT_FILE" 2>&1; then
  :
fi

targets=(
  "src/fafafa.ssl.capability.diff.pas"
  "src/fafafa.ssl.openssl.api.sha3.pas"
)

for target in "${targets[@]}"; do
  if grep -Fq "$target" "$OUT_FILE"; then
    echo "[INFO] remaining style issue(s) for: $target"
    grep -F "$target" "$OUT_FILE"
    fail "targeted style issues should be cleared for $target"
  fi
done

echo "[PASS] focused style contract for capability diff + api sha3 passed"
