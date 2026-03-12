#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
OUT_FILE="$ROOT_DIR/tmp/test_repo_hygiene_winssl_codepage_contract.out"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] repo hygiene WinSSL CODEPAGE contract"

mkdir -p "$ROOT_DIR/tmp"

if python3 "$ROOT_DIR/scripts/check_code_style.py" "$ROOT_DIR/src" >"$OUT_FILE" 2>&1; then
  :
fi

warnings=(
  "src/fafafa.ssl.winssl.session.pas 是 Windows 文件但缺少 {\$CODEPAGE UTF8}"
  "src/fafafa.ssl.winssl.native_handle.pas 是 Windows 文件但缺少 {\$CODEPAGE UTF8}"
)

for warning in "${warnings[@]}"; do
  if grep -Fq "$warning" "$OUT_FILE"; then
    echo "[INFO] offending warning: $warning"
    fail "WinSSL units should declare {\$CODEPAGE UTF8}"
  fi
done

echo "[PASS] repo hygiene WinSSL CODEPAGE contract passed"
