#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
EXEMPT_FILE="src/fafafa.ssl.openssl.cert.builder.pas"
PATTERN="fafafa\\.ssl\\.cert\\.builder\\.ICertificateEx|fafafa\\.ssl\\.cert\\.builder\\.IPrivateKeyEx"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] deprecated ICertificateEx namespace regression contract"

mapfile -t HITS < <(
  cd "$ROOT_DIR"
  rg -n --no-heading --color never "$PATTERN" src tests || true
)

UNEXPECTED=()
for hit in "${HITS[@]}"; do
  file="${hit%%:*}"
  if [[ "$file" != "$EXEMPT_FILE" ]]; then
    UNEXPECTED+=("$hit")
  fi
done

if (( ${#UNEXPECTED[@]} > 0 )); then
  echo "[INFO] unexpected deprecated namespace hits:"
  printf '%s\n' "${UNEXPECTED[@]}"
  fail "deprecated ICertificateEx namespace must not appear outside bridge file"
fi

echo "[PASS] no deprecated namespace hits outside $EXEMPT_FILE"
