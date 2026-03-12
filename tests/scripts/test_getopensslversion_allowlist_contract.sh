#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

mapfile -t HITS < <(
  rg -n '\bGetOpenSSLVersion\b' examples tests --glob '*.pas' --glob '*.lpr' || true
)

if (( ${#HITS[@]} == 0 )); then
  echo '[FAIL] expected a small explicit allowlist of GetOpenSSLVersion uses, found none'
  exit 1
fi

ALLOWED=(
  'examples/test_version_detection.lpr'
  'tests/openssl/test_openssl_1_1_compatibility.pas'
  'tests/openssl/test_openssl_load.pas'
)

for hit in "${HITS[@]}"; do
  allowed=false
  for path in "${ALLOWED[@]}"; do
    if [[ "$hit" == "$path"* ]]; then
      allowed=true
      break
    fi
  done
  if [[ "$allowed" == false ]]; then
    printf '%s\n' "${HITS[@]}"
    echo '[FAIL] unexpected GetOpenSSLVersion use outside allowlist'
    exit 1
  fi
done

echo '[PASS] GetOpenSSLVersion uses are limited to the approved allowlist'
