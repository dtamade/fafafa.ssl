#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

mapfile -t HITS < <(
  rg -n '\bLoadOpenSSLLibrary\b|\bIsCryptoLibraryLoaded\b' tests examples --glob '*.pas' --glob '*.lpr' || true
)

ALLOWED=(
  # Intentional/temporary compatibility-only sites should be emptied by remaining batches.
)

if (( ${#HITS[@]} == 0 )); then
  echo '[PASS] no stale old-loader hits remain'
  exit 0
fi

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
    echo '[FAIL] stale old-loader hits remain outside allowlist'
    exit 1
  fi
done

echo '[PASS] old-loader hits are limited to the approved allowlist'
