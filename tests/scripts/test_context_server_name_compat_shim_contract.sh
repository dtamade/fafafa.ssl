#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

compat_file="src/fafafa.ssl.context.compat.pas"

if ! grep -n -q 'function GetContextLevelServerNameCompatibilityValue' "$compat_file"; then
  echo "[FAIL] missing shared context ServerName compatibility helper in $compat_file"
  exit 1
fi

declare -a backend_files=(
  "src/fafafa.ssl.openssl.connection.pas"
  "src/fafafa.ssl.wolfssl.connection.pas"
  "src/fafafa.ssl.mbedtls.connection.pas"
  "src/fafafa.ssl.winssl.connection.pas"
)

declare -a no_helper_backend_files=(
  "src/fafafa.ssl.freepascal.connection.pas"
)

if grep -n -E -q '(AContext|FContext)\.GetServerName' "$compat_file"; then
  echo "[FAIL] shared compatibility helper should not read deprecated context-level ServerName directly anymore: $compat_file"
  grep -n -E '(AContext|FContext)\.GetServerName' "$compat_file" || true
  exit 1
fi

for file in "${backend_files[@]}"; do
  if ! grep -n -q 'GetContextLevelServerNameCompatibilityValue(' "$file"; then
    echo "[FAIL] backend does not use shared context ServerName compatibility helper: $file"
    exit 1
  fi
done

for file in "${no_helper_backend_files[@]}"; do
  if grep -n -q 'GetContextLevelServerNameCompatibilityValue(' "$file"; then
    echo "[FAIL] backend should no longer use shared context ServerName compatibility helper: $file"
    grep -n 'GetContextLevelServerNameCompatibilityValue(' "$file" || true
    exit 1
  fi
done

backend_files+=("${no_helper_backend_files[@]}")

for file in "${backend_files[@]}"; do
  if grep -n -E -q '(AContext|FContext)\.GetServerName' "$file"; then
    echo "[FAIL] backend still performs direct context-level ServerName fallback read: $file"
    grep -n -E '(AContext|FContext)\.GetServerName' "$file" || true
    exit 1
  fi
done

echo "[PASS] backend context ServerName compatibility seam matches current no-inheritance truth"
