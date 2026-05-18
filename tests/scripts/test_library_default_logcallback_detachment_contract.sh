#!/usr/bin/env bash
set -euo pipefail

check_backend() {
  local file="$1"

  if rg -n --quiet 'FLogCallback := LConfig\.LogCallback;' "$file"; then
    echo "[FAIL] $file still lets SetDefaultConfig install the runtime log callback"
    exit 1
  fi

  if ! rg -n --quiet 'FLogCallback := ACallback;' "$file"; then
    echo "[FAIL] $file no longer updates the runtime callback through SetLogCallback"
    exit 1
  fi

  if ! rg -n --quiet 'FDefaultConfig\.LogCallback := ACallback;' "$file"; then
    echo "[FAIL] $file no longer mirrors SetLogCallback into GetDefaultConfig truth"
    exit 1
  fi
}

check_backend "src/fafafa.ssl.openssl.backed.pas"
check_backend "src/fafafa.ssl.freepascal.lib.pas"
check_backend "src/fafafa.ssl.winssl.lib.pas"
check_backend "src/fafafa.ssl.mbedtls.lib.pas"
check_backend "src/fafafa.ssl.wolfssl.lib.pas"

echo "[PASS] library-default log callback detachment remains aligned across backend library paths"
