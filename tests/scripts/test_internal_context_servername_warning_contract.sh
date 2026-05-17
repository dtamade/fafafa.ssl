#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

mkdir -p tmp/internal_context_servername_warning_contract

log_file="tmp/internal_context_servername_warning_contract/build.log"
binary="tmp/internal_context_servername_warning_contract/test_capabilities_contract"
winssl_file="src/fafafa.ssl.winssl.connection.pas"

fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/internal_context_servername_warning_contract \
  -FEtmp/internal_context_servername_warning_contract \
  -o"$binary" \
  tests/contract/test_capabilities_contract.pas >"$log_file" 2>&1

declare -a patterns=(
  'fafafa\.ssl\.wolfssl\.connection\.pas\([0-9]+,[0-9]+\) Warning: Symbol "ISSLContext\.GetServerName" is deprecated'
  'fafafa\.ssl\.mbedtls\.connection\.pas\([0-9]+,[0-9]+\) Warning: Symbol "ISSLContext\.GetServerName" is deprecated'
)

for pattern in "${patterns[@]}"; do
  if rg -n --pcre2 --quiet "$pattern" "$log_file"; then
    echo "[FAIL] internal context-level ServerName compatibility path still emits deprecated warning"
    rg -n --pcre2 "$pattern" "$log_file" || true
    exit 1
  fi
done

if [[ "$(rg -F -c '{$PUSH}{$WARN 6058 off}{$WARN SYMBOL_DEPRECATED OFF}' "$winssl_file")" -lt 2 ]]; then
  echo "[FAIL] winssl context-level ServerName compatibility path lacks local warning quarantine"
  exit 1
fi

"$binary" >/dev/null

echo "[PASS] internal context-level ServerName compatibility warnings are quarantined"
