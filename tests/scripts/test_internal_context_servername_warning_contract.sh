#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

mkdir -p tmp/internal_context_servername_warning_contract

log_file="tmp/internal_context_servername_warning_contract/build.log"

fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/internal_context_servername_warning_contract \
  -FEtmp/internal_context_servername_warning_contract \
  -otmp/internal_context_servername_warning_contract/test_builder_integration \
  tests/test_builder_integration.pas >"$log_file" 2>&1

declare -a patterns=(
  'fafafa\.ssl\.factory\.pas\([0-9]+,[0-9]+\) Warning: Symbol "ISSLContext\.SetServerName" is deprecated'
  'fafafa\.ssl\.context\.builder\.pas\([0-9]+,[0-9]+\) Warning: Symbol "ISSLContext\.SetServerName" is deprecated'
  'fafafa\.ssl\.openssl\.connection\.pas\([0-9]+,[0-9]+\) Warning: Symbol "ISSLContext\.GetServerName" is deprecated'
  'fafafa\.ssl\.openssl\.backed\.pas\([0-9]+,[0-9]+\) Warning: Symbol "ISSLContext\.(GetServerName|SetServerName)" is deprecated'
)

for pattern in "${patterns[@]}"; do
  if rg -n --pcre2 --quiet "$pattern" "$log_file"; then
    echo "[FAIL] internal context-level ServerName compatibility path still emits deprecated warning"
    rg -n --pcre2 "$pattern" "$log_file" || true
    exit 1
  fi
done

echo "[PASS] internal context-level ServerName compatibility warnings are quarantined"
