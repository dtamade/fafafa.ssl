#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

declare -a files=(
  "tests/test_connection_builder_hostname_precedence.pas"
  "tests/test_tls_connector_hostname_override_precedence.pas"
  "tests/test_freepascal_context_server_name_inheritance.pas"
  "tests/integration/test_cross_backend_consistency_contract.pas"
  "tests/integration/test_cross_backend_errors_contract.pas"
)

pattern='(Context|Ctx|LCtx|LContext)\.SetServerName\('
marker='INTENTIONAL_COMPAT: legacy context-level SNI coverage'

for file in "${files[@]}"; do
  if ! rg -n --quiet "$pattern" "$file"; then
    echo "[FAIL] expected intentional context-level SNI coverage was not found: $file"
    exit 1
  fi

  if ! rg -n --quiet "$marker" "$file"; then
    echo "[FAIL] missing compatibility label for intentional context-level SNI: $file"
    exit 1
  fi
done

echo "[PASS] intentional context-level SNI compatibility tests are explicitly labeled"
