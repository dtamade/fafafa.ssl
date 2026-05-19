#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

conn_base_file="src/fafafa.ssl.connection.base.pas"
backend_contract="tests/contract/test_backend_contract.pas"
mbedtls_contract="tests/test_mbedtls_connection_session_reused_contract.pas"
openssl_contract="tests/test_openssl_connection_session_reused_contract.pas"
winssl_mock_file="tests/winssl/test_session_save_logic.pas"

require_fixed() {
  local file="$1"
  local text="$2"
  local message="$3"
  if ! rg -F -n --quiet -- "$text" "$file"; then
    echo "[FAIL] $message"
    exit 1
  fi
}

forbid_fixed() {
  local file="$1"
  local text="$2"
  local message="$3"
  if rg -F -n --quiet -- "$text" "$file"; then
    echo "[FAIL] $message"
    exit 1
  fi
}

require_fixed "$conn_base_file" \
  'direct core session-resumption 当前只剩 contract mirror proof 和 backend-specific semantic truth proofs。' \
  "base connection residual note has not been tightened to semantic-truth classification"

require_fixed "$backend_contract" \
  'INTENTIONAL_CORE_SURFACE: keep these direct core session-resumption' \
  "backend contract lost the explicit compatibility-mirror marker"

require_fixed "$mbedtls_contract" \
  'INTENTIONAL_SESSION_REUSED_SEMANTIC_PROOF' \
  "mbedtls session-reused contract is not explicitly marked as semantic proof"
require_fixed "$mbedtls_contract" \
  'LConn.SetSession(LSession);' \
  "mbedtls semantic proof no longer exercises direct core SetSession"
require_fixed "$mbedtls_contract" \
  'not LConn.IsSessionReused' \
  "mbedtls semantic proof no longer exercises direct core IsSessionReused"

require_fixed "$openssl_contract" \
  'INTENTIONAL_SESSION_REUSED_SEMANTIC_PROOF' \
  "openssl session-reused contract is not explicitly marked as semantic proof"
require_fixed "$openssl_contract" \
  'LResult := LConn.IsSessionReused;' \
  "openssl semantic proof no longer exercises direct core IsSessionReused"

forbid_fixed "$winssl_mock_file" \
  'function GetSession: ISSLSession;' \
  "winssl mock save-logic helper still exposes a public-looking GetSession surface"
forbid_fixed "$winssl_mock_file" \
  'function TMockConnection.GetSession: ISSLSession;' \
  "winssl mock save-logic helper still implements GetSession"
forbid_fixed "$winssl_mock_file" \
  'LConn.GetSession' \
  "winssl mock save-logic helper still uses GetSession call sites"
require_fixed "$winssl_mock_file" \
  'GetSavedSession' \
  "winssl mock save-logic helper did not move to a non-public mock getter name"

actual_residuals="$(
  rg -lP '\b(?:Conn|LConn|LConn1|LConn2|ResumedConn|InitialConn|LTLSStream\.Connection)\.(?:GetSession|SetSession|IsSessionReused)\b' \
    tests --glob '!tests/scripts/**' | sort
)"

expected_residuals=$'tests/contract/test_backend_contract.pas\ntests/test_mbedtls_connection_session_reused_contract.pas\ntests/test_openssl_connection_session_reused_contract.pas'

if [[ "$actual_residuals" != "$expected_residuals" ]]; then
  echo "[FAIL] session-resumption residual file set drifted"
  echo "       expected:"
  printf '%s\n' "$expected_residuals"
  echo "       actual:"
  printf '%s\n' "$actual_residuals"
  exit 1
fi

echo "[PASS] session-resumption residual classification is frozen to intentional proof files"
