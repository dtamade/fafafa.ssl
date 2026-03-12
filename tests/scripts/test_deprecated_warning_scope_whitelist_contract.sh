#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
PATTERN='{$WARN SYMBOL_DEPRECATED OFF}'
SELF_FILE="tests/scripts/test_deprecated_warning_scope_whitelist_contract.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

declare -A EXPECTED_COUNTS=(
  ["src/fafafa.ssl.connection.base.pas"]=1
  ["src/fafafa.ssl.context.builder.pas"]=1
  ["src/fafafa.ssl.openssl.cert.builder.pas"]=1
  ["src/fafafa.ssl.factory.pas"]=1
  ["src/fafafa.ssl.pas"]=1
  ["tests/config/test_config_validation.pas"]=1
  ["tests/examples/test_basic.pas"]=6
  ["tests/test_connection_builder_hostname_override_precedence.pas"]=4
  ["tests/test_connection_context_server_name_inheritance.pas"]=1
  ["tests/test_connection_server_mode_sni_isolation.pas"]=2
  ["tests/test_freepascal_connection_server_name_observability.pas"]=1
  ["tests/test_library_create_context_default_config_consistency.pas"]=1
  ["tests/test_mbedtls_connection_server_name_observability.pas"]=1
  ["tests/test_openssl_connection_server_name_clear_override.pas"]=1
  ["tests/test_tls_connector_hostname_override_precedence.pas"]=3
  ["tests/test_wolfssl_connection_server_name_observability.pas"]=1
  ["tests/test_wolfssl_standalone_connection_server_name_compatibility.pas"]=1
)

declare -A ACTUAL_COUNTS=()

echo "[TEST] deprecated warning scope whitelist contract"

mapfile -t HITS < <(
  cd "$ROOT_DIR"
  rg -n -F --no-heading --color never "$PATTERN" src tests || true
)

for hit in "${HITS[@]}"; do
  file="${hit%%:*}"
  if [[ "$file" == "$SELF_FILE" ]]; then
    continue
  fi
  ACTUAL_COUNTS["$file"]=$(( ${ACTUAL_COUNTS["$file"]:-0} + 1 ))
done

UNEXPECTED=()
for file in "${!ACTUAL_COUNTS[@]}"; do
  if [[ -z "${EXPECTED_COUNTS[$file]+x}" ]]; then
    UNEXPECTED+=("$file:${ACTUAL_COUNTS[$file]}")
  fi
done

if (( ${#UNEXPECTED[@]} > 0 )); then
  echo "[INFO] unexpected files containing deprecated warning OFF directives:"
  printf '%s\n' "${UNEXPECTED[@]}"
  fail "deprecated warning OFF scope leaked outside approved whitelist"
fi

MISMATCH=()
for file in "${!EXPECTED_COUNTS[@]}"; do
  expected="${EXPECTED_COUNTS[$file]}"
  actual="${ACTUAL_COUNTS[$file]:-0}"
  if [[ "$expected" != "$actual" ]]; then
    MISMATCH+=("$file expected=$expected actual=$actual")
  fi
done

if (( ${#MISMATCH[@]} > 0 )); then
  echo "[INFO] whitelist count mismatch:"
  printf '%s\n' "${MISMATCH[@]}"
  fail "deprecated warning suppression count drift detected"
fi

echo "[PASS] deprecated warning suppression stays within approved whitelist"
