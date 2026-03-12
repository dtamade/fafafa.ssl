#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
BATCH_SCRIPT="$ROOT_DIR/tests/scripts/test_active_program_runtime_contract_batch.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] active program runtime contract batch coverage contract"

[[ -f "$BATCH_SCRIPT" ]] || fail "missing batch script: tests/scripts/test_active_program_runtime_contract_batch.sh"

REQUIRED_SCRIPTS=(
  "tests/scripts/test_core_openssl_validation_runtime_contract.sh"
  "tests/scripts/test_module_headers_quick_runtime_contract.sh"
  "tests/scripts/test_quick_module_validation_runtime_contract.sh"
  "tests/scripts/test_backend_and_algorithms_runtime_contract.sh"
  "tests/scripts/test_algorithm_availability_runtime_contract.sh"
  "tests/scripts/test_crypto_basics_runtime_contract.sh"
  "tests/scripts/test_crypto_family_a_runtime_contract.sh"
  "tests/scripts/test_crypto_family_b_runtime_contract.sh"
  "tests/scripts/test_benchmark_crypto_runtime_contract.sh"
  "tests/scripts/test_cert_and_diag_runtime_contract.sh"
  "tests/scripts/test_certificate_smoke_runtime_contract.sh"
  "tests/scripts/test_certificate_utilities_runtime_contract.sh"
  "tests/scripts/test_certificate_p2_core_runtime_contract.sh"
  "tests/scripts/test_certificate_p2_pkcs_runtime_contract.sh"
  "tests/scripts/test_cert_load_debug_contract.sh"
  "tests/scripts/test_ocsp_simple_runtime_contract.sh"
  "tests/scripts/test_integration_runtime_contract.sh"
  "tests/scripts/test_integration_simple_runtime_contract.sh"
  "tests/scripts/test_integration_pkcs11_runtime_contract.sh"
  "tests/scripts/test_openssl_load_program_runtime_contract.sh"
  "tests/scripts/test_active_validation_version_string_runtime_contract.sh"
  "tests/scripts/test_self_contained_examples_runtime_contract.sh"
  "tests/scripts/test_pkcs7_examples_runtime_contract.sh"
  "tests/scripts/test_tool_examples_runtime_contract.sh"
)

mapfile -t BATCH_SCRIPTS < <(
  awk '
    /SCRIPTS=\(/ {inlist=1; next}
    inlist && /^[[:space:]]*\)/ {inlist=0; exit}
    inlist {
      line=$0
      gsub(/^[[:space:]]*"/, "", line)
      gsub(/"[[:space:]]*$/, "", line)
      if (length(line) > 0) print line
    }
  ' "$BATCH_SCRIPT"
)

[[ "${#BATCH_SCRIPTS[@]}" -gt 0 ]] || fail "unable to parse SCRIPTS list from batch script"

DUPLICATES="$(printf '%s\n' "${BATCH_SCRIPTS[@]}" | sort | uniq -d || true)"
if [[ -n "$DUPLICATES" ]]; then
  echo "$DUPLICATES"
  fail "batch script should not contain duplicate runtime contract entries"
fi

for required in "${REQUIRED_SCRIPTS[@]}"; do
  if ! printf '%s\n' "${BATCH_SCRIPTS[@]}" | rg -F --quiet -- "$required"; then
    fail "batch script missing required runtime contract: $required"
  fi
done

for listed in "${BATCH_SCRIPTS[@]}"; do
  if [[ ! -f "$ROOT_DIR/$listed" ]]; then
    fail "batch script references missing file: $listed"
  fi
done

echo "[PASS] active program runtime contract batch coverage is complete"
