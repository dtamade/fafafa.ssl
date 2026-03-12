#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] active program runtime contract batch"

SCRIPTS=(
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

for script in "${SCRIPTS[@]}"; do
  if [[ ! -f "$ROOT_DIR/$script" ]]; then
    fail "missing runtime contract script: $script"
  fi

  if ! bash "$ROOT_DIR/$script"; then
    fail "runtime contract failed: $script"
  fi
done

echo "[PASS] active program runtime contract batch passed"
