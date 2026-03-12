#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

compile_and_run() {
  local source_file="$1"
  local output_file="$2"
  local pass_token="$3"
  local log_file="$4"

  if ! fpc -Fu./src "$source_file" -o"$output_file" >/tmp/crypto-family-b.compile.log 2>&1; then
    echo "[INFO] compile output for $source_file:"
    sed -n '1,260p' /tmp/crypto-family-b.compile.log || true
    fail "$source_file should compile"
  fi

  if ! "./$output_file" >"$log_file" 2>&1; then
    echo "[INFO] runtime output for $source_file:"
    sed -n '1,260p' "$log_file" || true
    fail "$source_file should run successfully"
  fi

  if ! rg -F --quiet -- "$pass_token" "$log_file"; then
    echo "[INFO] runtime output for $source_file:"
    sed -n '1,260p' "$log_file" || true
    fail "$source_file should print success marker"
  fi
}

compile_and_run tests/crypto/test_aead_comprehensive.pas tmp/test_aead_comprehensive_contract '[PASS] aead comprehensive validation completed' /tmp/test_aead_comprehensive_contract.log
compile_and_run tests/crypto/test_hmac_comprehensive.pas tmp/test_hmac_comprehensive_contract '[PASS] hmac comprehensive validation completed' /tmp/test_hmac_comprehensive_contract.log
compile_and_run tests/crypto/test_hash_comprehensive.pas tmp/test_hash_comprehensive_contract '[PASS] hash comprehensive validation completed' /tmp/test_hash_comprehensive_contract.log
compile_and_run tests/crypto/test_signature_comprehensive.pas tmp/test_signature_comprehensive_contract '[PASS] signature comprehensive validation completed' /tmp/test_signature_comprehensive_contract.log
compile_and_run tests/crypto/test_kdf_comprehensive.pas tmp/test_kdf_comprehensive_contract '[PASS] kdf comprehensive validation completed' /tmp/test_kdf_comprehensive_contract.log
compile_and_run tests/crypto/test_evp_simple.pas tmp/test_evp_simple_contract '[PASS] evp simple validation completed' /tmp/test_evp_simple_contract.log
compile_and_run tests/crypto/test_evp_digest.pas tmp/test_evp_digest_contract '[PASS] evp digest validation completed' /tmp/test_evp_digest_contract.log
compile_and_run tests/crypto/test_evp_cipher.pas tmp/test_evp_cipher_contract '[PASS] evp cipher validation completed' /tmp/test_evp_cipher_contract.log
compile_and_run tests/crypto/test_evp_aead.pas tmp/test_evp_aead_contract '[PASS] evp aead validation completed' /tmp/test_evp_aead_contract.log
compile_and_run tests/crypto/test_evp_aead_tag_fail.pas tmp/test_evp_aead_tag_fail_contract '[PASS] evp aead tag-fail validation completed' /tmp/test_evp_aead_tag_fail_contract.log
compile_and_run tests/crypto/test_phase2_aead_verification.pas tmp/test_phase2_aead_verification_contract '[PASS] phase2 aead verification completed' /tmp/test_phase2_aead_verification_contract.log

echo '[PASS] crypto family B programs stay green at runtime'
