#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

FILES=(
  examples/pkcs7_encrypt_decrypt_example.pas
  examples/pkcs7_data_example.pas
  examples/example_crypto_simple.pas
  examples/hello_ssl.pas
  examples/test_openssl_rsa.lpr
  examples/pkcs7_sign_verify_example.pas
  examples/test_openssl_chacha.lpr
  examples/pkcs7_basic_example.pas
  tests/performance/test_hash_extended_perf.pas
  tests/pkcs11/test_pkcs11_softhsm.pas
  tests/crypto/test_modes_basic.pas
  tests/benchmarks/benchmark_aesgcm_pool.pas
  tests/diagnostic/test_logging.pas
)

if rg -n '\bGetOpenSSLVersion\b' "${FILES[@]}"; then
  echo '[FAIL] string-based version output sites should use GetOpenSSLVersionString'
  exit 1
fi

for f in "${FILES[@]}"; do
  rg -F --quiet -- 'GetOpenSSLVersionString' "$f" || {
    echo "[FAIL] missing GetOpenSSLVersionString in $f"
    exit 1
  }
done

echo '[PASS] string-based version output sites use GetOpenSSLVersionString'
