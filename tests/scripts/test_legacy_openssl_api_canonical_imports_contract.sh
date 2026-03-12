#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] legacy openssl api canonical imports contract"

pattern='fafafa\.ssl\.openssl\.(aes|aria|bio|blake2|bn|buffer|chacha|cmac|cms|comp|conf|consts|core|ct|des|dso|ec|ecdh|ecdsa|engine|err|evp|kdf|legacy_ciphers|lhash|modes|obj|ocsp|param|pem|pkcs|pkcs12|pkcs7|provider|rand|rsa|seed|sha3|sm|srp|ssl|stack|store|thread|ts|txt_db|types|ui|x509)\b'

if rg -n "$pattern" "$ROOT_DIR/src" "$ROOT_DIR/tests" "$ROOT_DIR/examples" \
  --glob '*.pas' --glob '*.lpr' --glob '!src/fafafa.ssl.openssl.*.pas' | sed -n '1,60p'; then
  fail "active Pascal source should prefer canonical fafafa.ssl.openssl.api.* imports"
fi

echo "[PASS] legacy openssl api canonical imports contract passed"
