#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] pure Pascal ALPN client-no-offer contract"

[[ -f tests/scripts/test_freepascal_tls13_builder_connector_stream_client_no_alpn_offer_contract.sh ]] || \
  fail "TLS1.3 client-no-offer ALPN contract should exist"

[[ -f tests/scripts/test_freepascal_tls12_local_client_no_alpn_offer_contract.sh ]] || \
  fail "TLS1.2 client-no-offer ALPN contract should exist"

echo "[PASS] pure Pascal ALPN client-no-offer contract stays enforced"
