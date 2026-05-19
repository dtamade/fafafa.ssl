#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$root_dir"

fail() {
  echo "[FAIL] $1" >&2
  exit 1
}

require_present() {
  local file="$1"
  local pattern="$2"
  local message="$3"
  if ! rg -F -n --quiet -- "$pattern" "$file"; then
    fail "$message"
  fi
}

require_absent() {
  local file="$1"
  local pattern="$2"
  local message="$3"
  if rg -F -n --quiet -- "$pattern" "$file"; then
    fail "$message"
  fi
}

base_file="src/fafafa.ssl.base.pas"
openssl_lib="src/fafafa.ssl.openssl.backed.pas"
openssl_ctx="src/fafafa.ssl.openssl.context.pas"
winssl_lib="src/fafafa.ssl.winssl.lib.pas"
winssl_ctx="src/fafafa.ssl.winssl.context.pas"
winssl_conn="src/fafafa.ssl.winssl.connection.pas"
freepascal_lib="src/fafafa.ssl.freepascal.lib.pas"
freepascal_ctx="src/fafafa.ssl.freepascal.context.pas"
freepascal_conn="src/fafafa.ssl.freepascal.connection.pas"
wolfssl_lib="src/fafafa.ssl.wolfssl.lib.pas"
wolfssl_ctx="src/fafafa.ssl.wolfssl.context.pas"
wolfssl_conn="src/fafafa.ssl.wolfssl.connection.pas"
mbedtls_lib="src/fafafa.ssl.mbedtls.lib.pas"
mbedtls_ctx="src/fafafa.ssl.mbedtls.context.pas"
mbedtls_conn="src/fafafa.ssl.mbedtls.connection.pas"

echo "[TEST] callback capability truth contract"

require_present "$base_file" "SupportsCallbacks: Boolean;" \
  "base capability record must continue to expose SupportsCallbacks"

require_present "$openssl_lib" "Result.SupportsCallbacks := True;" \
  "OpenSSL must continue to publish SupportsCallbacks"
require_present "$openssl_ctx" "SSL_CTX_set_cert_verify_callback(FSSLContext, @VerifyCertificateCallback, Self)" \
  "OpenSSL verify callback wiring must remain live"
require_present "$openssl_ctx" "SSL_CTX_set_default_passwd_cb(FSSLContext, @PasswordCallbackThunk);" \
  "OpenSSL password callback wiring must remain live"
require_present "$openssl_ctx" "SSL_CTX_set_info_callback(FSSLContext, @InfoCallbackThunk)" \
  "OpenSSL info callback wiring must remain live"

require_present "$winssl_lib" "Result.SupportsCallbacks := True;" \
  "WinSSL capability truth must publish SupportsCallbacks while runtime callback wiring exists"
require_present "$winssl_ctx" "function TWinSSLContext.GetWinSSLVerifyCallback: TSSLVerifyCallback;" \
  "WinSSL context must still expose verify callback accessors"
require_present "$winssl_ctx" "function TWinSSLContext.GetWinSSLInfoCallback: TSSLInfoCallback;" \
  "WinSSL context must still expose info callback accessors"
require_present "$winssl_conn" "LCallback := LContextAccess.GetWinSSLInfoCallback;" \
  "WinSSL connection must still consume the published info callback runtime path"
require_present "$winssl_conn" "LVerifyCallback := LContextAccess.GetWinSSLVerifyCallback;" \
  "WinSSL connection must still consume the published verify callback runtime path"

require_present "$freepascal_lib" "Result.SupportsCallbacks := False;" \
  "FreePascal must not publish SupportsCallbacks until callback runtime wiring exists"
require_absent "$freepascal_conn" "VerifyCallback" \
  "FreePascal connection runtime must stay callback-free while SupportsCallbacks is false"
require_absent "$freepascal_conn" "InfoCallback" \
  "FreePascal connection runtime must stay info-callback-free while SupportsCallbacks is false"
require_absent "$freepascal_conn" "PasswordCallback" \
  "FreePascal connection runtime must stay password-callback-free while SupportsCallbacks is false"
require_present "$freepascal_ctx" "procedure TFreePascalContext.SetVerifyCallback(ACallback: TSSLVerifyCallback);" \
  "FreePascal setter surface must remain present for interface compatibility"

require_present "$wolfssl_lib" "Result.SupportsCallbacks := False;" \
  "WolfSSL must not publish SupportsCallbacks before callback runtime wiring exists"
require_absent "$wolfssl_conn" "VerifyCallback" \
  "WolfSSL connection runtime must stay callback-free while SupportsCallbacks is false"
require_absent "$wolfssl_conn" "InfoCallback" \
  "WolfSSL connection runtime must stay info-callback-free while SupportsCallbacks is false"
require_absent "$wolfssl_conn" "PasswordCallback" \
  "WolfSSL connection runtime must stay password-callback-free while SupportsCallbacks is false"
require_present "$wolfssl_ctx" "procedure TWolfSSLContext.SetVerifyCallback(ACallback: TSSLVerifyCallback);" \
  "WolfSSL setter surface must remain present for interface compatibility"

require_present "$mbedtls_lib" "Result.SupportsCallbacks := False;" \
  "MbedTLS must not publish SupportsCallbacks before callback runtime wiring exists"
require_absent "$mbedtls_conn" "VerifyCallback" \
  "MbedTLS connection runtime must stay callback-free while SupportsCallbacks is false"
require_absent "$mbedtls_conn" "InfoCallback" \
  "MbedTLS connection runtime must stay info-callback-free while SupportsCallbacks is false"
require_absent "$mbedtls_conn" "PasswordCallback" \
  "MbedTLS connection runtime must stay password-callback-free while SupportsCallbacks is false"
require_present "$mbedtls_ctx" "procedure TMbedTLSContext.SetVerifyCallback(ACallback: TSSLVerifyCallback);" \
  "MbedTLS setter surface must remain present for interface compatibility"

echo "[PASS] callback capability truth remains aligned with runtime/source classification"
