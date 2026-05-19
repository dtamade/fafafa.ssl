#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$repo_root"

pass() {
  printf '[PASS] %s\n' "$1"
}

fail() {
  printf '[FAIL] %s\n' "$1"
  if [[ $# -ge 2 ]]; then
    printf '       %s\n' "$2"
  fi
  exit 1
}

require_match() {
  local file="$1"
  local pattern="$2"
  local name="$3"
  if rg -n --multiline --multiline-dotall "$pattern" "$file" >/dev/null; then
    pass "$name"
  else
    fail "$name" "pattern not found in $file: $pattern"
  fi
}

openssl_context="src/fafafa.ssl.openssl.context.pas"
openssl_lib="src/fafafa.ssl.openssl.backed.pas"
openssl_connection="src/fafafa.ssl.openssl.connection.pas"
wolfssl_context="src/fafafa.ssl.wolfssl.context.pas"
wolfssl_lib="src/fafafa.ssl.wolfssl.lib.pas"

printf '[TEST] optional interface capability alignment contract\n'

require_match "$openssl_context" \
  'TOpenSSLContext = class\(TInterfacedObject, ISSLContext, ISSLNativeHandleAccess,\s*ISSLHttpHooksAccess\)' \
  'OpenSSL base context no longer implements optional early-data or server-OCSP interfaces unconditionally'
require_match "$openssl_context" \
  'TOpenSSLEarlyDataContext = class\(TOpenSSLContext, ISSLEarlyDataContext\)' \
  'OpenSSL declares a dedicated early-data context subclass'
require_match "$openssl_context" \
  'TOpenSSLServerOCSPContext = class\(TOpenSSLContext, ISSLServerOCSPStaplingContext\)' \
  'OpenSSL declares a dedicated server-OCSP context subclass'
require_match "$openssl_context" \
  'TOpenSSLAdvancedContext = class\(TOpenSSLContext,\s*ISSLEarlyDataContext, ISSLServerOCSPStaplingContext\)' \
  'OpenSSL declares a combined optional-interface context subclass when both capabilities are present'
require_match "$openssl_lib" \
  'LExposeEarlyData := GetCapabilities\.EarlyDataSupport <> sslSupportNone;.*?LExposeServerOCSP := \(AType in \[sslCtxServer, sslCtxBoth\]\) and\s*\(GetCapabilities\.OCSPStaplingSupport <> sslSupportNone\);.*?TOpenSSLAdvancedContext\.Create.*?TOpenSSLEarlyDataContext\.Create.*?TOpenSSLServerOCSPContext\.Create.*?TOpenSSLContext\.Create' \
  'OpenSSL library create-context path selects the optional-interface subclass that matches current capability truth'

require_match "$openssl_connection" \
  'TOpenSSLConnection = class\(TBaseSSLConnection, ISSLClientConnection,\s*ISSLOCSPStapling, ISSLNativeHandleAccess\)' \
  'OpenSSL base connection no longer implements early-data connection unconditionally'
require_match "$openssl_connection" \
  'TOpenSSLEarlyDataConnection = class\(TOpenSSLConnection, ISSLEarlyDataConnection\)' \
  'OpenSSL declares a dedicated early-data connection subclass'
require_match "$openssl_context" \
  'function TOpenSSLContext\.CreateConnection\(ASocket: THandle\): ISSLConnection;.*?Supports\(Self, ISSLEarlyDataContext, LEarlyDataContext\).*?TOpenSSLEarlyDataConnection\.Create\(Self, ASocket\).*?TOpenSSLConnection\.Create\(Self, ASocket\)' \
  'OpenSSL socket connection path only creates an early-data connection subclass when the parent context still exposes early-data capability'
require_match "$openssl_context" \
  'function TOpenSSLContext\.CreateConnection\(AStream: TStream\): ISSLConnection;.*?Supports\(Self, ISSLEarlyDataContext, LEarlyDataContext\).*?TOpenSSLEarlyDataConnection\.Create\(Self, AStream\).*?TOpenSSLConnection\.Create\(Self, AStream\)' \
  'OpenSSL stream connection path only creates an early-data connection subclass when the parent context still exposes early-data capability'

require_match "$wolfssl_context" \
  'TWolfSSLContext = class\(TInterfacedObject, ISSLContext, ISSLNativeHandleAccess\)' \
  'WolfSSL base context no longer implements server OCSP stapling unconditionally'
require_match "$wolfssl_context" \
  'TWolfSSLOCSPStaplingContext = class\(TWolfSSLContext, ISSLServerOCSPStaplingContext\)' \
  'WolfSSL declares a dedicated server-OCSP context subclass'
require_match "$wolfssl_context" \
  'TWolfSSLAdvancedContext = class\(TWolfSSLContext,\s*ISSLEarlyDataContext, ISSLServerOCSPStaplingContext\)' \
  'WolfSSL declares a combined optional-interface context subclass when both capabilities are present'
require_match "$wolfssl_lib" \
  'LExposeEarlyData := GetCapabilities\.EarlyDataSupport <> sslSupportNone;.*?LExposeServerOCSP := \(AType in \[sslCtxServer, sslCtxBoth\]\) and\s*\(GetCapabilities\.OCSPStaplingSupport <> sslSupportNone\);.*?TWolfSSLAdvancedContext\.Create.*?TWolfSSLEarlyDataContext\.Create.*?TWolfSSLOCSPStaplingContext\.Create.*?TWolfSSLContext\.Create' \
  'WolfSSL library create-context path selects the optional-interface subclass that matches current capability truth'

printf '[PASS] optional interface capability alignment contract passed\n'
