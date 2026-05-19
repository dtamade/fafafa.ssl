#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$root_dir"

fail() {
  echo "[FAIL] $1" >&2
  exit 1
}

require_fixed() {
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

user_guide="docs/guides/USER_GUIDE.md"
integration_guide="docs/INTEGRATION_GUIDE.md"
winssl_quickstart="docs/guides/WINSSL_QUICKSTART.md"
winssl_guide="docs/guides/WINSSL_USER_GUIDE.md"
mbedtls_guide="docs/guides/MBEDTLS_USER_GUIDE.md"
troubleshooting="docs/guides/TROUBLESHOOTING.md"
api_reference="docs/reference/API_REFERENCE.md"

echo "[TEST] public unit/import guidance truth contract"

require_fixed "$user_guide" "SysUtils, fafafa.ssl;" \
  "USER_GUIDE must use the current public facade unit in active examples"
require_fixed "$user_guide" "LLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);" \
  "USER_GUIDE must use TSSLFactory.GetLibraryInstance for OpenSSL-focused examples"

require_fixed "$integration_guide" "fafafa.ssl.context.builder;" \
  "INTEGRATION_GUIDE must keep the current builder unit in hook/client setup examples"
require_fixed "$integration_guide" "  fafafa.ssl;" \
  "INTEGRATION_GUIDE must use the current public facade unit in active examples"

require_fixed "$winssl_quickstart" "fafafa.ssl;" \
  "WINSSL_QUICKSTART must use the current public facade unit"
require_fixed "$winssl_quickstart" "Lib := TSSLFactory.GetLibraryInstance(sslWinSSL);" \
  "WINSSL_QUICKSTART must use the current WinSSL library entrypoint"
require_fixed "$winssl_quickstart" "Ctx := Lib.CreateContext(sslCtxClient);" \
  "WINSSL_QUICKSTART must use the current context enum name"
require_fixed "$winssl_quickstart" "WriteLn('Using backend: ', LibraryTypeToString(LLib.GetLibraryType));" \
  "WINSSL_QUICKSTART must use current library-type reporting instead of stale GetLibraryName"

require_fixed "$winssl_guide" "fafafa.ssl;" \
  "WINSSL_USER_GUIDE must use the current public facade unit in examples"
require_fixed "$winssl_guide" "Lib := TSSLFactory.GetLibraryInstance(sslWinSSL);" \
  "WINSSL_USER_GUIDE must use the current WinSSL library entrypoint"

require_fixed "$mbedtls_guide" "Lib := TSSLFactory.GetLibraryInstance(sslMbedTLS);" \
  "MBEDTLS_USER_GUIDE must use the current MbedTLS library entrypoint"
require_fixed "$mbedtls_guide" "fafafa.ssl;" \
  "MBEDTLS_USER_GUIDE must use the current public facade unit"

require_fixed "$troubleshooting" "if not TSSLFactory.IsLibraryAvailable(sslOpenSSL) then" \
  "TROUBLESHOOTING must use current factory availability checks instead of manual OpenSSL loader guidance"
require_fixed "$troubleshooting" "LLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);" \
  "TROUBLESHOOTING must use current OpenSSL library entrypoint"

require_fixed "$api_reference" "TSSLFactory.GetLibraryInstance(ALibType: TSSLLibraryType = sslAutoDetect): ISSLLibrary;" \
  "API_REFERENCE must publish the current public library-entrypoint truth"
require_fixed "$api_reference" "backend-specific low-level creators" \
  "API_REFERENCE must classify backend-specific creators as low-level entrypoints"
require_fixed "$api_reference" "LLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);" \
  "API_REFERENCE examples must use the current public library-entrypoint truth"

for file in \
  "$integration_guide" \
  "$user_guide" \
  "$winssl_quickstart" \
  "$winssl_guide" \
  "$mbedtls_guide" \
  "$troubleshooting" \
  "$api_reference"; do
  require_absent "$file" "fafafa.ssl.abstract.intf" \
    "$file must stop using removed abstract.intf"
  require_absent "$file" "fafafa.ssl.abstract.types" \
    "$file must stop using removed abstract.types"
done

require_absent "$integration_guide" "fafafa.ssl.base," \
  "INTEGRATION_GUIDE must stop teaching direct base-unit imports in active examples"
require_absent "$integration_guide" "fafafa.ssl.tls;" \
  "INTEGRATION_GUIDE must stop teaching direct tls-unit imports in active examples"
require_absent "$user_guide" "fafafa.ssl.openssl" \
  "USER_GUIDE must stop teaching nonexistent fafafa.ssl.openssl facade unit"
require_absent "$troubleshooting" "fafafa.ssl.openssl;" \
  "TROUBLESHOOTING must stop recommending nonexistent fafafa.ssl.openssl facade unit"
require_absent "$api_reference" "fafafa.ssl.openssl," \
  "API_REFERENCE examples must stop using nonexistent fafafa.ssl.openssl facade unit"

for file in \
  "$integration_guide" \
  "$winssl_quickstart" \
  "$winssl_guide" \
  "$mbedtls_guide" \
  "$api_reference"; do
  require_absent "$file" "CreateSSLLibrary(" \
    "$file must stop teaching nonexistent CreateSSLLibrary(...)"
done

for file in \
  "$winssl_quickstart" \
  "$winssl_guide"; do
  require_absent "$file" "sslLibraryWinSSL" \
    "$file must stop using stale sslLibraryWinSSL enum name"
  require_absent "$file" "sslLibraryOpenSSL" \
    "$file must stop using stale sslLibraryOpenSSL enum name"
  require_absent "$file" "sslLibraryAutoDetect" \
    "$file must stop using stale sslLibraryAutoDetect enum name"
done

require_absent "$winssl_quickstart" "sslContextClient" \
  "WINSSL_QUICKSTART must stop using stale sslContextClient enum name"
require_absent "$winssl_quickstart" "GetLibraryName" \
  "WINSSL_QUICKSTART must stop using nonexistent GetLibraryName"

for file in \
  "$troubleshooting" \
  "$api_reference"; do
  require_absent "$file" "LoadOpenSSL" \
    "$file must stop teaching manual LoadOpenSSL as a high-entry public step"
done

echo "[PASS] public unit/import guidance truth contract passed"
