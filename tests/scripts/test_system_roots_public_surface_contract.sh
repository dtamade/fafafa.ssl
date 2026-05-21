#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

base_file="src/fafafa.ssl.base.pas"
factory_file="src/fafafa.ssl.factory.pas"
debug_file="src/fafafa.ssl.debug.utils.pas"
api_ref="docs/reference/API_REFERENCE.md"
arch_ref="docs/reference/ARCHITECTURE.md"
ca_doc="docs/CA_CERTIFICATE_AUTO_LOADING.md"
getting_started="docs/guides/GETTING_STARTED.md"

require_fixed() {
  local file="$1"
  local pattern="$2"
  local message="$3"

  if ! rg -F -n --quiet -- "$pattern" "$file"; then
    echo "[FAIL] $message"
    exit 1
  fi
}

require_rg() {
  local file="$1"
  local pattern="$2"
  local message="$3"

  if ! rg -n --quiet -- "$pattern" "$file"; then
    echo "[FAIL] $message"
    exit 1
  fi
}

require_rg "$base_file" \
  'UseSystemRoots: Boolean;[[:space:]]+// Context-scoped system trust-store opt-in; loads platform roots via ISSLCertificateStore' \
  "TSSLConfig no longer exposes UseSystemRoots as a context-scoped trust-store opt-in"

require_fixed "$factory_file" \
  "AConfig.UseSystemRoots)) then" \
  "factory server verify baseline no longer treats UseSystemRoots as a trust-root source"
require_fixed "$factory_file" \
  "procedure ApplySystemRootsIfRequested" \
  "factory context paths no longer gate system-root loading on UseSystemRoots"
require_fixed "$factory_file" \
  "LStore := TSSLFactory.CreateCertificateStore(LLib.GetLibraryType);" \
  "factory context paths no longer create certificate stores from the resolved backend"
require_fixed "$factory_file" \
  "LStore.LoadSystemStore;" \
  "factory context paths no longer load system roots into the created store"
require_fixed "$factory_file" \
  "AContext.SetCertificateStore(LStore);" \
  "factory context paths no longer inject the loaded system-root store into the context"

for file in \
  src/fafafa.ssl.openssl.backed.pas \
  src/fafafa.ssl.freepascal.lib.pas \
  src/fafafa.ssl.mbedtls.lib.pas \
  src/fafafa.ssl.wolfssl.lib.pas \
  src/fafafa.ssl.winssl.lib.pas
do
  require_fixed "$file" \
    "if LConfig.UseSystemRoots then" \
    "$file no longer applies UseSystemRoots on the direct-library path"
  require_fixed "$file" \
    "Store := TSSLFactory.CreateCertificateStore(GetLibraryType);" \
    "$file no longer creates a backend-matching certificate store for UseSystemRoots"
  require_fixed "$file" \
    "Store.LoadSystemStore;" \
    "$file no longer loads system roots before injecting the store"
  require_fixed "$file" \
    "Result.SetCertificateStore(Store);" \
    "$file no longer injects the system-root store into the direct-library context"
done

require_fixed "$debug_file" \
  "系统根证书: %s (context-scoped trust-store opt-in；factory/direct-library path 会在创建时加载)" \
  "DumpSSLConfig no longer reports UseSystemRoots"

require_fixed "$api_ref" \
  '`UseSystemRoots`' \
  "API reference no longer mentions UseSystemRoots in the active public truth"
require_fixed "$arch_ref" \
  '`UseSystemRoots`' \
  "ARCHITECTURE no longer records UseSystemRoots inside the context-scoped config bucket"
require_fixed "$ca_doc" \
  '`TSSLConfig.UseSystemRoots`' \
  "CA auto-loading doc no longer documents the config/direct-library system-roots opt-in"
require_fixed "$getting_started" \
  '`TSSLConfig.UseSystemRoots`' \
  "GETTING_STARTED no longer points to TSSLConfig.UseSystemRoots for factory/direct-library users"

echo "[PASS] system-roots public surface parity stays aligned across source and docs"
