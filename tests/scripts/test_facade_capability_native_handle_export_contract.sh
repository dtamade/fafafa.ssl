#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$repo_root"

fail() {
  printf '[FAIL] %s\n' "$1" >&2
  exit 1
}

require_fixed() {
  local file="$1"
  local expected="$2"
  local name="$3"
  if ! grep -Fq -- "$expected" "$file"; then
    fail "$name"
  fi
}

facade="src/fafafa.ssl.pas"
api_ref="docs/reference/API_REFERENCE.md"
contract_src="tests/contract/test_facade_capability_native_handle_entry.pas"
build_root="tmp/test_facade_capability_native_handle_entry"
units_dir="$build_root/units"
bin_dir="$build_root/bin"
binary="$bin_dir/test_facade_capability_native_handle_entry"

printf '[TEST] facade capability/native-handle export contract\n'

require_fixed "$facade" "TSSLBackendImplType = fafafa.ssl.base.TSSLBackendImplType;" \
  "main facade must re-export TSSLBackendImplType"
require_fixed "$facade" "TSSLFeatureSupportLevel = fafafa.ssl.base.TSSLFeatureSupportLevel;" \
  "main facade must re-export TSSLFeatureSupportLevel"
require_fixed "$facade" "TSSLFeature = fafafa.ssl.base.TSSLFeature;" \
  "main facade must re-export TSSLFeature"
require_fixed "$facade" "TSSLFeatures = fafafa.ssl.base.TSSLFeatures;" \
  "main facade must re-export TSSLFeatures"
require_fixed "$facade" "TSSLCipherSupport = fafafa.ssl.base.TSSLCipherSupport;" \
  "main facade must re-export TSSLCipherSupport"
require_fixed "$facade" "TSSLHashSupport = fafafa.ssl.base.TSSLHashSupport;" \
  "main facade must re-export TSSLHashSupport"
require_fixed "$facade" "TSSLKeyExchangeSupport = fafafa.ssl.base.TSSLKeyExchangeSupport;" \
  "main facade must re-export TSSLKeyExchangeSupport"
require_fixed "$facade" "TSSLBackendCapabilities = fafafa.ssl.base.TSSLBackendCapabilities;" \
  "main facade must re-export TSSLBackendCapabilities"
require_fixed "$facade" "ISSLNativeHandleAccess = fafafa.ssl.base.ISSLNativeHandleAccess;" \
  "main facade must re-export ISSLNativeHandleAccess"
require_fixed "$facade" "sslImplNative = fafafa.ssl.base.sslImplNative;" \
  "main facade must re-export backend implementation enum values"
require_fixed "$facade" "sslSupportStable = fafafa.ssl.base.sslSupportStable;" \
  "main facade must re-export feature-support enum values"
require_fixed "$facade" "sslFeatSNI = fafafa.ssl.base.sslFeatSNI;" \
  "main facade must re-export feature enum values"
require_fixed "$facade" "function IsFeatureStable(ASupport: TSSLFeatureSupportLevel): Boolean;" \
  "main facade must declare IsFeatureStable"
require_fixed "$facade" "function GetCapabilitiesDescription(const ACaps: TSSLBackendCapabilities): string;" \
  "main facade must declare GetCapabilitiesDescription"
require_fixed "$api_ref" '`fafafa.ssl` 主门面当前也 re-export `ISSLNativeHandleAccess` 与 capability helper surface（如 `TSSLBackendCapabilities` / `IsFeatureStable(...)` / `GetCapabilitiesDescription(...)`）。' \
  "API reference must record the main facade capability/native-handle coverage"

mkdir -p "$units_dir" "$bin_dir"
fpc -B -Fu./src -Fu./tests -FU"$units_dir" -FE"$bin_dir" -o"$binary" "$contract_src" >/dev/null
if [[ ! -x "$binary" ]]; then
  fail "facade capability/native-handle contract source must compile through uses fafafa.ssl"
fi

"$binary"

printf '[PASS] facade capability/native-handle export contract passed\n'
