#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

factory_file="src/fafafa.ssl.factory.pas"
api_ref="docs/reference/API_REFERENCE.md"

require_fixed() {
  local needle="$1"
  local file="$2"
  local message="$3"

  if ! rg -F -n --quiet "$needle" "$file"; then
    echo "[FAIL] $message"
    exit 1
  fi
}

require_fixed "Option-bridge compatibility inputs keep their historical write-through behavior:" \
  "$factory_file" \
  "factory source no longer documents option-bridge write-through precedence"
require_fixed "if AConfig.EnableCompression then" \
  "$factory_file" \
  "factory no longer applies EnableCompression before final option projection"
require_fixed "if AConfig.EnableSessionTickets then" \
  "$factory_file" \
  "factory no longer applies EnableSessionTickets before final option projection"
require_fixed "if AConfig.EnableOCSPStapling then" \
  "$factory_file" \
  "factory no longer applies EnableOCSPStapling before final option projection"
require_fixed "AConfig.EnableCompression := not (ssoDisableCompression in AConfig.Options);" \
  "$factory_file" \
  "factory no longer projects final compression truth back to legacy boolean"
require_fixed "AConfig.EnableSessionTickets := ssoEnableSessionTickets in AConfig.Options;" \
  "$factory_file" \
  "factory no longer projects final session-ticket truth back to legacy boolean"
require_fixed "AConfig.EnableOCSPStapling := ssoEnableOCSPStapling in AConfig.Options;" \
  "$factory_file" \
  "factory no longer projects final OCSP stapling truth back to legacy boolean"

require_fixed 'When callers pass conflicting `Options` and option-bridge booleans, normalization currently treats the legacy booleans as the compatibility write surface:' \
  "$api_ref" \
  "API reference no longer records option-bridge conflict precedence"
require_fixed 'the legacy boolean wins, updates the relevant option bit, and then the final `Options` truth is projected back into the boolean fields.' \
  "$api_ref" \
  "API reference no longer explains the final option-truth projection"

require_fixed "TSSLFactory.NormalizeConfig(LConfig);" \
  "src/fafafa.ssl.openssl.backed.pas" \
  "OpenSSL SetDefaultConfig no longer normalizes conflicting option-bridge input"
require_fixed "TSSLFactory.NormalizeConfig(LConfig);" \
  "src/fafafa.ssl.freepascal.lib.pas" \
  "FreePascal SetDefaultConfig no longer normalizes conflicting option-bridge input"
require_fixed "TSSLFactory.NormalizeConfig(LConfig);" \
  "src/fafafa.ssl.winssl.lib.pas" \
  "WinSSL SetDefaultConfig no longer normalizes conflicting option-bridge input"
require_fixed "TSSLFactory.NormalizeConfig(LConfig);" \
  "src/fafafa.ssl.mbedtls.lib.pas" \
  "MbedTLS SetDefaultConfig no longer normalizes conflicting option-bridge input"
require_fixed "TSSLFactory.NormalizeConfig(LConfig);" \
  "src/fafafa.ssl.wolfssl.lib.pas" \
  "WolfSSL SetDefaultConfig no longer normalizes conflicting option-bridge input"

echo "[PASS] TSSLConfig option-bridge precedence truth remains frozen across source and docs"
