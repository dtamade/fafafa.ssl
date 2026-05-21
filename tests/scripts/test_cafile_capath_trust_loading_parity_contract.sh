#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

require_fixed() {
  local file="$1"
  local pattern="$2"
  local message="$3"

  if ! rg -F -n --quiet -- "$pattern" "$file"; then
    echo "[FAIL] $message"
    exit 1
  fi
}

factory_file="src/fafafa.ssl.factory.pas"
api_ref="docs/reference/API_REFERENCE.md"

require_fixed "$factory_file" \
  "if LConfig.CAPath <> '' then" \
  "factory one-shot path no longer checks CAPath before trust loading"
require_fixed "$factory_file" \
  "Result.LoadCAPath(LConfig.CAPath);" \
  "factory one-shot path no longer loads CAPath into the created context"

for file in \
  src/fafafa.ssl.openssl.backed.pas \
  src/fafafa.ssl.freepascal.lib.pas \
  src/fafafa.ssl.mbedtls.lib.pas \
  src/fafafa.ssl.wolfssl.lib.pas \
  src/fafafa.ssl.winssl.lib.pas
do
  require_fixed "$file" \
    "if LConfig.CAFile <> '' then" \
    "$file no longer checks CAFile on the direct-library path"
  require_fixed "$file" \
    "Result.LoadCAFile(LConfig.CAFile);" \
    "$file no longer loads CAFile on the direct-library path"
  require_fixed "$file" \
    "if LConfig.CAPath <> '' then" \
    "$file no longer checks CAPath on the direct-library path"
  require_fixed "$file" \
    "Result.LoadCAPath(LConfig.CAPath);" \
    "$file no longer loads CAPath on the direct-library path"
done

require_fixed "$api_ref" \
  "- \`CAFile\`" \
  "API reference direct-library aligned-field list no longer mentions CAFile"
require_fixed "$api_ref" \
  "- \`CAPath\`" \
  "API reference direct-library aligned-field list no longer mentions CAPath"

echo "[PASS] CAFile/CAPath trust-loading parity source contract remains aligned"
