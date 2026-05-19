#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
abstraction_doc="$root_dir/docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md"
selector_doc="$root_dir/docs/reference/BACKEND_SELECTOR_DESIGN.md"
platform_doc="$root_dir/docs/PLATFORM_SUPPORT.md"

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

require_absent "$abstraction_doc" "| FIPS | ✅ | ❌ | ✅ |" \
  "Backend abstraction design doc still advertises stale OpenSSL FIPS truth"
require_present "$abstraction_doc" "| FIPS | ❌ 默认构建 | ❌ | ✅ |" \
  "Backend abstraction design doc no longer records current default-build FIPS truth"
require_present "$abstraction_doc" "OpenSSL 如需 FIPS 需要专门模块/构建；当前默认 backend capability 不发布 FIPS。" \
  "Backend abstraction design doc no longer records the OpenSSL FIPS note"

require_absent "$selector_doc" "| FIPS | ✅ | ✅ | ❌ |" \
  "Backend selector design doc still advertises stale OpenSSL FIPS truth"
require_present "$selector_doc" "| FIPS | ❌ 默认构建 | ✅ | ❌ |" \
  "Backend selector design doc no longer records current default-build FIPS truth"
require_present "$selector_doc" "OpenSSL 如需进入 FIPS 路线，必须先满足专门模块/构建前提；默认 capability 不能当成已满足。" \
  "Backend selector design doc no longer records the OpenSSL FIPS selector note"

require_absent "$platform_doc" "| **FIPS 模式** | 支持                | 支持         |" \
  "Platform support doc still markets OpenSSL default-build FIPS support"
require_present "$platform_doc" "| **FIPS 模式** | 默认构建不发布      | 支持         |" \
  "Platform support doc no longer records the current OpenSSL/WinSSL FIPS split"
require_present "$platform_doc" "OpenSSL 若要进入 FIPS 路线，需要额外的专门模块/构建；当前 fafafa.ssl 默认 OpenSSL backend capability 仍为未发布。" \
  "Platform support doc no longer records the current OpenSSL FIPS note"

echo "[PASS] active FIPS docs remain aligned with current source truth"
