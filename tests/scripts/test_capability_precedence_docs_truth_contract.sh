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

guide="docs/CAPABILITY_MATRIX_GUIDE.md"
api_reference="docs/reference/API_REFERENCE.md"
backend_matrix="docs/BACKEND_CAPABILITY_MATRIX.md"
base_unit="src/fafafa.ssl.base.pas"
serializer_unit="src/fafafa.ssl.capability.serializer.pas"
diff_unit="src/fafafa.ssl.capability.diff.pas"

echo "[TEST] capability precedence docs truth contract"

require_fixed "$base_unit" "@note runtime truth 以 support-level 字段为准；legacy boolean 仅作兼容派生" \
  "base unit must keep the support-level-first capability truth note"
require_fixed "$serializer_unit" "v1.2 support-level 字段一旦出现，就以它为真相源回填 legacy boolean" \
  "serializer must keep the support-level-first precedence rule"
require_fixed "$diff_unit" "v1.2 support-level 为真相，legacy boolean 仅作兼容回退" \
  "capability diff must keep the support-level-first comparison rule"

require_fixed "$guide" "当 \`SNISupport\` / \`ALPNSupport\` / \`OCSPStaplingSupport\` / \`CertTransparencySupport\` / \`SessionTicketsSupport\` 存在时，它们是当前 capability truth；legacy \`SupportsSNI\` / \`SupportsALPN\` / \`SupportsOCSPStapling\` / \`SupportsCertificateTransparency\` / \`SupportsSessionTickets\` 只作为兼容投影。" \
  "CAPABILITY_MATRIX_GUIDE must state that paired *Support fields are the truth source"
require_fixed "$guide" "\`SupportsTLS13\` 仍然是主 bool truth，因为当前没有 \`TLS13Support\` 支持级别字段。" \
  "CAPABILITY_MATRIX_GUIDE must explicitly keep SupportsTLS13 as the primary bool truth"
require_fixed "$guide" "Lib := TSSLFactory.GetLibraryInstance(sslOpenSSL);" \
  "CAPABILITY_MATRIX_GUIDE must use GetLibraryInstance in high-entry examples"
require_fixed "$guide" "CompatibilityLevel: Integer;  // 0-100" \
  "CAPABILITY_MATRIX_GUIDE must publish the current CompatibilityLevel type"
require_fixed "$guide" "NormalizeLegacyCapabilityBooleans(Result);" \
  "CAPABILITY_MATRIX_GUIDE new-backend example must show the compatibility projection step"
require_fixed "$guide" "**API 参考**: \`docs/reference/API_REFERENCE.md\` - 完整 API 文档" \
  "CAPABILITY_MATRIX_GUIDE must link to the current API reference path"
require_fixed "$guide" "**迁移指南**: \`docs/guides/MIGRATION_GUIDE.md\` - 当前迁移主线" \
  "CAPABILITY_MATRIX_GUIDE must link to the current migration guide path"

require_fixed "$api_reference" "当 \`SNISupport\` / \`ALPNSupport\` / \`OCSPStaplingSupport\` / \`CertTransparencySupport\` / \`SessionTicketsSupport\` 出现时，它们是当前 source/runtime truth；legacy \`SupportsSNI\` / \`SupportsALPN\` / \`SupportsOCSPStapling\` / \`SupportsCertificateTransparency\` / \`SupportsSessionTickets\` 仅作为兼容投影。" \
  "API_REFERENCE must state the support-level-first precedence rule"
require_fixed "$api_reference" "\`SupportsTLS13\` 仍是主 bool 字段，因为当前没有 \`TLS13Support\`。" \
  "API_REFERENCE must explicitly keep SupportsTLS13 as the primary bool truth"
require_fixed "$api_reference" "CompatibilityLevel: Integer;  // 0-100" \
  "API_REFERENCE must publish the current CompatibilityLevel type"
require_fixed "$api_reference" "Lib := TSSLFactory.GetLibraryInstance(ABackend);" \
  "API_REFERENCE capability example must use GetLibraryInstance"

require_fixed "$backend_matrix" "本表对 SNI / ALPN / OCSP stapling / Certificate Transparency / Session Tickets 统一按 \`*Support\` 支持级别字段汇总；legacy \`Supports*\` 布尔值仅作为兼容投影。" \
  "BACKEND_CAPABILITY_MATRIX must explain the table's support-level-first precedence"
require_fixed "$backend_matrix" "\`SupportsTLS13\` 仍按主 bool 字段解读，因为当前没有 \`TLS13Support\`。" \
  "BACKEND_CAPABILITY_MATRIX must explicitly preserve the TLS13 bool truth note"

require_absent "$guide" "Lib := TSSLFactory.GetLibrary(sslOpenSSL);" \
  "CAPABILITY_MATRIX_GUIDE must stop using GetLibrary as the high-entry example"
require_absent "$guide" "CompatibilityLevel: Byte;  // 0-100" \
  "CAPABILITY_MATRIX_GUIDE must stop publishing the stale CompatibilityLevel type"
require_absent "$api_reference" "CompatibilityLevel: Byte;  // 0-100" \
  "API_REFERENCE must stop publishing the stale CompatibilityLevel type"
require_absent "$api_reference" "Lib := TSSLFactory.GetLibrary(ABackend);" \
  "API_REFERENCE capability example must stop using GetLibrary"
require_absent "$guide" "**API 参考**: \`docs/API_REFERENCE.md\` - 完整 API 文档" \
  "CAPABILITY_MATRIX_GUIDE must stop linking to the stale API reference path"
require_absent "$guide" "**迁移指南**: \`docs/MIGRATION_GUIDE_V1.1.md\` - v1.1/v1.2 迁移说明" \
  "CAPABILITY_MATRIX_GUIDE must stop linking to the stale migration guide path"

echo "[PASS] capability precedence docs truth contract passed"
