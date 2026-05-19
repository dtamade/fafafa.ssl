#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

ocsp_guide="docs/guides/OCSP_USAGE_GUIDE.md"
ct_guide="docs/guides/CT_IMPLEMENTATION_GUIDE.md"

require_fixed() {
  local needle="$1"
  local file="$2"
  local message="$3"

  if ! rg -F -n --quiet -- "$needle" "$file"; then
    echo "[FAIL] $message"
    exit 1
  fi
}

require_fixed '这里直接回到 `CreateConnection(...)`，是因为 stapled OCSP runtime state 通过 `ISSLOCSPStapling` 挂在连接对象上，握手失败时的 verify 结果也通过 `ISSLCertificateVerification` 从连接侧读取；如果你只是普通客户端接入而不需要这层 owner surface，握手入口仍可保持在 `TSSLConnector` / `TSSLStream`。' \
  "$ocsp_guide" \
  "OCSP_USAGE_GUIDE must explain why it intentionally uses the connection owner path"

require_fixed '这里直接回到 `CreateConnection(...)`，是因为 `ISSLCertificateTransparency` / `ISSLCertificateTransparencyValidation` 这组 CT runtime owner surface 挂在连接对象上；如果你只是普通客户端接入而不需要读取 CT owner surface，握手入口仍可保持在 `TSSLConnector` / `TSSLStream`。' \
  "$ct_guide" \
  "CT_IMPLEMENTATION_GUIDE must explain why it intentionally uses the connection owner path"

echo "[PASS] specialized guides explain why they intentionally use connection owner paths"
