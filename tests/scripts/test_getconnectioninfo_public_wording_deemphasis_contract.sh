#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

require_pattern() {
  local file="$1"
  local pattern="$2"
  if ! grep -F -q -- "$pattern" "$file"; then
    echo "[FAIL] missing GetConnectionInfo wording pattern in $file: $pattern"
    exit 1
  fi
}

forbid_pattern() {
  local file="$1"
  local pattern="$2"
  if grep -F -q -- "$pattern" "$file"; then
    echo "[FAIL] stale GetConnectionInfo wording still present in $file: $pattern"
    exit 1
  fi
}

require_pattern "src/fafafa.ssl.base.pas" "@owner-note 默认 owner 为 ISSLConnectionInfo.GetConnectionInfo；此入口仅兼容保留"
require_pattern "src/fafafa.ssl.base.pas" "@compatibility-note v1.x compatibility-core mirror; not recommended as the primary entry for new code; Stage-A demotion target is ISSLConnectionInfo"

require_pattern "docs/reference/API_REFERENCE.md" "function GetConnectionInfo: TSSLConnectionInfo; // 仅兼容保留；新代码优先走 ISSLConnectionInfo.GetConnectionInfo"
require_pattern "docs/reference/API_REFERENCE.md" "\`GetConnectionInfo\` 在 \`ISSLConnection\` 上仅作为 \`v1.x\` compatibility-core mirror 保留；需要完整连接信息记录时，新代码优先通过 \`ISSLConnectionInfo.GetConnectionInfo\`。"
require_pattern "docs/reference/API_REFERENCE.md" "WriteLn('协议版本: ', GetProtocolName(LInfo.ProtocolVersion));"
require_pattern "docs/reference/API_REFERENCE.md" "WriteLn('密码套件: ', LInfo.CipherSuite);"
require_pattern "docs/reference/API_REFERENCE.md" "新代码若要获取这份结构，优先通过 \`ISSLConnectionInfo.GetConnectionInfo\`；\`ISSLConnection.GetConnectionInfo\` 当前只作为 \`v1.x\` compatibility-core mirror 保留"

require_pattern "docs/reference/INTERFACE_DESIGN_V2.md" "LConn.GetConnectionInfo;  // 仅兼容保留，不再作为新代码推荐入口"
require_pattern "docs/reference/INTERFACE_DESIGN_V2.md" "| GetConnectionInfo | ISSLConnectionInfo | 默认 owner 已切到 ISSLConnectionInfo；core 侧仅兼容保留 |"
require_pattern "docs/reference/INTERFACE_DESIGN_V2.md" "换句话说，\`GetConnectionInfo\` 在 \`ISSLConnection\` core 上虽然仍存在，但这里只把它视为 compatibility mirror，不再把它当作新代码默认入口。"

forbid_pattern "docs/reference/INTERFACE_DESIGN_V2.md" "LConn.GetConnectionInfo;  // 仍然存在"
forbid_pattern "docs/reference/INTERFACE_DESIGN_V2.md" "| GetConnectionInfo | ISSLConnectionInfo | Stage A 先 demote 出 core |"

echo "[PASS] GetConnectionInfo public wording de-emphasis is aligned across source and docs"
