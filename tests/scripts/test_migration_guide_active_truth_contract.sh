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

migration_guide="docs/guides/MIGRATION_GUIDE.md"

echo "[TEST] migration guide active truth contract"

require_fixed "$migration_guide" "> **版本**: rolling" \
  "MIGRATION_GUIDE must declare rolling version instead of stale v0.8 snapshot"
require_fixed "$migration_guide" '当前迁移真相以 `src/fafafa.ssl.base.pas`、`src/fafafa.ssl.pas`、`src/fafafa.ssl.tls.pas` 和 `docs/reference/API_REFERENCE.md` 为准。' \
  "MIGRATION_GUIDE must anchor current truth to source and canonical API reference"
require_fixed "$migration_guide" '预 `v1.0` 的历史变更只适合帮助你阅读旧代码，不应再被当成当前 active public API 说明。' \
  "MIGRATION_GUIDE must demote pre-v1.0 version history to historical context"
require_fixed "$migration_guide" "uses" \
  "MIGRATION_GUIDE must keep concrete code examples"
require_fixed "$migration_guide" "fafafa.ssl," \
  "MIGRATION_GUIDE must use the current public facade unit"
require_fixed "$migration_guide" "fafafa.ssl.context.builder;" \
  "MIGRATION_GUIDE must use the current context-builder unit"
require_fixed "$migration_guide" "LContext := TSSLContextBuilder.Create" \
  "MIGRATION_GUIDE must use current builder-based context creation in migration examples"
require_fixed "$migration_guide" "LTLS := TSSLConnector.FromContext(LContext).ConnectSocket(THandle(LSocket), 'example.com');" \
  "MIGRATION_GUIDE must use current connector facade for client migrations"
require_fixed "$migration_guide" "if Supports(LConn, ISSLClientConnection, LClientConn) then" \
  "MIGRATION_GUIDE must show current per-connection client-role access when using raw ISSLConnection"
require_fixed "$migration_guide" "LClientConn.SetServerName('example.com');" \
  "MIGRATION_GUIDE must use current per-connection SNI setter"
require_fixed "$migration_guide" '`TSSLConfig.ServerName` / `ISSLContext.SetServerName(...)` / `TSSLContextBuilder.WithSNI(...)` 当前都只应视为 compatibility-only 入口。' \
  "MIGRATION_GUIDE must classify old context-level SNI surfaces as compatibility-only"
require_fixed "$migration_guide" '`TSSLEnterpriseConfig` 当前 helper 名称是 `IsFIPSEnabled`、`GetTrustedRoots`、`GetAllPolicies`。' \
  "MIGRATION_GUIDE must use current WinSSL enterprise helper names"
require_fixed "$migration_guide" '`GetFriendlyErrorMessage(...)` / `GetOpenSSLErrorCategory(...)` 当前来自 `fafafa.ssl.openssl.api.err`，属于 OpenSSL-specific low-level helper，不是通用 public facade API。' \
  "MIGRATION_GUIDE must bound OpenSSL low-level helper scope"

require_absent "$migration_guide" "> **版本**: v0.8" \
  "MIGRATION_GUIDE must stop advertising stale v0.8 as current version"
require_absent "$migration_guide" "uses fafafa.ssl.abstract.intf;" \
  "MIGRATION_GUIDE must stop using removed abstract.intf unit in active code examples"
require_absent "$migration_guide" "uses fafafa.ssl.openssl," \
  "MIGRATION_GUIDE must stop teaching nonexistent fafafa.ssl.openssl facade unit"
require_absent "$migration_guide" "IsFipsModeEnabled" \
  "MIGRATION_GUIDE must stop using stale enterprise helper name IsFipsModeEnabled"
require_absent "$migration_guide" "GetEnterpriseTrustedRoots" \
  "MIGRATION_GUIDE must stop using stale enterprise helper name GetEnterpriseTrustedRoots"
require_absent "$migration_guide" "GetGroupPolicies" \
  "MIGRATION_GUIDE must stop using stale enterprise helper name GetGroupPolicies"
require_absent "$migration_guide" "CreateOpenSSLLibrary;" \
  "MIGRATION_GUIDE must stop promoting backend-specific CreateOpenSSLLibrary in active migration examples"

echo "[PASS] migration guide active truth contract passed"
