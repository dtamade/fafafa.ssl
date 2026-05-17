# Shared Client Context SNI Fallback Cut

## Goal

把剩余 shared shim 仍保留的 `sslCtxClient` inherited context-level `ServerName` fallback 一次性收掉，让 OpenSSL / WolfSSL / MbedTLS / WinSSL 跟 FreePascal 当前的 no-inheritance 规则重新对齐。

## Status

- completed on 2026-05-18
- implementation truth:
  - `src/fafafa.ssl.context.compat.pas` now keeps the seam but returns `''` for any non-nil context
  - OpenSSL / WolfSSL / MbedTLS / WinSSL still route through the shared seam, but the seam no longer forwards deprecated context state
  - FreePascal remains off the shared helper and keeps the same no-inheritance rule

## Architecture

- 先用一个 Linux-safe 的跨-backend focused contract 打出 RED：
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
- 再改 shared seam：
  - `src/fafafa.ssl.context.compat.pas`
- 不改：
  - context setter/getter 的 API-surface tests
  - builder / factory 的 deprecated context-state write surfaces
  - connector / per-connection hostname APIs

## Files

- Add: `docs/plans/2026-05-18-shared-client-context-sni-fallback-cut.md`
- Add: `tests/test_cross_backend_client_context_server_name_clarification.pas`
- Modify: `src/fafafa.ssl.context.compat.pas`
- Update: `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
- Update: `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps

1. Add a cross-backend client-context contract that expects no inherited `ServerName` on new connections.
2. Observe RED on the backends still using the shared compatibility shim.
3. Change `GetContextLevelServerNameCompatibilityValue(...)` so `sslCtxClient` also stops inheriting deprecated context-level SNI.
4. Re-run focused verification:
   - `tests/test_cross_backend_client_context_server_name_clarification.pas`
   - `tests/test_context_builder_server_servername_runtime_consistency.pas`
   - `tests/test_factory_server_name_scope_clarification.pas`
   - `tests/test_factory_config_server_name_isolation.pas`
   - `tests/test_sslctxboth_client_capability_clarification.pas`
   - `bash tests/scripts/test_context_server_name_compat_shim_contract.sh`
5. Close out docs / working memory and decide whether the remaining direct server-context control case still deserves to survive.

## Expected Outputs

- OpenSSL / WolfSSL / MbedTLS / WinSSL stop inheriting deprecated context-level `ServerName` on `sslCtxClient`
- FreePascal no-inheritance rule becomes the shared cross-backend rule
- the next compatibility question can move from client fallback inheritance to the last direct server-context legacy-state control case

## Closeout

- dedicated cross-backend RED proved OpenSSL / WolfSSL / MbedTLS still inherited `"client.example.com"` while FreePascal already stayed empty
- after the shared helper cut, the dedicated cross-backend contract turned green and the adjacent builder/factory consistency contracts stayed green
- the stale source contract was updated to current truth:
  - helper required in OpenSSL / WolfSSL / MbedTLS / WinSSL
  - helper forbidden in FreePascal
  - direct `(AContext|FContext).GetServerName` reads forbidden everywhere
