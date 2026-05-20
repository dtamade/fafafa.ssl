# Shared Client Context SNI Fallback Cut

## Goal

把剩余 shared shim 仍保留的 `sslCtxClient` inherited context-level `ServerName` fallback 一次性收掉，让 OpenSSL / WolfSSL / MbedTLS / WinSSL 跟 FreePascal 当前的 no-inheritance 规则重新对齐。

## Status

- completed on 2026-05-18
- historical intermediate state only; superseded on 2026-05-20 by `docs/plans/2026-05-20-context-servername-dead-seam-removal.md`
- implementation truth:
  - on 2026-05-18, `src/fafafa.ssl.context.compat.pas` kept the seam but returned `''` for any non-nil context
  - on 2026-05-20, that dead seam was removed entirely
  - current source truth is:
    - all backends follow the same no-inheritance rule
    - OpenSSL / WolfSSL / MbedTLS / WinSSL no longer route through a shared helper
    - `src/fafafa.ssl.context.compat.pas` no longer exists

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
- later follow-up closeout on 2026-05-20 removed the helper entirely:
  - `src/fafafa.ssl.context.compat.pas` deleted
  - OpenSSL / WolfSSL / MbedTLS / WinSSL no longer reference `GetContextLevelServerNameCompatibilityValue(...)`
  - the focused contract now guards helper absence rather than helper presence
