# Context Server-Mode ServerName Isolation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Preserve legacy context-level `ServerName` state for server contexts, but stop server-side `CreateConnection(...)` from inheriting that client-only SNI fallback into new connections.

**Architecture:** Keep the earlier `BuildServer` fix intact: builder/factory/direct-context paths may still retain `Ctx.GetServerName` for compatibility. Tighten the next boundary one layer lower at connection construction. Add a focused FreePascal-safe regression that locks three cases:
- client builder path still inherits `ServerName`
- server builder path preserves `Ctx.GetServerName`
- server-side connections created from that context must expose empty `ISSLClientConnection.GetServerName`

Then apply the smallest shared guard across backend constructors: only inherit `AContext.GetServerName` when `AContext.GetContextType = sslCtxClient`. This preserves client fallback behavior while preventing server-path leakage.

**Tech Stack:** Free Pascal, constructor-level regression, no network handshake required

## Files
- Add: `docs/plans/2026-03-20-context-server-mode-servername-isolation.md`
- Modify: `tests/test_context_builder_server_servername_runtime_consistency.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`
- Modify: `src/fafafa.ssl.openssl.connection.pas`
- Modify: `src/fafafa.ssl.mbedtls.connection.pas`
- Modify: `src/fafafa.ssl.wolfssl.connection.pas`
- Modify: `src/fafafa.ssl.winssl.connection.pas`

## Steps
1. RED
   - Update the focused FreePascal regression:
     - `BuildClient.WithSNI(...)` remains the passing control
     - `BuildServer.WithSNI(...)` must preserve `Ctx.GetServerName`
     - but the server-side connection created from that context must keep `GetServerName = ''`
     - add one direct server-context control if needed to prove the same isolation outside the builder path
   - Run the isolated test and confirm the current constructors still leak the server-context value into the connection.

2. GREEN
   - In each backend constructor / allocation path that currently copies `AContext.GetServerName`:
     - keep client-context fallback intact
     - add a `GetContextType = sslCtxClient` guard before inheriting
   - Preserve per-connection overrides, ALPN initialization, validation behavior, and existing deprecated-context storage.

3. VERIFY
   - Re-run the focused regression.
   - Re-run adjacent client-side `ServerName` regressions:
     - `tests/test_freepascal_context_server_name_inheritance.pas`
     - `tests/test_connection_builder_hostname_precedence.pas`
     - `tests/test_tls_connector_hostname_override_precedence.pas`
   - Run `python3 scripts/compile_all_modules.py`.

4. WRITEBACK
   - Update `task_plan.md`, `findings.md`, and `progress.md` with:
     - why the old plan memory saying this was fixed did not match current disk state
     - why preserving `Ctx.GetServerName` on server contexts is compatible with blocking server-connection inheritance
     - the next queue after this batch
