# Context Builder Server SNI Validation Alignment Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Align `TSSLContextBuilder.ValidateServer` with the current runtime contract: server contexts may retain legacy `.WithSNI(...)` state for compatibility, but server-side connections ignore it, so validation must stop emitting client-oriented guidance.

**Architecture:** Add a focused validation regression in `tests/config/test_config_validation.pas` that builds a valid server config with certificate + key + `.WithSNI(...)`. The config should remain valid, but the warning must explicitly state that server-side connections ignore this deprecated setting. Then make the smallest safe validation change in `src/fafafa.ssl.context.builder.pas`: split server validation away from the client-specific SNI warning path so server configs receive server-accurate guidance while preserving all existing client warnings and server-required certificate/key checks.

**Tech Stack:** Free Pascal, validation-only regression, no handshake required

## Files
- Add: `docs/plans/2026-03-20-context-builder-server-sni-validation-alignment.md`
- Modify: `tests/config/test_config_validation.pas`
- Modify: `src/fafafa.ssl.context.builder.pas`

## Steps
1. RED
   - Add a server validation case using:
     - `WithTLS12And13`
     - `WithCertificatePEM(...)`
     - `WithPrivateKeyPEM(...)`
     - `.WithSNI('server.example.com')`
   - Assert:
     - `ValidateServer` remains valid
     - warnings are present
     - warning mentions server-side ignore / deprecated server-context semantics
     - warning no longer points to `ISSLClientConnection.SetServerName`
   - Run the config-validation suite and confirm the new assertion fails on current code.

2. GREEN
   - Adjust `ValidateServer` so it no longer inherits the client-only `.WithSNI(...)` warning text.
   - Keep shared protocol/cipher/session checks intact.
   - Preserve server-specific certificate/private-key/CA warnings.

3. VERIFY
   - Re-run `tests/config/test_config_validation.pas`.
   - Re-run adjacent `ServerName` regressions:
     - `tests/test_context_builder_server_servername_runtime_consistency.pas`
     - `tests/test_freepascal_context_server_name_inheritance.pas`
   - Run `python3 scripts/compile_all_modules.py`.

4. WRITEBACK
   - Update `task_plan.md`, `findings.md`, and `progress.md` with the new validation/runtime alignment evidence and the next review queue.
