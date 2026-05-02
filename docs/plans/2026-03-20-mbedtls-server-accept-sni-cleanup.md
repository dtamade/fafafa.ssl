# MbedTLS Server Accept SNI Cleanup Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove deprecated context-level `SetServerName(...)` guidance from the client side of the MbedTLS server-accept tests, while preserving the server-side `Accept`/handshake intent.

**Architecture:** Treat this as a narrow mixed server/client-flow cleanup batch. The selected files primarily test server accept and local round-trip behavior, but their client half still performs a real TLS client handshake into the local server. Add a focused grep contract for those files, then move the client-side hostname setup from `ISSLContext` to `ISSLClientConnection` immediately after `CreateConnection(...)` and before `Connect`/handshake driving. Preserve all server startup, socket ordering, self-signed verification settings, handshake loops, and echo assertions.

**Tech Stack:** Pascal tests, shell contract test, focused compile verification

---

### Task 1: Add focused RED contract

**Files:**
- Add: `tests/scripts/test_mbedtls_server_accept_tests_no_context_level_sni_guidance_contract.sh`

**Step 1: Write the contract**

- Limit scope to:
  - `tests/mbedtls/test_mbedtls_server_accept.pas`
  - `tests/mbedtls/test_mbedtls_server_accept_simple.pas`
- Fail if either file still uses context-level `SetServerName(...)` on local context variables such as:
  - `LCtx.SetServerName(...)`
  - `LContext.SetServerName(...)`

**Step 2: Run RED**

Run:
`bash tests/scripts/test_mbedtls_server_accept_tests_no_context_level_sni_guidance_contract.sh`

Expected:
- FAIL on the current stale context-level SNI setup

### Task 2: GREEN - Update selected client-handshake sites

**Files:**
- Modify: `tests/mbedtls/test_mbedtls_server_accept.pas`
- Modify: `tests/mbedtls/test_mbedtls_server_accept_simple.pas`

**Step 1: Replace stale guidance in the client side**

- Keep creating/configuring the client context first.
- Keep `SetVerifyMode([])` on the client context.
- Create the connection.
- Cast to `ISSLClientConnection`.
- Set `ServerName` on the connection before `Connect` / `DoHandshake`.
- Preserve all server accept behavior and round-trip assertions.

**Step 2: Re-run the contract**

Run:
`bash tests/scripts/test_mbedtls_server_accept_tests_no_context_level_sni_guidance_contract.sh`

Expected:
- PASS

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_mbedtls_server_accept_tests_no_context_level_sni_guidance_contract.sh`
- Test: `tests/mbedtls/test_mbedtls_server_accept.pas`
- Test: `tests/mbedtls/test_mbedtls_server_accept_simple.pas`

**Step 1: Run focused checks**

Run:
- `bash tests/scripts/test_mbedtls_server_accept_tests_no_context_level_sni_guidance_contract.sh`
- `fpc -Fu./src -Fu./tests -otmp/test_mbedtls_server_accept tests/mbedtls/test_mbedtls_server_accept.pas`
- `fpc -Fu./src -Fu./tests -otmp/test_mbedtls_server_accept_simple tests/mbedtls/test_mbedtls_server_accept_simple.pas`

Expected:
- contract passes
- selected files compile successfully in the local Linux environment

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record the classification**

- Note that these tests are server-side in overall purpose, but the retained `SetServerName(...)` calls live on the local client handshake half.
- Note that this still counts as stale connection-flow guidance rather than intentional context API coverage.

**Step 2: Roll the next queue**

- Continue with explicit API-surface labeling for:
  - `tests/test_mbedtls_framework.pas`
  - `tests/test_wolfssl_framework.pas`
