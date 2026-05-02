# WinSSL Integration Multi SNI Cleanup Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove deprecated context-level `SetServerName(...)` guidance from `tests/winssl/test_winssl_integration_multi.pas`, where the file is exercising real TCP/TLS connection flow, protocol negotiation, transfer sizes, expected handshake failures, and repeated sequential connections rather than intentional context-API coverage.

**Architecture:** Treat this as a narrow connection-flow cleanup batch. Add a focused grep contract for `test_winssl_integration_multi`, then update each selected client flow to set SNI on `ISSLClientConnection` immediately after `CreateConnection(...)` and before `Connect`. Preserve all existing socket setup, protocol configuration, verification mode, HTTP send/receive logic, and error/stability assertions.

**Tech Stack:** Pascal tests, shell contract test, focused Win64 cross-compile verification

---

### Task 1: Add focused RED contract

**Files:**
- Add: `tests/scripts/test_winssl_integration_multi_no_context_level_sni_guidance_contract.sh`

**Step 1: Write the contract**

- Limit scope to:
  - `tests/winssl/test_winssl_integration_multi.pas`
- Fail if the file still uses context-level `SetServerName(...)` on local context variables such as:
  - `LContext.SetServerName(...)`
  - `LCtx.SetServerName(...)`

**Step 2: Run RED**

Run:
`bash tests/scripts/test_winssl_integration_multi_no_context_level_sni_guidance_contract.sh`

Expected:
- FAIL on the current stale context-level SNI setup

### Task 2: GREEN - Update selected normal-flow sites

**Files:**
- Modify: `tests/winssl/test_winssl_integration_multi.pas`

**Step 1: Replace stale guidance**

- In `TestHTTPSServer`, `TestProtocolNegotiation`, `TestDataTransferSizes`, `TestErrorScenarios`, and `TestMultipleSequentialConnections`:
  - keep creating/configuring the shared context first
  - keep TCP connect ordering unchanged
  - create the SSL connection
  - cast to `ISSLClientConnection`
  - set `ServerName` on the connection before `Connect`
- Preserve all existing assertions and request/response flow.
- Do not change the comprehensive context/unit tests in this batch; those are separate API-surface classification candidates.

**Step 2: Re-run the contract**

Run:
`bash tests/scripts/test_winssl_integration_multi_no_context_level_sni_guidance_contract.sh`

Expected:
- PASS

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_winssl_integration_multi_no_context_level_sni_guidance_contract.sh`
- Test: `tests/winssl/test_winssl_integration_multi.pas`

**Step 1: Run focused checks**

Run:
- `bash tests/scripts/test_winssl_integration_multi_no_context_level_sni_guidance_contract.sh`
- `fpc -Twin64 -Fu./src -otmp/test_winssl_integration_multi.exe tests/winssl/test_winssl_integration_multi.pas`

Expected:
- contract passes
- selected file cross-compiles successfully on the local Linux host targeting Win64

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record the classification**

- Note that `test_winssl_integration_multi` is a normal multi-host client/integration flow, not intentional compatibility coverage.
- Note that the remaining `test_winssl_context_comprehensive` and `test_winssl_unit_comprehensive` hits are better treated as explicit `ISSLContext.SetServerName(...)` API-surface coverage to label in a separate batch.

**Step 2: Roll the next queue**

- Continue with explicit API-surface labeling for:
  - `tests/winssl/test_winssl_context_comprehensive.pas`
  - `tests/winssl/test_winssl_unit_comprehensive.pas`
- Later classify server/framework files separately from client-flow cleanup.
