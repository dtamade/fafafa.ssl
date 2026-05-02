# WinSSL Performance And Backend Comparison SNI Cleanup Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove deprecated context-level `SetServerName(...)` guidance from selected WinSSL performance/session benchmark tests and the backend comparison test where the code is exercising real connection flow, data transfer, certificate handling, and expected handshake failures rather than compatibility coverage.

**Architecture:** Treat this as a narrow test-guidance cleanup batch. Add a focused grep contract for the selected files, then update each normal client flow to set SNI on `ISSLClientConnection` immediately after `CreateConnection(...)` and before `Connect`. Preserve all timing, data-transfer, certificate, and error-path assertions. In the session reuse benchmark, keep `SetSession(...)` before `SetServerName(...)` and both before `Connect`.

**Tech Stack:** Pascal tests, shell contract test, focused Win64 cross-compile verification

---

### Task 1: Add focused RED contract

**Files:**
- Add: `tests/scripts/test_winssl_performance_and_backend_comparison_no_context_level_sni_guidance_contract.sh`

**Step 1: Write the contract**

- Limit scope to:
  - `tests/winssl/test_winssl_performance.pas`
  - `tests/winssl/test_winssl_session_reuse_benchmark.pas`
  - `tests/integration/test_backend_comparison.pas`
- Fail if any of those files still use context-level `SetServerName(...)` on local context variables such as:
  - `LContext.SetServerName(...)`
  - `LCtx.SetServerName(...)`
  - `LWinSSLCtx.SetServerName(...)`
  - `LOpenSSLCtx.SetServerName(...)`

**Step 2: Run RED**

Run:
`bash tests/scripts/test_winssl_performance_and_backend_comparison_no_context_level_sni_guidance_contract.sh`

Expected:
- FAIL on the current stale context-level SNI setup

### Task 2: GREEN - Update selected normal-flow tests

**Files:**
- Modify: `tests/winssl/test_winssl_performance.pas`
- Modify: `tests/winssl/test_winssl_session_reuse_benchmark.pas`
- Modify: `tests/integration/test_backend_comparison.pas`

**Step 1: Replace stale guidance**

- In each selected client flow:
  - create the connection first
  - cast to `ISSLClientConnection`
  - set `ServerName` on the connection before `Connect`
- For session reuse flows:
  - keep `SetSession(...)` before `SetServerName(...)`
  - keep both before `Connect`
- Preserve all existing timing, response, certificate, and negative-handshake assertions

**Step 2: Re-run the contract**

Run:
`bash tests/scripts/test_winssl_performance_and_backend_comparison_no_context_level_sni_guidance_contract.sh`

Expected:
- PASS

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_winssl_performance_and_backend_comparison_no_context_level_sni_guidance_contract.sh`
- Test: `tests/winssl/test_winssl_performance.pas`
- Test: `tests/winssl/test_winssl_session_reuse_benchmark.pas`
- Test: `tests/integration/test_backend_comparison.pas`

**Step 1: Run focused checks**

Run:
- `bash tests/scripts/test_winssl_performance_and_backend_comparison_no_context_level_sni_guidance_contract.sh`
- `fpc -Twin64 -Fu./src -otmp/test_winssl_performance.exe tests/winssl/test_winssl_performance.pas`
- `fpc -Twin64 -Fu./src -otmp/test_winssl_session_reuse_benchmark.exe tests/winssl/test_winssl_session_reuse_benchmark.pas`
- `fpc -Twin64 -Fu./src -Fu./tests -otmp/test_backend_comparison.exe tests/integration/test_backend_comparison.pas`

Expected:
- contract passes
- selected files cross-compile successfully on the local Linux host targeting Win64

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record the classification**

- Note that these files are normal online/performance/comparison flows, not intentional compatibility coverage.
- Note that even expected-failure handshakes in performance/comparison suites still count as connection-flow guidance when they are not explicitly testing context fallback.

**Step 2: Roll the next queue**

- Continue classifying the remaining backend-specific online tests with the same rule:
  - normal client flow => move to per-connection SNI
  - explicit compatibility/API-surface coverage => label
