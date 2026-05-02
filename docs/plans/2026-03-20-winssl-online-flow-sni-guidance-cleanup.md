# WinSSL Online Flow SNI Guidance Cleanup Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove deprecated context-level `SetServerName(...)` guidance from selected WinSSL online-flow tests that exercise real client handshakes, ALPN negotiation, hostname mismatch handling, and session-resumption baselines.

**Architecture:** Treat this as a narrow test-guidance cleanup batch. Add a focused grep contract for the selected WinSSL files, then update each normal client flow to set SNI on `ISSLClientConnection` immediately after `CreateConnection(...)` and before `Connect`. Keep protocol, ALPN, timing, and verification assertions unchanged.

**Tech Stack:** Pascal tests, shell contract test, focused compile verification

---

### Task 1: Add focused RED contract

**Files:**
- Add: `tests/scripts/test_winssl_online_flow_tests_no_context_level_sni_guidance_contract.sh`

**Step 1: Write the contract**

- Limit scope to:
  - `tests/winssl/test_winssl_hostname_mismatch_online.pas`
  - `tests/winssl/test_winssl_alpn_sni.pas`
  - `tests/winssl/test_winssl_session_resumption.pas`
- Fail if any of those files still use:
  - `Context.SetServerName(...)`
  - `Ctx.SetServerName(...)`
  - `LCtx.SetServerName(...)`
  - `LContext.SetServerName(...)`

**Step 2: Run RED**

Run:
`bash tests/scripts/test_winssl_online_flow_tests_no_context_level_sni_guidance_contract.sh`

Expected:
- FAIL on the current stale context-level SNI setup

### Task 2: GREEN - Update selected WinSSL online-flow tests

**Files:**
- Modify: `tests/winssl/test_winssl_hostname_mismatch_online.pas`
- Modify: `tests/winssl/test_winssl_alpn_sni.pas`
- Modify: `tests/winssl/test_winssl_session_resumption.pas`

**Step 1: Replace stale guidance**

- In each online client flow:
  - create the connection first
  - cast to `ISSLClientConnection`
  - set `ServerName` on the connection before `Connect`
- Preserve all existing hostname mismatch, ALPN, timing, and session-resumption assertions

**Step 2: Re-run the contract**

Run:
`bash tests/scripts/test_winssl_online_flow_tests_no_context_level_sni_guidance_contract.sh`

Expected:
- PASS

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_winssl_online_flow_tests_no_context_level_sni_guidance_contract.sh`
- Test: `tests/winssl/test_winssl_hostname_mismatch_online.pas`
- Test: `tests/winssl/test_winssl_alpn_sni.pas`
- Test: `tests/winssl/test_winssl_session_resumption.pas`

**Step 1: Run focused checks**

Run:
- `bash tests/scripts/test_winssl_online_flow_tests_no_context_level_sni_guidance_contract.sh`
- `fpc -Fu./src -otmp/test_winssl_hostname_mismatch_online tests/winssl/test_winssl_hostname_mismatch_online.pas`
- `fpc -Fu./src -otmp/test_winssl_alpn_sni tests/winssl/test_winssl_alpn_sni.pas`
- `fpc -Fu./src -otmp/test_winssl_session_resumption tests/winssl/test_winssl_session_resumption.pas`

Expected:
- contract passes
- selected WinSSL tests compile on the local Linux harness

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record the classification**

- Note that these files exercise normal WinSSL online client flows, not intentional context-level compatibility coverage.
- Note that hostname mismatch and session-resumption baselines still belong to normal client-flow guidance even when they observe negative or timing-based outcomes.

**Step 2: Roll the next queue**

- Continue classifying remaining candidates with the same rule:
  - normal online/performance/session flow => move to per-connection SNI
  - explicit compatibility/API-surface coverage => label
