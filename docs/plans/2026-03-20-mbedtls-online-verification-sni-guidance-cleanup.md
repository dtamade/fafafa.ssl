# MbedTLS Online Verification SNI Guidance Cleanup Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove deprecated context-level `SetServerName(...)` guidance from selected MbedTLS online verification tests that exercise normal client handshakes, certificate validation, and OCSP capability rather than compatibility fallback behavior.

**Architecture:** Treat this as a narrow test-guidance cleanup batch, not an API-removal or compatibility-contract batch. Add a focused grep contract for the selected MbedTLS online verification files, then update each test to set SNI on `ISSLClientConnection` immediately after `CreateConnection(...)` and before `Connect`. Keep certificate, verify-mode, and OCSP assertions unchanged.

**Tech Stack:** Pascal tests, shell contract test, focused compile verification

---

### Task 1: Add focused RED contract

**Files:**
- Add: `tests/scripts/test_mbedtls_online_verification_tests_no_context_level_sni_guidance_contract.sh`

**Step 1: Write the contract**

- Limit scope to:
  - `tests/mbedtls/test_mbedtls_cert_chain.pas`
  - `tests/mbedtls/test_mbedtls_cert_verify_flags.pas`
  - `tests/mbedtls/test_mbedtls_cert_errors.pas`
  - `tests/mbedtls/test_mbedtls_ocsp_capability.pas`
- Fail if any of those files still use:
  - `Context.SetServerName(...)`
  - `Ctx.SetServerName(...)`
  - `LCtx.SetServerName(...)`
  - `LContext.SetServerName(...)`

**Step 2: Run RED**

Run:
`bash tests/scripts/test_mbedtls_online_verification_tests_no_context_level_sni_guidance_contract.sh`

Expected:
- FAIL on the current stale context-level SNI setup

### Task 2: GREEN - Update selected MbedTLS online verification tests

**Files:**
- Modify: `tests/mbedtls/test_mbedtls_cert_chain.pas`
- Modify: `tests/mbedtls/test_mbedtls_cert_verify_flags.pas`
- Modify: `tests/mbedtls/test_mbedtls_cert_errors.pas`
- Modify: `tests/mbedtls/test_mbedtls_ocsp_capability.pas`

**Step 1: Replace stale guidance**

- In each online verification flow:
  - create the connection first
  - cast to `ISSLClientConnection`
  - set `ServerName` on the connection before `Connect`
- Preserve all existing verification/error assertions

**Step 2: Re-run the contract**

Run:
`bash tests/scripts/test_mbedtls_online_verification_tests_no_context_level_sni_guidance_contract.sh`

Expected:
- PASS

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_mbedtls_online_verification_tests_no_context_level_sni_guidance_contract.sh`
- Test: `tests/mbedtls/test_mbedtls_cert_chain.pas`
- Test: `tests/mbedtls/test_mbedtls_cert_verify_flags.pas`
- Test: `tests/mbedtls/test_mbedtls_cert_errors.pas`
- Test: `tests/mbedtls/test_mbedtls_ocsp_capability.pas`

**Step 1: Run focused checks**

Run:
- `bash tests/scripts/test_mbedtls_online_verification_tests_no_context_level_sni_guidance_contract.sh`
- `fpc -Fu./src -Fu./examples -otmp/test_mbedtls_cert_chain tests/mbedtls/test_mbedtls_cert_chain.pas`
- `fpc -Fu./src -Fu./examples -otmp/test_mbedtls_cert_verify_flags tests/mbedtls/test_mbedtls_cert_verify_flags.pas`
- `fpc -Fu./src -Fu./examples -otmp/test_mbedtls_cert_errors tests/mbedtls/test_mbedtls_cert_errors.pas`
- `fpc -Fu./src -Fu./examples -otmp/test_mbedtls_ocsp_capability tests/mbedtls/test_mbedtls_ocsp_capability.pas`

Expected:
- contract passes
- selected tests compile on the local Linux harness

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record the classification**

- Note that these files exercise normal MbedTLS online verification flows, not intentional context-level compatibility coverage.
- Note that certificate/OCSP tests should not keep teaching deprecated shared-context hostname setup.

**Step 2: Roll the next queue**

- Continue classifying remaining backend-specific online tests, especially the WinSSL online/performance/session files, with the same rule:
  - normal client flow => move to per-connection SNI
  - explicit compatibility/API-surface coverage => label
