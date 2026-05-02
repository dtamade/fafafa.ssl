# WinSSL Cross-Target Compile Drift Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Restore Win64 cross-compilation for the selected WinSSL online-flow tests after their SNI guidance cleanup, by fixing the shared compile drift in `winssl.connection`, `winssl.context`, and `winssl.lib`.

**Architecture:** Treat this as a shared compile-surface repair, not a behavior redesign. Start from the focused Win64 test compile that now reaches the WinSSL implementation units, fix only the concrete compile blockers, and re-run the same targeted test compiles to confirm the chain is green again.

**Tech Stack:** Free Pascal, Win64 cross-compile verification

---

### Task 1: Confirm the post-shim RED state

**Files:**
- Reference: `tests/winssl/test_winssl_alpn_sni.pas`

**Step 1: Reproduce the shared compile blockers**

Run:
`fpc -Twin64 -Fu./src -otmp/test_winssl_alpn_sni.exe tests/winssl/test_winssl_alpn_sni.pas`

Expected:
- FAIL in shared WinSSL units with errors around:
  - `GetVerifyCallback`
  - `TWinSSLLibrary`
  - `AcceptSecurityContext`
  - `TryGetNativeHandle(..., nil)`
  - `RaiseSSLInitError`
  - misplaced constructor-body initialization

### Task 2: GREEN - Fix only the compile blockers

**Files:**
- Modify: `src/fafafa.ssl.winssl.connection.pas`
- Modify: `src/fafafa.ssl.winssl.context.pas`
- Modify: `src/fafafa.ssl.winssl.lib.pas`

**Step 1: Apply minimal compile-surface repairs**

- In `winssl.connection`:
  - store `GetVerifyCallback` into a local callback variable before invoking it
  - import `TWinSSLLibrary` in implementation scope
  - use `AcceptSecurityContextW`
- In `winssl.context`:
  - replace `TryGetNativeHandle(AStore, nil)` with a real local out variable
- In `winssl.lib`:
  - move misplaced capability-cache initialization back inside the constructor body
  - replace undefined `RaiseSSLInitError(...)` helpers with direct `ESSLInitError` raises

### Task 3: Focused verification

**Files:**
- Test: `tests/winssl/test_winssl_alpn_sni.pas`
- Test: `tests/winssl/test_winssl_hostname_mismatch_online.pas`
- Test: `tests/winssl/test_winssl_session_resumption.pas`

**Step 1: Re-run focused Win64 compiles**

Run:
- `fpc -Twin64 -Fu./src -otmp/test_winssl_alpn_sni.exe tests/winssl/test_winssl_alpn_sni.pas`
- `fpc -Twin64 -Fu./src -otmp/test_winssl_hostname_mismatch_online.exe tests/winssl/test_winssl_hostname_mismatch_online.pas`
- `fpc -Twin64 -Fu./src -otmp/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`

Expected:
- all selected WinSSL test binaries cross-compile successfully
