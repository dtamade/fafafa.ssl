# WinSSL Secure Compare Shim Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Restore WinSSL certstore compilation by adding the missing `fafafa.ssl.secure.compare` unit that `src/fafafa.ssl.winssl.certstore.pas` already references.

**Architecture:** Treat this as a narrow compile-drift fix, not a behavioral redesign. Add a minimal standalone compare shim that depends only on `fafafa.ssl.crypto.constant_time`, exposing the compare helpers needed by WinSSL certstore without pulling in the heavier OpenSSL-backed `fafafa.ssl.secure` unit.

**Tech Stack:** Free Pascal unit shim, focused Win64 cross-compile verification

---

### Task 1: Confirm RED compile failure

**Files:**
- Reference: `src/fafafa.ssl.winssl.certstore.pas`
- Reference: `tests/winssl/test_winssl_alpn_sni.pas`

**Step 1: Reproduce the missing-unit failure**

Run:
`fpc -Twin64 -Fu./src -otmp/test_winssl_alpn_sni.exe tests/winssl/test_winssl_alpn_sni.pas`

Expected:
- FAIL with `Can't find unit fafafa.ssl.secure.compare`

### Task 2: GREEN - Add minimal compare shim

**Files:**
- Add: `src/fafafa.ssl.secure.compare.pas`

**Step 1: Implement the shim**

- Expose:
  - `SecureCompare(const A, B: TBytes): Boolean`
  - `SecureCompareStrings(const A, B: string): Boolean`
- Delegate to `fafafa.ssl.crypto.constant_time.TConstantTime`
- Keep the new unit independent of OpenSSL-backed secure storage code

### Task 3: Focused verification

**Files:**
- Test: `tests/winssl/test_winssl_alpn_sni.pas`

**Step 1: Re-run focused Win64 compile**

Run:
`fpc -Twin64 -Fu./src -otmp/test_winssl_alpn_sni.exe tests/winssl/test_winssl_alpn_sni.pas`

Expected:
- the missing-unit failure is gone
- if another compile issue appears, capture it separately as the next independent drift
