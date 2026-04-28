# PKCS7 Verify Symbol Guard Plan

**Goal:** Make `VerifyPKCS7SignedData(...)` preserve its existing `False` contract when `PKCS7_verify` is unavailable, instead of dereferencing a nil PKCS7 function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for `VerifyPKCS7SignedData(...)`
- change only `src/fafafa.ssl.openssl.api.pkcs.pas`
- preserve current successful PKCS7 verify behavior when `PKCS7_verify` is available
- do not redesign PKCS7 sign flow, PKCS12 parsing, or CMS helpers

## Task 1: RED - Reproduce the PKCS7 verify symbol gap

**Files:**
- Add: `tests/test_pkcs7_verify_symbol_contract.pas`
- Reference: `src/fafafa.ssl.openssl.api.pkcs.pas`

**Steps:**
- Write a focused contract test that:
  - loads OpenSSL core, BIO, and PKCS support on the current runtime
  - temporarily clears `PKCS7_verify`
  - asserts `VerifyPKCS7SignedData(...)` does not raise and returns `False`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal PKCS7 verify symbol guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.pkcs.pas`

**Steps:**
- Add an early-return guard so `VerifyPKCS7SignedData(...)` requires:
  - `PKCS7_verify`
- Keep successful PKCS7 verification behavior unchanged when that helper is available.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/pkcs7_verify_symbol_contract && fpc -B -Fu./src -FUtmp/pkcs7_verify_symbol_contract -FEtmp/pkcs7_verify_symbol_contract -otmp/pkcs7_verify_symbol_contract/test_pkcs7_verify_symbol_contract tests/test_pkcs7_verify_symbol_contract.pas && ./tmp/pkcs7_verify_symbol_contract/test_pkcs7_verify_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-pkcs7-verify-symbol-guard.md src/fafafa.ssl.openssl.api.pkcs.pas tests/test_pkcs7_verify_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused PKCS7 verify helper contract passes without raising
- `VerifyPKCS7SignedData(...)` degrades to `False` when `PKCS7_verify` is unavailable
