# PKCS12 d2i Symbol Guard Plan

**Goal:** Make `LoadPKCS12FromFile(...)` preserve its existing `False` contract when `d2i_PKCS12_bio` is unavailable, instead of dereferencing a nil PKCS12 function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for `LoadPKCS12FromFile(...)`
- change only `src/fafafa.ssl.openssl.api.pkcs.pas`
- preserve current failed-load behavior when `d2i_PKCS12_bio` is unavailable
- do not redesign `PKCS12_parse`, PKCS7 helpers, or CMS helpers

## Task 1: RED - Reproduce the PKCS12 decode symbol gap

**Files:**
- Add: `tests/test_pkcs12_d2i_symbol_contract.pas`
- Reference: `src/fafafa.ssl.openssl.api.pkcs.pas`

**Steps:**
- Write a focused contract test that:
  - loads OpenSSL core, BIO, and PKCS support on the current runtime
  - prepares a temporary PKCS12 input file
  - temporarily clears `d2i_PKCS12_bio`
  - asserts `LoadPKCS12FromFile(...)` does not raise, returns `False`, and keeps outputs `nil`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal PKCS12 decode symbol guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.pkcs.pas`

**Steps:**
- Add an early-return guard so `LoadPKCS12FromFile(...)` requires:
  - `d2i_PKCS12_bio`
- Keep current load-failure behavior unchanged when that helper is unavailable.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/pkcs12_d2i_symbol_contract && fpc -B -Fu./src -FUtmp/pkcs12_d2i_symbol_contract -FEtmp/pkcs12_d2i_symbol_contract -otmp/pkcs12_d2i_symbol_contract/test_pkcs12_d2i_symbol_contract tests/test_pkcs12_d2i_symbol_contract.pas && ./tmp/pkcs12_d2i_symbol_contract/test_pkcs12_d2i_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-pkcs12-d2i-symbol-guard.md src/fafafa.ssl.openssl.api.pkcs.pas tests/test_pkcs12_d2i_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused PKCS12 decode helper contract passes without raising
- `LoadPKCS12FromFile(...)` degrades to `False` with nil outputs when `d2i_PKCS12_bio` is unavailable
