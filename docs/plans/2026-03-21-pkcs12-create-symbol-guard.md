# PKCS12 Create Symbol Guard Plan

**Goal:** Make `SavePKCS12ToFile(...)` preserve its existing `False` contract when `PKCS12_create` is unavailable, instead of dereferencing a nil PKCS12 function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for `SavePKCS12ToFile(...)`
- change only `src/fafafa.ssl.openssl.api.pkcs.pas`
- preserve current failed-save behavior when `PKCS12_create` is unavailable
- do not redesign `i2d_PKCS12_bio`, PKCS12 load helpers, PKCS7 helpers, or CMS helpers

## Task 1: RED - Reproduce the PKCS12 create symbol gap

**Files:**
- Add: `tests/test_pkcs12_create_symbol_contract.pas`
- Reference: `src/fafafa.ssl.openssl.api.pkcs.pas`

**Steps:**
- Write a focused contract test that:
  - loads OpenSSL core, BIO, and PKCS support on the current runtime
  - temporarily clears `PKCS12_create`
  - calls `SavePKCS12ToFile(...)` with non-nil key/cert pointers so this batch stays isolated at the create step
  - asserts `SavePKCS12ToFile(...)` does not raise and returns `False`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal PKCS12 create symbol guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.pkcs.pas`

**Steps:**
- Add an early-return guard so `SavePKCS12ToFile(...)` requires:
  - `PKCS12_create`
- Keep current save-failure behavior unchanged when that helper is unavailable.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/pkcs12_create_symbol_contract && fpc -B -Fu./src -FUtmp/pkcs12_create_symbol_contract -FEtmp/pkcs12_create_symbol_contract -otmp/pkcs12_create_symbol_contract/test_pkcs12_create_symbol_contract tests/test_pkcs12_create_symbol_contract.pas && ./tmp/pkcs12_create_symbol_contract/test_pkcs12_create_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-pkcs12-create-symbol-guard.md src/fafafa.ssl.openssl.api.pkcs.pas tests/test_pkcs12_create_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused PKCS12 save helper contract passes without raising
- `SavePKCS12ToFile(...)` degrades to `False` when `PKCS12_create` is unavailable
