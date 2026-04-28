# CMS Encrypt Symbol Guard Plan

**Goal:** Make `CMSEncryptData(...)` preserve its existing `nil` contract when `CMS_encrypt` is unavailable, instead of dereferencing a nil CMS function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for `CMSEncryptData(...)`
- change only `src/fafafa.ssl.openssl.api.cms.pas`
- preserve current successful CMS encrypt behavior when `CMS_encrypt` is available
- do not redesign CMS sign/verify/decrypt helpers

## Task 1: RED - Reproduce the CMS encrypt symbol gap

**Files:**
- Add: `tests/test_cms_encrypt_symbol_contract.pas`
- Reference: `src/fafafa.ssl.openssl.api.cms.pas`

**Steps:**
- Write a focused contract test that:
  - loads OpenSSL core, BIO, and CMS support on the current runtime
  - temporarily clears `CMS_encrypt`
  - calls `CMSEncryptData(...)` with a non-nil cipher to keep this batch isolated from default `EVP_aes_256_cbc()` lookup
  - asserts `CMSEncryptData(...)` does not raise and returns `nil`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal CMS encrypt symbol guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.cms.pas`

**Steps:**
- Add an early-return guard so `CMSEncryptData(...)` requires:
  - `CMS_encrypt`
- Keep successful CMS encryption behavior unchanged when that helper is available.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cms_encrypt_symbol_contract && fpc -B -Fu./src -FUtmp/cms_encrypt_symbol_contract -FEtmp/cms_encrypt_symbol_contract -otmp/cms_encrypt_symbol_contract/test_cms_encrypt_symbol_contract tests/test_cms_encrypt_symbol_contract.pas && ./tmp/cms_encrypt_symbol_contract/test_cms_encrypt_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-cms-encrypt-symbol-guard.md src/fafafa.ssl.openssl.api.cms.pas tests/test_cms_encrypt_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused CMS encrypt helper contract passes without raising
- `CMSEncryptData(...)` degrades to `nil` when `CMS_encrypt` is unavailable
