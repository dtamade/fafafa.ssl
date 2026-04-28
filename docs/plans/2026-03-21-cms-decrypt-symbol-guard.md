# CMS Decrypt Symbol Guard Plan

**Goal:** Make `CMSDecryptData(...)` preserve its existing empty-bytes contract when `CMS_decrypt` is unavailable, instead of dereferencing a nil CMS function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for `CMSDecryptData(...)`
- change only `src/fafafa.ssl.openssl.api.cms.pas`
- preserve current successful CMS decrypt behavior when `CMS_decrypt` is available
- do not redesign CMS sign/verify/encrypt helpers

## Task 1: RED - Reproduce the CMS decrypt symbol gap

**Files:**
- Add: `tests/test_cms_decrypt_symbol_contract.pas`
- Reference: `src/fafafa.ssl.openssl.api.cms.pas`

**Steps:**
- Write a focused contract test that:
  - loads OpenSSL core, BIO, and CMS support on the current runtime
  - temporarily clears `CMS_decrypt`
  - asserts `CMSDecryptData(...)` does not raise and returns empty bytes
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal CMS decrypt symbol guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.cms.pas`

**Steps:**
- Add an early-return guard so `CMSDecryptData(...)` requires:
  - `CMS_decrypt`
- Keep successful CMS decryption behavior unchanged when that helper is available.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cms_decrypt_symbol_contract && fpc -B -Fu./src -FUtmp/cms_decrypt_symbol_contract -FEtmp/cms_decrypt_symbol_contract -otmp/cms_decrypt_symbol_contract/test_cms_decrypt_symbol_contract tests/test_cms_decrypt_symbol_contract.pas && ./tmp/cms_decrypt_symbol_contract/test_cms_decrypt_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-cms-decrypt-symbol-guard.md src/fafafa.ssl.openssl.api.cms.pas tests/test_cms_decrypt_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused CMS decrypt helper contract passes without raising
- `CMSDecryptData(...)` degrades to empty bytes when `CMS_decrypt` is unavailable
