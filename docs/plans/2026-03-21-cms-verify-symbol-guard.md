# CMS Verify Symbol Guard Plan

**Goal:** Make `CMSVerifySignature(...)` preserve its existing `False` contract when `CMS_verify` is unavailable, instead of dereferencing a nil CMS function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for `CMSVerifySignature(...)`
- change only `src/fafafa.ssl.openssl.api.cms.pas`
- preserve current successful CMS verify behavior when `CMS_verify` is available
- do not redesign CMS sign/encrypt/decrypt helpers

## Task 1: RED - Reproduce the CMS verify symbol gap

**Files:**
- Add: `tests/test_cms_verify_symbol_contract.pas`
- Reference: `src/fafafa.ssl.openssl.api.cms.pas`

**Steps:**
- Write a focused contract test that:
  - loads OpenSSL core, BIO, and CMS support on the current runtime
  - temporarily clears `CMS_verify`
  - asserts `CMSVerifySignature(...)` does not raise and returns `False`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal CMS verify symbol guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.cms.pas`

**Steps:**
- Add an early-return guard so `CMSVerifySignature(...)` requires:
  - `CMS_verify`
- Keep successful CMS verification behavior unchanged when that helper is available.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cms_verify_symbol_contract && fpc -B -Fu./src -FUtmp/cms_verify_symbol_contract -FEtmp/cms_verify_symbol_contract -otmp/cms_verify_symbol_contract/test_cms_verify_symbol_contract tests/test_cms_verify_symbol_contract.pas && ./tmp/cms_verify_symbol_contract/test_cms_verify_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-cms-verify-symbol-guard.md src/fafafa.ssl.openssl.api.cms.pas tests/test_cms_verify_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused CMS verify helper contract passes without raising
- `CMSVerifySignature(...)` degrades to `False` when `CMS_verify` is unavailable
