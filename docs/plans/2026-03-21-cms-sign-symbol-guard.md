# CMS Sign Symbol Guard Plan

**Goal:** Make `CMSSignData(...)` preserve its existing `nil` contract when `CMS_sign` is unavailable, instead of dereferencing a nil CMS function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for `CMSSignData(...)`
- change only `src/fafafa.ssl.openssl.api.cms.pas`
- preserve current successful CMS signing behavior when `CMS_sign` is available
- do not redesign CMS verify/encrypt/decrypt helpers

## Task 1: RED - Reproduce the CMS sign symbol gap

**Files:**
- Add: `tests/test_cms_sign_symbol_contract.pas`
- Reference: `src/fafafa.ssl.openssl.api.cms.pas`

**Steps:**
- Write a focused contract test that:
  - loads OpenSSL core, BIO, and CMS support on the current runtime
  - temporarily clears `CMS_sign`
  - asserts `CMSSignData(...)` does not raise and returns `nil`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal CMS sign symbol guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.cms.pas`

**Steps:**
- Add an early-return guard so `CMSSignData(...)` requires:
  - `CMS_sign`
- Keep successful CMS signing behavior unchanged when that helper is available.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cms_sign_symbol_contract && fpc -B -Fu./src -FUtmp/cms_sign_symbol_contract -FEtmp/cms_sign_symbol_contract -otmp/cms_sign_symbol_contract/test_cms_sign_symbol_contract tests/test_cms_sign_symbol_contract.pas && ./tmp/cms_sign_symbol_contract/test_cms_sign_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-cms-sign-symbol-guard.md src/fafafa.ssl.openssl.api.cms.pas tests/test_cms_sign_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused CMS sign helper contract passes without raising
- `CMSSignData(...)` degrades to `nil` when `CMS_sign` is unavailable
