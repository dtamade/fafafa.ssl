# PEM Certificate Symbol Guard Plan

**Goal:** Make the PEM certificate helper entrypoints preserve their existing `nil` / `False` contracts when `PEM_read_bio_X509` or `PEM_write_bio_X509` is unavailable, instead of dereferencing nil PEM function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for PEM certificate-only file/memory/save helpers
- change only `src/fafafa.ssl.openssl.api.pem.pas`
- preserve current successful PEM certificate behavior when helpers are available
- do not redesign PEM loading, key helpers, or global loader behavior

## Task 1: RED - Reproduce the certificate-symbol helper gap

**Files:**
- Add: `tests/test_pem_certificate_symbol_contract.pas`
- Reference: `src/fafafa.ssl.openssl.api.pem.pas`

**Steps:**
- Write a focused contract test that:
  - loads the PEM module on the current runtime
  - temporarily clears `PEM_read_bio_X509` and asserts:
    - `LoadCertificateFromPEM(...)` does not raise and returns `nil`
    - `LoadCertificateFromMemory(...)` does not raise and returns `nil`
  - temporarily clears `PEM_write_bio_X509` and asserts:
    - `SaveCertificateToPEM(...)` does not raise and returns `False`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal certificate-symbol guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.pem.pas`

**Steps:**
- Add early-return guards so PEM certificate read helpers require:
  - `PEM_read_bio_X509`
- Add an early-return guard so PEM certificate save helpers require:
  - `PEM_write_bio_X509`
- Keep successful certificate parsing/writing behavior unchanged when those helpers are available.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/pem_certificate_symbol_contract && fpc -B -Fu./src -FUtmp/pem_certificate_symbol_contract -FEtmp/pem_certificate_symbol_contract -otmp/pem_certificate_symbol_contract/test_pem_certificate_symbol_contract tests/test_pem_certificate_symbol_contract.pas && ./tmp/pem_certificate_symbol_contract/test_pem_certificate_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-pem-certificate-symbol-guard.md src/fafafa.ssl.openssl.api.pem.pas tests/test_pem_certificate_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused PEM certificate helper contract passes without raising
- helper entrypoints degrade to `nil` / `False` when PEM certificate symbols are unavailable
