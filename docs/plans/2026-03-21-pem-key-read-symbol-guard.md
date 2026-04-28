# PEM Key Read Symbol Guard Plan

**Goal:** Make the PEM key read helper entrypoints preserve their existing `nil` contracts when `PEM_read_bio_PrivateKey` or `PEM_read_bio_PUBKEY` is unavailable, instead of dereferencing nil PEM function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for PEM key read helpers
- change only `src/fafafa.ssl.openssl.api.pem.pas`
- preserve current successful PEM key read behavior when helpers are available
- do not redesign PEM certificate helpers, key save helpers, or global loader behavior

## Task 1: RED - Reproduce the key-read symbol helper gap

**Files:**
- Add: `tests/test_pem_key_read_symbol_contract.pas`
- Reference: `src/fafafa.ssl.openssl.api.pem.pas`

**Steps:**
- Write a focused contract test that:
  - loads the PEM module on the current runtime
  - temporarily clears `PEM_read_bio_PrivateKey` and asserts:
    - `LoadPrivateKeyFromPEM(...)` does not raise and returns `nil`
    - `LoadPrivateKeyFromMemory(...)` does not raise and returns `nil`
  - temporarily clears `PEM_read_bio_PUBKEY` and asserts:
    - `LoadPublicKeyFromPEM(...)` does not raise and returns `nil`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal key-read symbol guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.pem.pas`

**Steps:**
- Add early-return guards so PEM private-key read helpers require:
  - `PEM_read_bio_PrivateKey`
- Add an early-return guard so PEM public-key read helper requires:
  - `PEM_read_bio_PUBKEY`
- Keep successful key parsing behavior unchanged when those helpers are available.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/pem_key_read_symbol_contract && fpc -B -Fu./src -FUtmp/pem_key_read_symbol_contract -FEtmp/pem_key_read_symbol_contract -otmp/pem_key_read_symbol_contract/test_pem_key_read_symbol_contract tests/test_pem_key_read_symbol_contract.pas && ./tmp/pem_key_read_symbol_contract/test_pem_key_read_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-pem-key-read-symbol-guard.md src/fafafa.ssl.openssl.api.pem.pas tests/test_pem_key_read_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused PEM key read helper contract passes without raising
- helper entrypoints degrade to `nil` when PEM key read symbols are unavailable
