# PEM Key Save Symbol Guard Plan

**Goal:** Make the PEM key save helper entrypoints preserve their existing `False` contracts when `PEM_write_bio_PrivateKey` or `PEM_write_bio_PUBKEY` is unavailable, instead of dereferencing nil PEM function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for PEM key save helpers
- change only `src/fafafa.ssl.openssl.api.pem.pas`
- preserve current successful PEM key save behavior when helpers are available
- do not redesign PEM certificate helpers, key read helpers, or the encrypted private-key branch

## Task 1: RED - Reproduce the key-save symbol helper gap

**Files:**
- Add: `tests/test_pem_key_save_symbol_contract.pas`
- Reference: `src/fafafa.ssl.openssl.api.pem.pas`

**Steps:**
- Write a focused contract test that:
  - loads the PEM module on the current runtime
  - temporarily clears `PEM_write_bio_PrivateKey` and asserts:
    - `SavePrivateKeyToPEM(..., '')` does not raise and returns `False`
  - temporarily clears `PEM_write_bio_PUBKEY` and asserts:
    - `SavePublicKeyToPEM(...)` does not raise and returns `False`
- Keep the private-key test on the unencrypted branch so this batch stays isolated from `EVP_aes_256_cbc()` semantics.
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal key-save symbol guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.pem.pas`

**Steps:**
- Add an early-return guard so `SavePrivateKeyToPEM(..., '')` requires:
  - `PEM_write_bio_PrivateKey`
- Add an early-return guard so `SavePublicKeyToPEM(...)` requires:
  - `PEM_write_bio_PUBKEY`
- Keep successful key writing behavior unchanged when those helpers are available.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/pem_key_save_symbol_contract && fpc -B -Fu./src -FUtmp/pem_key_save_symbol_contract -FEtmp/pem_key_save_symbol_contract -otmp/pem_key_save_symbol_contract/test_pem_key_save_symbol_contract tests/test_pem_key_save_symbol_contract.pas && ./tmp/pem_key_save_symbol_contract/test_pem_key_save_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-pem-key-save-symbol-guard.md src/fafafa.ssl.openssl.api.pem.pas tests/test_pem_key_save_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused PEM key save helper contract passes without raising
- helper entrypoints degrade to `False` when PEM key save symbols are unavailable
