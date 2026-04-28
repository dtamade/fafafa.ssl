# PEM Encrypted PrivateKey Cipher Symbol Guard Plan

**Goal:** Make the encrypted private-key branch of `SavePrivateKeyToPEM` preserve its existing `False` contract when `EVP_aes_256_cbc` is unavailable, instead of dereferencing a nil EVP function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for the encrypted private-key save branch
- change only `src/fafafa.ssl.openssl.api.pem.pas`
- preserve current successful encrypted PEM private-key behavior when helpers are available
- do not redesign unencrypted save behavior or broader EVP loading semantics

## Task 1: RED - Reproduce the encrypted-branch cipher helper gap

**Files:**
- Add: `tests/test_pem_encrypted_privatekey_cipher_symbol_contract.pas`
- Reference: `src/fafafa.ssl.openssl.api.pem.pas`
- Reference: `src/fafafa.ssl.openssl.api.evp.pas`

**Steps:**
- Write a focused contract test that:
  - loads OpenSSL core, BIO, PEM, and EVP support on the current runtime
  - temporarily clears `EVP_aes_256_cbc`
  - calls `SavePrivateKeyToPEM(..., 'testpass')`
  - asserts the helper does not raise and returns `False`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal encrypted-branch cipher guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.pem.pas`

**Steps:**
- Add an early-return guard so the encrypted branch of `SavePrivateKeyToPEM` requires:
  - `EVP_aes_256_cbc`
- Keep current successful encrypted PEM writing behavior unchanged when the helper is available.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/pem_encrypted_privatekey_cipher_symbol_contract && fpc -B -Fu./src -FUtmp/pem_encrypted_privatekey_cipher_symbol_contract -FEtmp/pem_encrypted_privatekey_cipher_symbol_contract -otmp/pem_encrypted_privatekey_cipher_symbol_contract/test_pem_encrypted_privatekey_cipher_symbol_contract tests/test_pem_encrypted_privatekey_cipher_symbol_contract.pas && ./tmp/pem_encrypted_privatekey_cipher_symbol_contract/test_pem_encrypted_privatekey_cipher_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-pem-encrypted-privatekey-cipher-symbol-guard.md src/fafafa.ssl.openssl.api.pem.pas tests/test_pem_encrypted_privatekey_cipher_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused encrypted private-key PEM contract passes without raising
- helper entrypoint degrades to `False` when `EVP_aes_256_cbc` is unavailable
