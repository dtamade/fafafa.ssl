# PKCS12 Parse Symbol Guard Plan

**Goal:** Make `LoadPKCS12FromFile(...)` preserve its existing `False` + nil-outputs contract when `PKCS12_parse` is unavailable, instead of dereferencing a nil PKCS12 function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for `LoadPKCS12FromFile(...)`
- change only `src/fafafa.ssl.openssl.api.pkcs.pas`
- preserve current failed-load behavior when `PKCS12_parse` is unavailable
- do not redesign `d2i_PKCS12_bio`, PKCS7 helpers, or CMS helpers

## Task 1: RED - Reproduce the PKCS12 parse symbol gap

**Files:**
- Add: `tests/test_pkcs12_parse_symbol_contract.pas`
- Reference: `src/fafafa.ssl.openssl.api.pkcs.pas`
- Reference: `tests/certificate/test_certs/signer_cert.pem`
- Reference: `tests/certificate/test_certs/signer_key.pem`

**Steps:**
- Write a focused contract test that:
  - loads OpenSSL core, BIO, PEM, X509, EVP, and PKCS support on the current runtime
  - loads a real certificate/private-key fixture from repository test certs
  - creates a valid temporary PKCS12 file via `SavePKCS12ToFile(...)`
  - temporarily clears `PKCS12_parse`
  - asserts `LoadPKCS12FromFile(...)` does not raise, returns `False`, and keeps outputs `nil`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal PKCS12 parse symbol guard

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.pkcs.pas`

**Steps:**
- Add an early-return guard so `LoadPKCS12FromFile(...)` requires:
  - `PKCS12_parse`
- Keep current parse-failure behavior unchanged when that helper is unavailable.

## Task 3: Verification

**Run:**
- `mkdir -p tmp/pkcs12_parse_symbol_contract && fpc -B -Fu./src -FUtmp/pkcs12_parse_symbol_contract -FEtmp/pkcs12_parse_symbol_contract -otmp/pkcs12_parse_symbol_contract/test_pkcs12_parse_symbol_contract tests/test_pkcs12_parse_symbol_contract.pas && ./tmp/pkcs12_parse_symbol_contract/test_pkcs12_parse_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-pkcs12-parse-symbol-guard.md src/fafafa.ssl.openssl.api.pkcs.pas tests/test_pkcs12_parse_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused PKCS12 parse helper contract passes without raising
- `LoadPKCS12FromFile(...)` degrades to `False` with nil outputs when `PKCS12_parse` is unavailable
