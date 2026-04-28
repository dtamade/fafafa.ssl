# Certificate Builder BIO Guard Plan

**Goal:** Make `src/fafafa.ssl.cert.builder.impl.pas` convenience certificate/private-key conversion helpers fail with controlled `ESSLException`s when required BIO helpers are unavailable, instead of dereferencing nil function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around certificate/private-key handle <-> PEM conversion helpers
- change only `src/fafafa.ssl.cert.builder.impl.pas`
- preserve current successful builder behavior when helper capabilities are available
- do not redesign builder architecture, `TCertificateUtils`, or the OpenSSL loader

## Task 1: RED - Reproduce the helper gap

**Files:**
- Add: `tests/test_cert_builder_bio_contract.pas`
- Reference: `src/fafafa.ssl.cert.builder.impl.pas`
- Reference: `src/fafafa.ssl.cert.pas`
- Reference: `src/fafafa.ssl.openssl.api.bio.pas`
- Reference: `src/fafafa.ssl.openssl.api.x509.pas`
- Reference: `src/fafafa.ssl.openssl.api.evp.pas`
- Reference: `src/fafafa.ssl.openssl.api.pem.pas`

**Steps:**
- Write a focused contract test that:
  - loads OpenSSL core, BIO, X509, EVP, and PEM support on the current runtime
  - reads valid certificate/private-key PEM fixtures from `tests/certificate/test_certs`
  - prepares valid certificate/private-key handles before niling cleanup helpers
  - temporarily clears representative helpers such as:
    - `BIO_new_mem_buf`
    - `BIO_new`
    - `BIO_s_mem`
    - `BIO_free`
  - asserts these convenience paths do not raise `EAccessViolation`:
    - `TCertificateImpl.GetX509Handle`
    - `TCertificateImpl.CreateFromHandle(...)`
    - `TPrivateKeyImpl.GetEVP_PKEYHandle`
    - `TPrivateKeyImpl.CreateFromHandle(...)`
    - `TCertificateImpl.ToPEM`
    - `TPrivateKeyImpl.ToPEM`
  - asserts they fail with controlled `ESSLException` messages instead
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal BIO guard

**Files:**
- Modify: `src/fafafa.ssl.cert.builder.impl.pas`

**Steps:**
- Add local guard checks so handle-from-PEM helpers require:
  - `BIO_new_mem_buf`
  - `BIO_free`
- Add local guard checks so PEM-from-handle helpers require:
  - `BIO_new`
  - `BIO_s_mem`
  - `BIO_free`
- Keep the current exception-based contract:
  - missing capability should raise controlled `ESSLException`
  - successful certificate/private-key conversion behavior should remain unchanged

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_builder_bio_contract && fpc -B -Fu./src -FUtmp/cert_builder_bio_contract -FEtmp/cert_builder_bio_contract -otmp/cert_builder_bio_contract/test_cert_builder_bio_contract tests/test_cert_builder_bio_contract.pas && ./tmp/cert_builder_bio_contract/test_cert_builder_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-20-cert-builder-bio-guard.md src/fafafa.ssl.cert.builder.impl.pas tests/test_cert_builder_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused cert-builder BIO contract passes without `EAccessViolation`
- convenience conversion paths fail with controlled `ESSLException`s when BIO dependencies are unavailable
