# Certificate Utils DERToPEM d2i Symbol Guard Plan

**Goal:** Make `TCertificateUtils.DERToPEM(...)` preserve its existing empty-string contract when `d2i_X509` is unavailable, instead of dereferencing a nil X509 decode function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for `TCertificateUtils.DERToPEM(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful DER-to-PEM conversion behavior when `d2i_X509` is available
- do not redesign `PEMToDER(...)`, fingerprint helpers, generation helpers, or broader PEM/BIO loading logic

## Task 1: RED - Reproduce the DERToPEM decode symbol gap

**Files:**
- Add: `tests/test_cert_utils_dertopem_d2i_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `tests/certificate/test_certs/signer_cert.pem`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads a valid certificate PEM fixture and warms a normal `DERToPEM(...)` path by first producing valid DER bytes
  - temporarily clears `d2i_X509`
  - asserts `TCertificateUtils.DERToPEM(...)` must not raise and must return an empty string
  - asserts `TCertificateUtils.TryDERToPEM(...)` must not raise, must return `False`, and must clear its output
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal DERToPEM decode guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(d2i_X509)` guard inside `TCertificateUtils.DERToPEM(...)` before the DER decode call
- Preserve current behavior:
  - empty input still returns an empty string
  - missing PEM/BIO write helpers still return an empty string through the existing guard path
  - `TryDERToPEM(...)` remains non-throwing and returns `False`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_dertopem_d2i_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_dertopem_d2i_symbol_contract -FEtmp/cert_utils_dertopem_d2i_symbol_contract -otmp/cert_utils_dertopem_d2i_symbol_contract/test_cert_utils_dertopem_d2i_symbol_contract tests/test_cert_utils_dertopem_d2i_symbol_contract.pas && ./tmp/cert_utils_dertopem_d2i_symbol_contract/test_cert_utils_dertopem_d2i_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-cert-utils-dertopem-d2i-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_dertopem_d2i_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused `DERToPEM(...)` contract passes without `EAccessViolation`
- `DERToPEM(...)` safely returns an empty string when `d2i_X509` is unavailable
- `TryDERToPEM(...)` stays non-throwing and returns `False`
