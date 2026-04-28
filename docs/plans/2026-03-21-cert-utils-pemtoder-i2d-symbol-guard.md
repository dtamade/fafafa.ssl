# Certificate Utils PEMToDER i2d Symbol Guard Plan

**Goal:** Make `TCertificateUtils.PEMToDER(...)` preserve its existing empty-bytes contract when `i2d_X509` is unavailable, instead of dereferencing a nil X509 encode function pointer.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test for `TCertificateUtils.PEMToDER(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve current successful PEM-to-DER conversion behavior when `i2d_X509` is available
- do not redesign `DERToPEM(...)`, fingerprint helpers, generation helpers, or broader PEM/BIO loading logic

## Task 1: RED - Reproduce the PEMToDER encode symbol gap

**Files:**
- Add: `tests/test_cert_utils_pemtoder_i2d_symbol_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `tests/certificate/test_certs/signer_cert.pem`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL on the current runtime
  - loads a valid certificate PEM fixture and warms a normal `PEMToDER(...)` path
  - temporarily clears `i2d_X509`
  - asserts `TCertificateUtils.PEMToDER(...)` must not raise and must return empty bytes
  - asserts `TCertificateUtils.TryPEMToDER(...)` must not raise, must return `False`, and must clear its output
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal PEMToDER encode guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local `Assigned(i2d_X509)` guard inside `TCertificateUtils.PEMToDER(...)` after PEM parsing succeeds and before the DER encode call
- Preserve current behavior:
  - empty input still returns empty bytes
  - missing PEM/BIO read helpers still return empty bytes through the existing guard path
  - `TryPEMToDER(...)` remains non-throwing and returns `False`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_pemtoder_i2d_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_pemtoder_i2d_symbol_contract -FEtmp/cert_utils_pemtoder_i2d_symbol_contract -otmp/cert_utils_pemtoder_i2d_symbol_contract/test_cert_utils_pemtoder_i2d_symbol_contract tests/test_cert_utils_pemtoder_i2d_symbol_contract.pas && ./tmp/cert_utils_pemtoder_i2d_symbol_contract/test_cert_utils_pemtoder_i2d_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-cert-utils-pemtoder-i2d-symbol-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_pemtoder_i2d_symbol_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- focused `PEMToDER(...)` contract passes without `EAccessViolation`
- `PEMToDER(...)` safely returns empty bytes when `i2d_X509` is unavailable
- `TryPEMToDER(...)` stays non-throwing and returns `False`
