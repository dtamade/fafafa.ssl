# Certificate Utils DERToPEM Export Delayed-Loss Family Plan

**Goal:** Close the remaining delayed-loss family inside `TCertificateUtils.DERToPEM(...)` after `d2i_X509(...)` has already succeeded, so export-stage helper loss degrades back to the existing empty-string contract instead of crashing on nil helper dereferences.

**Architecture:** Keep this batch narrow:

- add one focused family-level contract test around `DERToPEM(...)` / `TryDERToPEM(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve the current successful DER-to-PEM output when helpers remain available
- close only the remaining reachable delayed-loss helpers inside the export block before PEM output is materialized:
  - `BIO_s_mem()` after decode success and before the export constructor
  - `BIO_new(...)` after `BIO_s_mem()` succeeds
  - `PEM_write_bio_X509(...)` after the export BIO constructor succeeds
- do not redesign `PEMToDER(...)`, post-success cleanup families, fingerprint helpers, generation helpers, or broader PEM/BIO entry guards

## Task 1: RED - Reproduce the export delayed-loss family gaps

**Files:**
- Add: `tests/test_cert_utils_dertopem_export_delayed_loss_family_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `tests/test_cert_utils_conversion_bio_contract.pas`
- Reference: `tests/test_cert_utils_conversion_post_success_cleanup_family_contract.pas`

**Steps:**
- Write one focused family-level contract test that:
  - initializes OpenSSL and loads `Core/BIO/X509/PEM`
  - loads a real PEM certificate fixture and warms a normal `DERToPEM(...)` path by first producing valid DER bytes
  - uses delayed-loss wrappers so export helpers disappear only after decode success or a prior local export step:
    - clear `BIO_s_mem` from a `d2i_X509(...)` wrapper after decode succeeds
    - clear `BIO_new` from a `BIO_s_mem()` wrapper after the export method is acquired
    - clear `PEM_write_bio_X509` from a `BIO_new(...)` wrapper after the export BIO constructor succeeds
  - asserts direct `DERToPEM(...)` must not raise and must return an empty string
  - asserts `TryDERToPEM(...)` must not raise, must return `False`, and must clear its output
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal export delayed-loss guards

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Harden only the actual delayed-loss dereference sites inside `DERToPEM(...)`:
  - require `Assigned(BIO_s_mem)` before `BIO_s_mem()`
  - split `LBIO := BIO_new(BIO_s_mem())` through `LBIOMethod := BIO_s_mem()` and re-check `Assigned(BIO_new)` immediately before `BIO_new(LBIOMethod)`
  - require `Assigned(PEM_write_bio_X509)` immediately before `PEM_write_bio_X509(LBIO, LCert)`
- Preserve existing contracts:
  - `DERToPEM(...)` remains non-throwing and returns an empty string when export helpers disappear before PEM output is materialized
  - `TryDERToPEM(...)` remains non-throwing, returns `False`, and clears its output
  - the already-closed post-success cleanup family stays untouched

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_dertopem_export_delayed_loss_family_contract && fpc -B -Fu./src -FUtmp/cert_utils_dertopem_export_delayed_loss_family_contract -FEtmp/cert_utils_dertopem_export_delayed_loss_family_contract -otmp/cert_utils_dertopem_export_delayed_loss_family_contract/test_cert_utils_dertopem_export_delayed_loss_family_contract tests/test_cert_utils_dertopem_export_delayed_loss_family_contract.pas && ./tmp/cert_utils_dertopem_export_delayed_loss_family_contract/test_cert_utils_dertopem_export_delayed_loss_family_contract`
- `mkdir -p tmp/cert_utils_conversion_bio_contract && fpc -B -Fu./src -FUtmp/cert_utils_conversion_bio_contract -FEtmp/cert_utils_conversion_bio_contract -otmp/cert_utils_conversion_bio_contract/test_cert_utils_conversion_bio_contract tests/test_cert_utils_conversion_bio_contract.pas && ./tmp/cert_utils_conversion_bio_contract/test_cert_utils_conversion_bio_contract`
- `mkdir -p tmp/cert_utils_conversion_post_success_cleanup_family_contract && fpc -B -Fu./src -FUtmp/cert_utils_conversion_post_success_cleanup_family_contract -FEtmp/cert_utils_conversion_post_success_cleanup_family_contract -otmp/cert_utils_conversion_post_success_cleanup_family_contract/test_cert_utils_conversion_post_success_cleanup_family_contract tests/test_cert_utils_conversion_post_success_cleanup_family_contract.pas && ./tmp/cert_utils_conversion_post_success_cleanup_family_contract/test_cert_utils_conversion_post_success_cleanup_family_contract`
- `mkdir -p tmp/cert_utils_dertopem_d2i_symbol_contract && fpc -B -Fu./src -FUtmp/cert_utils_dertopem_d2i_symbol_contract -FEtmp/cert_utils_dertopem_d2i_symbol_contract -otmp/cert_utils_dertopem_d2i_symbol_contract/test_cert_utils_dertopem_d2i_symbol_contract tests/test_cert_utils_dertopem_d2i_symbol_contract.pas && ./tmp/cert_utils_dertopem_d2i_symbol_contract/test_cert_utils_dertopem_d2i_symbol_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-24-cert-utils-dertopem-export-delayed-loss-family.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_dertopem_export_delayed_loss_family_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- the focused DER-to-PEM export family contract passes without raw `EAccessViolation`
- `DERToPEM(...)` degrades to an empty string across the targeted delayed-loss scenarios
- `TryDERToPEM(...)` remains non-throwing, returns `False`, and clears its output
- the earlier conversion BIO guard, post-success cleanup family, and decode-stage `d2i_X509` contract still pass
