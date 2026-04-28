# Certificate Utils VerifyChain Bundled Intermediate Cleanup Family Plan

**Goal:** Close the remaining bundled-intermediate parse/export cleanup family inside `TCertificateUtils.VerifyChain(...)` so helper loss keeps the existing boolean-return contract instead of dereferencing nil OpenSSL cleanup or constructor helpers.

**Architecture:** Keep this batch narrow:

- add one focused family-level contract test around `VerifyChain(...)` and `TryVerifyChain(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve the existing bundled `leaf + intermediate` success path when helpers remain available
- close only the remaining reachable delayed-loss helpers inside the bundled-intermediate extraction/export loop:
  - skip-leaf cleanup `X509_free(LX509)`
  - intermediate export constructor `BIO_new(...)` after `BIO_s_mem()` succeeds
  - loop cleanup `X509_free(LX509)` after intermediate-export `BIO_free(LOutBIO)`
- do not redesign `fafafa.ssl.certchain`, CA store loading, `GetInfo(...)`, or broader certificate parsing logic

## Task 1: RED - Reproduce the bundled-intermediate cleanup family gaps

**Files:**
- Add: `tests/test_cert_utils_verify_chain_bundled_intermediate_cleanup_family_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `tests/test_cert_utils_verify_chain_bio_contract.pas`

**Steps:**
- Write one focused family-level contract test that:
  - initializes OpenSSL and loads `Core/BIO/X509/PEM/EVP`
  - generates a root CA, an intermediate CA, and a leaf certificate
  - verifies a normal `VerifyChain(leaf + intermediate, root)` success path before stubbing helpers
  - uses delayed-loss wrappers so helper loss happens only after the previous local step succeeds:
    - clear `X509_free` immediately after the first successful `PEM_read_bio_X509(...)` return to expose the skip-leaf cleanup dereference
    - clear `BIO_new` immediately after `BIO_s_mem()` returns to expose the intermediate export constructor dereference
    - track the intermediate export BIO via a `BIO_new(...)` wrapper, then clear `X509_free` when that tracked export BIO is freed to expose the later loop cleanup dereference
  - asserts direct `VerifyChain(...)` must not raise and must return `False`
  - asserts `TryVerifyChain(...)` must not raise, must return `True`, and must set `AIsValid=False`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal bundled-intermediate cleanup guards

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Harden `TCertificateUtils.VerifyChain(...)` only at the actual delayed-loss dereference points:
  - guard skip-leaf `X509_free(LX509)` before dereference
  - split `LOutBIO := BIO_new(BIO_s_mem())` through `LBIOMethod := BIO_s_mem()` and re-check `Assigned(BIO_new)` immediately before `BIO_new(LBIOMethod)`
  - guard loop cleanup `X509_free(LX509)` before dereference
- Preserve existing contracts:
  - direct `VerifyChain(...)` must not raise and must return `False` on helper loss
  - `TryVerifyChain(...)` remains non-throwing, returns `True`, and reports `AIsValid=False`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_verify_chain_bundled_intermediate_cleanup_family_contract && fpc -B -Fu./src -FUtmp/cert_utils_verify_chain_bundled_intermediate_cleanup_family_contract -FEtmp/cert_utils_verify_chain_bundled_intermediate_cleanup_family_contract -otmp/cert_utils_verify_chain_bundled_intermediate_cleanup_family_contract/test_cert_utils_verify_chain_bundled_intermediate_cleanup_family_contract tests/test_cert_utils_verify_chain_bundled_intermediate_cleanup_family_contract.pas && ./tmp/cert_utils_verify_chain_bundled_intermediate_cleanup_family_contract/test_cert_utils_verify_chain_bundled_intermediate_cleanup_family_contract`
- `mkdir -p tmp/cert_utils_verify_chain_bio_contract && fpc -B -Fu./src -FUtmp/cert_utils_verify_chain_bio_contract -FEtmp/cert_utils_verify_chain_bio_contract -otmp/cert_utils_verify_chain_bio_contract/test_cert_utils_verify_chain_bio_contract tests/test_cert_utils_verify_chain_bio_contract.pas && ./tmp/cert_utils_verify_chain_bio_contract/test_cert_utils_verify_chain_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-24-cert-utils-verify-chain-bundled-intermediate-cleanup-family.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_verify_chain_bundled_intermediate_cleanup_family_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- the focused family-level contract passes without raw `EAccessViolation`
- direct `VerifyChain(...)` degrades to `False` for each bundled-intermediate cleanup family scenario
- `TryVerifyChain(...)` remains non-throwing, returns `True`, and reports `AIsValid=False`
- the earlier verify-chain BIO contract still passes
- full module compile remains green
