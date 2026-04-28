# Certificate Utils VerifyChain BIO Guard Plan

**Goal:** Make `TCertificateUtils.VerifyChain(...)` degrade according to its existing boolean-return contract when bundled-chain helper dependencies disappear during intermediate-certificate extraction, instead of dereferencing nil function pointers.

**Architecture:** Keep this batch narrow:

- add one focused program-style contract test around `VerifyChain(...)` / `TryVerifyChain(...)`
- change only `src/fafafa.ssl.cert.utils.pas`
- preserve the existing bundled-chain success path when helpers are available
- do not redesign `fafafa.ssl.certchain`, CA store loading, or broader verification semantics

## Task 1: RED - Reproduce the reachable helper gap

**Files:**
- Add: `tests/test_cert_utils_verify_chain_bio_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `tests/test_cert_utils_verify_chain_contract.pas`

**Steps:**
- Write a focused contract test that:
  - initializes OpenSSL and loads `Core/BIO/X509/PEM/EVP`
  - generates a root CA, an intermediate CA, and a leaf certificate
  - verifies a normal `VerifyChain(leaf + intermediate, root)` success path before stubbing helpers
  - uses call-count stubs where needed so the leaf certificate load still succeeds, then helper loss happens only during `VerifyChain(...)`'s internal bundled-intermediate parsing
  - exercises these representative reachable failures:
    - second-stage `BIO_new_mem_buf`
    - second-stage `PEM_read_bio_X509`
    - second-stage cleanup via `BIO_free`
    - intermediate export `BIO_new`
    - intermediate export `BIO_s_mem`
    - intermediate export `PEM_write_bio_X509`
  - asserts direct `VerifyChain(...)` must not raise and must return `False`
  - asserts `TryVerifyChain(...)` must not raise, must return `True`, and must set `AIsValid=False`
- Run the focused test and confirm it fails on the current source.

## Task 2: GREEN - Minimal VerifyChain guard

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Steps:**
- Add a local read-helper guard before `VerifyChain(...)` starts bundled-intermediate parsing so second-stage `BIO_new_mem_buf` / `PEM_read_bio_X509` / `BIO_free` loss degrades to `False`
- Add a local write-helper guard immediately before intermediate PEM export so `BIO_new` / `BIO_s_mem` / `PEM_write_bio_X509` / `BIO_free` loss also degrades to `False`
- Preserve existing contracts:
  - `VerifyChain(...)` returns `False` on helper loss instead of raising
  - `TryVerifyChain(...)` remains non-throwing and reports the degraded direct result through `AIsValid`

## Task 3: Verification

**Run:**
- `mkdir -p tmp/cert_utils_verify_chain_bio_contract && fpc -B -Fu./src -FUtmp/cert_utils_verify_chain_bio_contract -FEtmp/cert_utils_verify_chain_bio_contract -otmp/cert_utils_verify_chain_bio_contract/test_cert_utils_verify_chain_bio_contract tests/test_cert_utils_verify_chain_bio_contract.pas && ./tmp/cert_utils_verify_chain_bio_contract/test_cert_utils_verify_chain_bio_contract`
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-21-cert-utils-verify-chain-bio-guard.md src/fafafa.ssl.cert.utils.pas tests/test_cert_utils_verify_chain_bio_contract.pas task_plan.md findings.md progress.md`

**Expected:**
- bundled-chain helper loss no longer triggers `EAccessViolation`
- direct `VerifyChain(...)` safely degrades to `False`
- `TryVerifyChain(...)` remains non-throwing and reports `AIsValid=False`
- full module compile remains green
