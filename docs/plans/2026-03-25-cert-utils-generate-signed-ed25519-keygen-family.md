# Certificate Utils GenerateSigned Ed25519 Keygen Family Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Determine whether the remaining `GenerateSigned(...) -> GenerateEd25519Key(...)` delayed-loss helper family still needs dedicated hardening on the signed Ed25519 leaf path, and close it with focused contract evidence.

**Architecture:** Keep this batch narrow and discovery-driven. Add one focused family-level contract around RSA-CA-signed Ed25519 leaf generation, reusing the same delayed-loss helper sequence already closed for the self-signed Ed25519 path. If the new signed-path contract reproduces a fresh RED, apply the smallest possible production fix in `GenerateEd25519Key(...)` or its signed caller boundary; if it passes immediately, treat that as evidence that the shared helper hardening already covers the signed path and stop without touching production code.

**Tech Stack:** Free Pascal, OpenSSL loader stubs, focused Pascal contract tests

---

### Task 1: RED - Probe the signed Ed25519 keygen family

**Files:**
- Add: `tests/test_cert_utils_generate_signed_ed25519_keygen_family_contract.pas`
- Reference: `src/fafafa.ssl.cert.utils.pas`
- Reference: `tests/test_cert_utils_generate_selfsigned_ed25519_keygen_family_contract.pas`
- Reference: `tests/test_cert_utils_ed25519_contract.pas`

**Step 1: Write the focused test**

- Add one family-level contract test that:
  - initializes OpenSSL and loads `Core/BIO/X509/X509v3/PEM/EVP`
  - warms a normal RSA-CA generation + Ed25519 signed leaf path
  - uses delayed-loss wrappers to clear helpers inside `GenerateEd25519Key(...)` only after the helper gate and prior local step succeed:
    - clear `EVP_PKEY_keygen_init` immediately after `EVP_PKEY_CTX_new_id(...)`
    - clear `EVP_PKEY_keygen` immediately after `EVP_PKEY_keygen_init(...)`
    - clear `EVP_PKEY_CTX_free` immediately after `EVP_PKEY_keygen(...)`
  - asserts direct `GenerateSigned(...)` must raise a controlled `ESSLCertError`
  - asserts `TryGenerateSigned(...)` must not raise, must return `False`, and must clear outputs

**Step 2: Run the focused test**

Run:
`mkdir -p tmp/cert_utils_generate_signed_ed25519_keygen_family_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_signed_ed25519_keygen_family_contract -FEtmp/cert_utils_generate_signed_ed25519_keygen_family_contract -otmp/cert_utils_generate_signed_ed25519_keygen_family_contract/test_cert_utils_generate_signed_ed25519_keygen_family_contract tests/test_cert_utils_generate_signed_ed25519_keygen_family_contract.pas && ./tmp/cert_utils_generate_signed_ed25519_keygen_family_contract/test_cert_utils_generate_signed_ed25519_keygen_family_contract`

Expected:
- if the signed Ed25519 path still has an uncovered family gap, FAIL because direct `GenerateSigned(...)` dereferences a delayed-loss helper or returns the wrong public contract
- if the shared `GenerateEd25519Key(...)` hardening already covers the signed path, PASS immediately and no production edit is required

### Task 2: GREEN - Minimal shared helper fix only if RED reproduces

**Files:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Step 1: Apply the smallest safe fix only if the new contract fails**

- If Task 1 reproduces a fresh RED:
  - harden only the actual signed-path failure boundary, preferring a shared `GenerateEd25519Key(...)` fix if the nil-call still occurs there
  - preserve current public contracts:
    - direct `GenerateSigned(...)` raises controlled `ESSLCertError`
    - `TryGenerateSigned(...)` remains non-throwing, returns `False`, and clears outputs
  - do not redesign RSA/ECDSA key generation, PEM export, or post-success cleanup families
- If Task 1 passes immediately:
  - skip production edits
  - treat the family as already covered by the earlier shared helper hardening

### Task 3: Verification

**Files:**
- Verify: `tests/test_cert_utils_generate_signed_ed25519_keygen_family_contract.pas`
- Verify: `tests/test_cert_utils_generate_selfsigned_ed25519_keygen_family_contract.pas`
- Verify: `tests/test_cert_utils_ed25519_contract.pas`
- Verify: `src/fafafa.ssl.cert.utils.pas`
- Verify: `task_plan.md`
- Verify: `findings.md`
- Verify: `progress.md`

**Step 1: Run the signed-path family contract**

Run:
`mkdir -p tmp/cert_utils_generate_signed_ed25519_keygen_family_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_signed_ed25519_keygen_family_contract -FEtmp/cert_utils_generate_signed_ed25519_keygen_family_contract -otmp/cert_utils_generate_signed_ed25519_keygen_family_contract/test_cert_utils_generate_signed_ed25519_keygen_family_contract tests/test_cert_utils_generate_signed_ed25519_keygen_family_contract.pas && ./tmp/cert_utils_generate_signed_ed25519_keygen_family_contract/test_cert_utils_generate_signed_ed25519_keygen_family_contract`

Expected:
- PASS with no raw `EAccessViolation`
- each delayed-loss helper scenario either proves the shared hardening already works or validates the new minimal fix

**Step 2: Run adjacent Ed25519 regressions**

Run:
- `mkdir -p tmp/cert_utils_generate_selfsigned_ed25519_keygen_family_contract && fpc -B -Fu./src -FUtmp/cert_utils_generate_selfsigned_ed25519_keygen_family_contract -FEtmp/cert_utils_generate_selfsigned_ed25519_keygen_family_contract -otmp/cert_utils_generate_selfsigned_ed25519_keygen_family_contract/test_cert_utils_generate_selfsigned_ed25519_keygen_family_contract tests/test_cert_utils_generate_selfsigned_ed25519_keygen_family_contract.pas && ./tmp/cert_utils_generate_selfsigned_ed25519_keygen_family_contract/test_cert_utils_generate_selfsigned_ed25519_keygen_family_contract`
- `mkdir -p tmp/cert_utils_ed25519_contract && fpc -B -Fu./src -FUtmp/cert_utils_ed25519_contract -FEtmp/cert_utils_ed25519_contract -otmp/cert_utils_ed25519_contract/test_cert_utils_ed25519_contract tests/test_cert_utils_ed25519_contract.pas && ./tmp/cert_utils_ed25519_contract/test_cert_utils_ed25519_contract`

Expected:
- the earlier self-signed Ed25519 delayed-loss family remains green
- the baseline Ed25519 contract remains green on both self-signed and RSA-CA-signed leaf flows

**Step 3: Run full compile and diff hygiene**

Run:
- `python3 scripts/compile_all_modules.py`
- `git diff --check -- docs/plans/2026-03-25-cert-utils-generate-signed-ed25519-keygen-family.md tests/test_cert_utils_generate_signed_ed25519_keygen_family_contract.pas src/fafafa.ssl.cert.utils.pas task_plan.md findings.md progress.md`

Expected:
- full module compile remains green
- diff hygiene remains clean
