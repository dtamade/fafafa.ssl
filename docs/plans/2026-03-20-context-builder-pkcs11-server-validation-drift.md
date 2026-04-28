# Context Builder PKCS#11 Server Validation Drift Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Restore `TSSLContextBuilder.ValidateServer` so a server config using a certificate plus `UsePKCS11(...)` is treated as valid, matching the existing `BuildServer` private-key source contract.

**Architecture:** Add a focused Pascal validation regression that covers a certificate PEM plus PKCS#11 URI without file/PEM private-key material, then make the smallest possible validation fix in `ValidateServer`. Keep runtime build semantics unchanged; only close the validation/runtime drift.

**Tech Stack:** Free Pascal, builder validation tests, self-signed PEM fixtures

---

### Task 1: Add focused RED regression

**Files:**
- Modify: `tests/config/test_config_validation.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`

**Step 1: Write the failing test**

- Generate a real self-signed certificate PEM.
- Build a server config with:
  - `WithCertificatePEM(...)`
  - `UsePKCS11('pkcs11:token=TestToken;object=ServerKey;type=private')`
- Assert `ValidateServer` is valid and does not report a missing private-key error.

**Step 2: Run test to verify it fails**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation`

Expected:
- FAIL because `ValidateServer` still ignores `FPKCS11URI` when checking required private-key sources.

### Task 2: Minimal validation fix

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Write minimal implementation**

- In `ValidateServer`:
  - treat `FPKCS11URI` as a valid private-key source for the required-key check
  - update the error text so it matches the supported APIs (`WithPrivateKey`, `WithPrivateKeyPEM`, `UsePKCS11`)

**Step 2: Re-run the RED test**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation`

Expected:
- PASS

### Task 3: Focused regression verification

**Files:**
- Test: `tests/config/test_config_validation.pas`
- Test: `tests/config/test_context_builder_try.pas`
- Test: `tests/config/test_context_builder_pem_precedence_regression.pas`

**Step 1: Run adjacent regressions**

Run:
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/test_context_builder_try`
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_context_builder_pem_precedence_regression tests/config/test_context_builder_pem_precedence_regression.pas && ./tmp/test_context_builder_pem_precedence_regression`

Expected:
- PASS

**Step 2: Run compile verification**

Run:
`python3 -u scripts/compile_all_modules.py`

Expected:
- PASS

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record root cause and RED/GREEN evidence**

- Note that `BuildServer` already accepted `FPKCS11URI` as a private-key source while `ValidateServer` still rejected the same configuration.

**Step 2: Mark batch complete and roll next queue**

- Move the review queue from this server validation drift to the next highest-value behavior or contract gap discovered during focused verification.
