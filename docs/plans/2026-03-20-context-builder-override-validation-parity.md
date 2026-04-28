# Context Builder Override Validation Parity Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Restore `TSSLContextBuilder.Override(...)` parity for validation-relevant fields so override-based configuration behaves like the dedicated builder APIs for `use_system_roots` and `pkcs11_uri`.

**Architecture:** Add focused validation regressions in `tests/config/test_config_validation.pas` that exercise override-based configuration. First prove RED for a client config that should inherit `.WithSystemRoots` semantics and a server config that should inherit `.UsePKCS11(...)` semantics. Then apply the smallest safe fix in `src/fafafa.ssl.context.builder.pas` by teaching `Override(...)` to dispatch those fields instead of silently ignoring them. Keep runtime build behavior unchanged outside the existing field assignment path.

**Tech Stack:** Free Pascal, builder validation tests, no backend-specific runtime dependency

---

### Task 1: Add focused RED regressions

**Files:**
- Modify: `tests/config/test_config_validation.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`

**Step 1: Write the failing tests**

- Add a client validation case using:
  - `WithTLS12And13`
  - `WithVerifyPeer`
  - `.Override('use_system_roots', 'true')`
- Assert:
  - config remains valid
  - warning about missing CA certificates is gone

- Add a server validation case using:
  - generated certificate PEM
  - `.Override('pkcs11_uri', 'pkcs11:token=TestToken;object=ServerKey;type=private')`
- Assert:
  - `ValidateServer` is valid
  - missing private-key error is gone

**Step 2: Run test to verify it fails**

Run:
`mkdir -p tmp/config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_validation -FEtmp/config_validation -otmp/config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/config_validation/test_config_validation`

Expected:
- FAIL because `Override(...)` still ignores `use_system_roots` and `pkcs11_uri`

### Task 2: Minimal builder fix

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Write minimal implementation**

- Extend `TSSLContextBuilderImpl.Override(...)` to recognize:
  - `use_system_roots`
  - `pkcs11_uri`
- Preserve existing behavior for all already-supported fields and unknown-field no-op semantics.

**Step 2: Re-run the RED test**

Run:
`mkdir -p tmp/config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_validation -FEtmp/config_validation -otmp/config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/config_validation/test_config_validation`

Expected:
- PASS

### Task 3: Focused regression verification

**Files:**
- Test: `tests/config/test_config_validation.pas`
- Test: `tests/test_transformation_methods.pas`
- Test: `tests/config/test_context_builder_try.pas`

**Step 1: Run adjacent regressions**

Run:
- `mkdir -p tmp/transformation_methods && fpc -B -Fu./src -FUtmp/transformation_methods -FEtmp/transformation_methods -otmp/transformation_methods/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/transformation_methods/test_transformation_methods`
- `mkdir -p tmp/context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_try -FEtmp/context_builder_try -otmp/context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/context_builder_try/test_context_builder_try`

Expected:
- PASS

**Step 2: Run compile verification**

Run:
`python3 scripts/compile_all_modules.py`

Expected:
- PASS

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record root cause and RED/GREEN evidence**

- Note that `Override(...)` was narrower than the validation/runtime field surface:
  - `WithSystemRoots` and `UsePKCS11(...)` affected validation/build
  - override-based configuration for the same fields was silently dropped

**Step 2: Mark batch complete and roll next queue**

- Queue the next highest-value builder drift, likely remaining `Override(...)` / serialization gaps for PKCS#11 PIN-related fields if they still lack observable coverage.
