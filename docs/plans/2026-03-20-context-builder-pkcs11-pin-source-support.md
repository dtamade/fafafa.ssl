# Context Builder PKCS#11 PIN Source Support Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Extend `TSSLContextBuilder` so `WithPKCS11PINMethod(pmEnvironment)` and `WithPKCS11PINMethod(pmFile)` are real supported paths instead of builder-level false negatives. The builder should resolve those PIN sources using the existing PKCS#11 PIN manager and then reuse the current PKCS#11 private-key loading path.

**Architecture:** Add focused regressions in `tests/config/test_config_validation.pas` and `tests/config/test_context_builder_try.pas`. Validation should accept non-empty `pmEnvironment` / `pmFile` builder PIN sources, while still rejecting `pmCallback` / `pmInteractive`. Runtime should fail deterministically on missing env var / missing file before attempting PKCS#11 key load, proving the builder now routes env/file PIN acquisition through `TPKCS11PINManager`. Apply the smallest safe fix in `src/fafafa.ssl.context.builder.pas` by reusing `fafafa.ssl.pkcs11.pin` instead of adding callback plumbing or secret serialization.

**Tech Stack:** Free Pascal, builder validation tests, Try* build contract tests, PKCS#11 PIN manager

---

### Task 1: Add focused RED regressions

**Files:**
- Modify: `tests/config/test_config_validation.pas`
- Modify: `tests/config/test_context_builder_try.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`

**Step 1: Write the failing tests**

- Update builder validation coverage:
  - `UsePKCS11(...) + WithPKCS11PIN('PKCS11_PIN_ENV') + WithPKCS11PINMethod(pmEnvironment)` should validate
  - `UsePKCS11(...) + WithPKCS11PIN('/tmp/pkcs11-pin.txt') + WithPKCS11PINMethod(pmFile)` should validate
  - keep a callback-path regression proving `pmCallback` is still explicitly unsupported

- Add deterministic `TryBuildServer` regressions:
  - missing environment variable should fail with an env-source error
  - missing PIN file should fail with a file-source error

**Step 2: Run focused RED**

Run:
`mkdir -p tmp/config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_validation -FEtmp/config_validation -otmp/config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/config_validation/test_config_validation`

Run:
`mkdir -p tmp/context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_try -FEtmp/context_builder_try -otmp/context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/context_builder_try/test_context_builder_try`

Expected:
- FAIL because validation still rejects env/file methods as unsupported
- FAIL because build path still treats env/file methods as unsupported instead of resolving PIN sources

### Task 2: Minimal builder fix

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Reuse existing PKCS#11 PIN manager**

- Add builder-side support for:
  - `pmEnvironment`
  - `pmFile`
- Resolve those methods via `TPKCS11PINManager.GetPIN(...)`
- Keep `pmNone` / `pmValue` behavior unchanged

**Step 2: Preserve explicit unsupported boundary**

- `pmCallback` and `pmInteractive` remain unsupported in builder runtime
- validation should only reject unsupported methods or missing source values
- no callback injection API in this batch

### Task 3: Verification

**Files:**
- Test: `tests/config/test_config_validation.pas`
- Test: `tests/config/test_context_builder_try.pas`

**Step 1: Re-run focused and adjacent regressions**

Run:
`mkdir -p tmp/config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_validation -FEtmp/config_validation -otmp/config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/config_validation/test_config_validation`

Run:
`mkdir -p tmp/context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_try -FEtmp/context_builder_try -otmp/context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/context_builder_try/test_context_builder_try`

Run:
`python3 scripts/compile_all_modules.py`

Expected:
- PASS

**Step 2: Run whitespace / patch hygiene**

Run:
`git diff --check -- docs/plans/2026-03-20-context-builder-pkcs11-pin-source-support.md tests/config/test_config_validation.pas tests/config/test_context_builder_try.pas src/fafafa.ssl.context.builder.pas task_plan.md findings.md progress.md`

Expected:
- PASS

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record root cause and evidence**

- Note that builder support was previously too narrow, but env/file support already existed one layer lower in `TPKCS11PINManager`
- Record that this batch deliberately reuses existing secure source resolution instead of growing URI-mutation or callback APIs

**Step 2: Roll next queue**

- Keep callback/interactive support as a separate design decision
- Revisit docs to describe builder-supported env/file paths once the runtime contract is locked
