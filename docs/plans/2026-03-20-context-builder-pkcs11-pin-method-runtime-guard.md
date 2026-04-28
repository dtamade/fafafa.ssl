# Context Builder PKCS#11 PIN Method Runtime Guard Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Stop `TSSLContextBuilder` from silently accepting unsupported PKCS#11 PIN acquisition modes. Builder-based PKCS#11 config should fail validation and direct `TryBuildServer` attempts when callers set `WithPKCS11PINMethod(...)` to modes the builder runtime cannot actually honor.

**Architecture:** Add focused regressions in `tests/config/test_config_validation.pas` and `tests/config/test_context_builder_try.pas` proving that non-value PKCS#11 PIN methods currently pass too far through the builder surface. Apply the smallest safe fix in `src/fafafa.ssl.context.builder.pas` by validating unsupported methods when `UsePKCS11(...)` is active and by guarding `BuildClient` / `BuildServer` so they only pass a direct PIN override when `pmValue` is selected. Keep scope narrow: no new callback plumbing, no new serialization, no PKCS#11 backend API expansion.

**Tech Stack:** Free Pascal, builder validation tests, Try* build contract tests

---

### Task 1: Add focused RED regressions

**Files:**
- Modify: `tests/config/test_config_validation.pas`
- Modify: `tests/config/test_context_builder_try.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`

**Step 1: Write the failing tests**

- Add server-validation regression:
  - builder with certificate PEM + `UsePKCS11(...)`
  - set `WithPKCS11PIN('PKCS11_PIN_ENV')`
  - set `WithPKCS11PINMethod(pmEnvironment)`
  - assert validation is invalid and reports that callers must use URI `pin-source` or direct PIN

- Add `TryBuildServer` regression:
  - same server shape
  - set `WithPKCS11PINMethod(pmCallback)`
  - assert `TryBuildServer` returns an error instead of silently building

**Step 2: Run focused RED**

Run:
`mkdir -p tmp/config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_validation -FEtmp/config_validation -otmp/config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/config_validation/test_config_validation`

Run:
`mkdir -p tmp/context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_try -FEtmp/context_builder_try -otmp/context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/context_builder_try/test_context_builder_try`

Expected:
- FAIL because builder validation still treats unsupported PIN methods as valid PKCS#11 state
- FAIL because `TryBuildServer` still calls into build without a builder-level PKCS#11 PIN method guard

### Task 2: Minimal builder guard

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Add a narrow builder-level support boundary**

- When `FPKCS11URI <> ''`, treat only these builder PIN modes as supported:
  - `pmNone`
  - `pmValue`
- For `pmEnvironment`, `pmFile`, `pmCallback`, `pmInteractive`:
  - validation should add an error explaining the builder does not support those acquisition modes directly
  - build should raise a configuration error before delegating to backend loading

**Step 2: Keep runtime override semantics coherent**

- Only pass `FPKCS11PIN` as a direct `LoadPrivateKey(..., APIN)` override when `FPKCS11PINMethod = pmValue`
- Do not change PKCS#11 URI parsing, backend resolution, or secret serialization in this batch

### Task 3: Verification

**Files:**
- Test: `tests/config/test_config_validation.pas`
- Test: `tests/config/test_context_builder_try.pas`

**Step 1: Re-run focused and adjacent regressions**

Run:
`mkdir -p tmp/config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_validation -FEtmp/config_validation -otmp/config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/config_validation/test_config_validation`

Run:
`mkdir -p tmp/context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_try -FEtmp/context_builder_try -otmp/context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/context_builder_try/test_context_builder_try`

Expected:
- PASS

**Step 2: Run compile verification**

Run:
`python3 scripts/compile_all_modules.py`

Expected:
- PASS

**Step 3: Run whitespace / patch hygiene**

Run:
`git diff --check -- docs/plans/2026-03-20-context-builder-pkcs11-pin-method-runtime-guard.md tests/config/test_config_validation.pas tests/config/test_context_builder_try.pas src/fafafa.ssl.context.builder.pas task_plan.md findings.md progress.md`

Expected:
- PASS

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record root cause and evidence**

- Note that `WithPKCS11PINMethod(...)` exposed a broader builder API than the runtime/load surface could honor
- Record that the fix explicitly chooses guardrails over partial callback/file/env plumbing

**Step 2: Roll next queue**

- Revisit whether builder should later grow safe, explicit support for URI-generated `pin-source` composition or lower-level callback injection
- Keep that separate from serialization policy for secret-adjacent state
