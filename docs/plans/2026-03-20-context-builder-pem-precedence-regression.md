# Context Builder PEM Precedence Regression Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Restore `TSSLContextBuilder` so imported dual-state PEM inputs behave as validation promises: certificate uses `PEM > file`, and private key uses `PKCS#11 > PEM > file`.

**Architecture:** Add a focused Pascal regression test that creates real dual-state builder configs through `ImportFromJSON(...)` on top of missing file paths, then fix `BuildClient` and `BuildServer` to match the already-documented precedence. Keep the change local to builder load order; do not redesign import/merge semantics.

**Tech Stack:** Free Pascal, real builder/runtime tests, self-signed PEM fixtures

---

### Task 1: Add focused RED regression

**Files:**
- Add: `tests/config/test_context_builder_pem_precedence_regression.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`

**Step 1: Write the failing test**

- Generate a real self-signed certificate/key pair in PEM.
- Create dual-state configs with missing `certificate_file` / `private_key_file` plus imported `certificate_pem` / `private_key_pem`.
- Cover four cases:
  - `BuildClient` certificate precedence
  - `BuildClient` private-key precedence
  - `BuildServer` certificate precedence
  - `BuildServer` private-key precedence

**Step 2: Run test to verify it fails**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_context_builder_pem_precedence_regression tests/config/test_context_builder_pem_precedence_regression.pas && ./tmp/test_context_builder_pem_precedence_regression`

Expected:
- FAIL because build still hits missing file paths before PEM.

### Task 2: Minimal builder fix

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Write minimal implementation**

- In both `BuildClient` and `BuildServer`:
  - change certificate loading to `PEM > file`
  - change private-key loading to `PKCS#11 > PEM > file`

**Step 2: Re-run the focused test**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_context_builder_pem_precedence_regression tests/config/test_context_builder_pem_precedence_regression.pas && ./tmp/test_context_builder_pem_precedence_regression`

Expected:
- PASS

### Task 3: Focused regression verification

**Files:**
- Test: `tests/config/test_context_builder_pem_precedence_regression.pas`
- Test: `tests/config/test_config_validation.pas`
- Test: `tests/config/test_context_builder_try.pas`

**Step 1: Run adjacent regressions**

Run:
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation`
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/test_context_builder_try`

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

- Note that current disk code had regressed from prior recorded behavior: `ValidateServer` still promised PEM precedence while `BuildClient/BuildServer` had fallen back to file-first paths.

**Step 2: Mark batch complete and roll next queue**

- Move the queue back to the next concrete backend/default-validation contract gap.
