# Context Builder PKCS#11 PIN Order Sensitivity Contract Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove silent order sensitivity between `pkcs11_pin` and `pkcs11_pin_method` so an explicit builder env/file source selection is not overwritten just because the caller sets the source value afterwards.

**Architecture:** Add a focused runtime regression in `tests/config/test_context_builder_try.pas` proving that `.WithPKCS11PINMethod(pmEnvironment).WithPKCS11PIN(...)` still routes through environment source resolution, and add a focused override/export regression in `tests/test_transformation_methods.pas` proving `.Override('pkcs11_pin_method', 'pmEnvironment').Override('pkcs11_pin', ...)` keeps env/file state export-visible. Apply the smallest safe fix in `src/fafafa.ssl.context.builder.pas` by changing `WithPKCS11PIN(...)` and `Override('pkcs11_pin', ...)` to preserve explicitly chosen non-value methods instead of unconditionally resetting to `pmValue`. Keep the default direct-PIN behavior when no explicit non-value method was selected.

**Tech Stack:** Free Pascal, Try* build contract tests, builder transformation/export tests

---

### Task 1: Add focused RED regressions

**Files:**
- Modify: `tests/config/test_context_builder_try.pas`
- Modify: `tests/test_transformation_methods.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`

**Step 1: Add fluent runtime regression**

- Create a builder using:
  - `UsePKCS11(...)`
  - `WithPKCS11PINMethod(pmEnvironment)`
  - `WithPKCS11PIN('MISSING_ENV_VAR')`
- Assert:
  - `TryBuildServer` fails
  - failure still mentions environment-variable source resolution

**Step 2: Add override state regression**

- Create a builder using:
  - `.Override('pkcs11_pin_method', 'pmEnvironment')`
  - `.Override('pkcs11_pin', 'PKCS11_PIN_ENV')`
- Assert:
  - exported JSON still includes `pkcs11_pin_method`
  - exported JSON still includes the env source value

**Step 3: Run focused RED**

Run:
`mkdir -p tmp/context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_try -FEtmp/context_builder_try -otmp/context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/context_builder_try/test_context_builder_try`

Run:
`mkdir -p tmp/transformation_methods && fpc -B -Fu./src -FUtmp/transformation_methods -FEtmp/transformation_methods -otmp/transformation_methods/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/transformation_methods/test_transformation_methods`

Expected:
- FAIL because setting the PIN value after selecting env/file method currently resets the method to `pmValue`

### Task 2: Minimal builder fix

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Preserve explicit non-value methods**

- `WithPKCS11PIN(...)` should keep `pmEnvironment` / `pmFile` / other explicit non-value methods intact
- `Override('pkcs11_pin', ...)` should do the same
- Keep `pmValue` as the default when the caller never selected another method

**Step 2: Keep scope narrow**

- Do not change callback/interactive support boundaries
- Do not change import/export format
- Do not change validation/build wording except through restored state semantics

### Task 3: Verification

**Files:**
- Test: `tests/config/test_context_builder_try.pas`
- Test: `tests/test_transformation_methods.pas`
- Test: `tests/config/test_config_import_export.pas`

**Step 1: Re-run focused and adjacent regressions**

Run:
`mkdir -p tmp/context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_try -FEtmp/context_builder_try -otmp/context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/context_builder_try/test_context_builder_try`

Run:
`mkdir -p tmp/transformation_methods && fpc -B -Fu./src -FUtmp/transformation_methods -FEtmp/transformation_methods -otmp/transformation_methods/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/transformation_methods/test_transformation_methods`

Run:
`mkdir -p tmp/config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_import_export -FEtmp/config_import_export -otmp/config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/config_import_export/test_config_import_export`

Run:
`python3 scripts/compile_all_modules.py`

Expected:
- PASS

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record root cause and next queue**

- Note that the contract gap was not missing parser support anymore, but setter ordering
- Record whether preserving explicit non-value methods should also cover unsupported builder methods as a consistency rule
