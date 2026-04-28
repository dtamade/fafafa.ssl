# Context Builder Override PKCS#11 PIN Method Parity Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Restore `TSSLContextBuilder.Override(...)` parity for PKCS#11 PIN-source selection so override/transform-based configuration can express the same env/file builder contract as `.WithPKCS11PINMethod(...)`.

**Architecture:** Add a focused override-surface regression in `tests/test_transformation_methods.pas` that proves an override-configured PKCS#11 env source becomes export-visible on the builder state surface, and add a runtime regression in `tests/config/test_context_builder_try.pas` proving override-configured `pmEnvironment` still routes through builder PIN-source resolution. Apply the smallest safe fix in `src/fafafa.ssl.context.builder.pas` by teaching `Override(...)` to recognize `pkcs11_pin_method` using a tolerant parser for enum names and ordinals. Keep direct `pkcs11_pin` override semantics intact for callers that only set the PIN value.

**Tech Stack:** Free Pascal, builder transform tests, Try* build contract tests

---

### Task 1: Add focused RED regressions

**Files:**
- Modify: `tests/test_transformation_methods.pas`
- Modify: `tests/config/test_context_builder_try.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`

**Step 1: Add override state-surface regression**

- Add a transformation/override test using:
  - `.UsePKCS11('pkcs11:...')`
  - `.Override('pkcs11_pin', 'PKCS11_PIN_ENV')`
  - `.Override('pkcs11_pin_method', 'pmEnvironment')`
- Assert:
  - exported JSON includes `pkcs11_pin_method`
  - exported JSON includes the env source value

**Step 2: Add runtime regression**

- Add a `TryBuildServer` case using:
  - certificate PEM
  - `.Override('pkcs11_uri', 'pkcs11:...module-path=...')`
  - `.Override('pkcs11_pin', 'PKCS11_BUILDER_MISSING_ENV_OVERRIDE')`
  - `.Override('pkcs11_pin_method', 'pmEnvironment')`
- Assert:
  - build fails
  - error mentions missing environment-variable resolution rather than silently treating the source name as a direct PIN

**Step 3: Run focused RED**

Run:
`mkdir -p tmp/transformation_methods && fpc -B -Fu./src -FUtmp/transformation_methods -FEtmp/transformation_methods -otmp/transformation_methods/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/transformation_methods/test_transformation_methods`

Run:
`mkdir -p tmp/context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_try -FEtmp/context_builder_try -otmp/context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/context_builder_try/test_context_builder_try`

Expected:
- FAIL because `Override(...)` still ignores `pkcs11_pin_method`

### Task 2: Minimal override fix

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Extend only the missing override field**

- Teach `Override(...)` to recognize `pkcs11_pin_method`
- Accept case-insensitive enum names such as `pmEnvironment` plus numeric ordinals
- Leave invalid values as no-op to preserve existing defensive override behavior

**Step 2: Keep scope narrow**

- Do not add callback plumbing
- Do not expand secret export/import surfaces beyond the already-approved env/file serialization
- Do not change validation/build wording except through restored override state

### Task 3: Verification

**Files:**
- Test: `tests/test_transformation_methods.pas`
- Test: `tests/config/test_context_builder_try.pas`
- Test: `tests/config/test_config_import_export.pas`

**Step 1: Re-run focused and adjacent regressions**

Run:
`mkdir -p tmp/transformation_methods && fpc -B -Fu./src -FUtmp/transformation_methods -FEtmp/transformation_methods -otmp/transformation_methods/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/transformation_methods/test_transformation_methods`

Run:
`mkdir -p tmp/context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_try -FEtmp/context_builder_try -otmp/context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/context_builder_try/test_context_builder_try`

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

- Note that override/transform support had not caught up with the newly-supported builder env/file PIN methods
- Record whether `pkcs11_pin` override remains order-sensitive relative to `pkcs11_pin_method`
- Roll next queue toward the next builder external-config parity gap
