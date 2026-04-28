# Context Builder PKCS#11 Direct PIN Import Default Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Restore external-config parity for manual PKCS#11 direct PIN imports. When JSON or INI contains `pkcs11_pin` but omits `pkcs11_pin_method`, the builder should default that imported state to `pmValue` instead of silently leaving it at `pmNone`.

**Architecture:** Add focused manual-import regressions in `tests/config/test_config_import_export.pas` that import JSON and INI snippets containing a PKCS#11 URI with URI-level `pin-source=env:...` plus a top-level `pkcs11_pin` only. Observe `TryBuildServer`: correct direct-PIN import should override the URI pin-source and therefore must not surface an environment-variable lookup error. Apply the smallest safe fix in `src/fafafa.ssl.context.builder.pas` by making `ImportFromJSON` / `ImportFromINI` default `pkcs11_pin`-only imports to `pmValue` when no explicit method is present. Keep named/ordinal `pkcs11_pin_method` parsing and env/file serialization contracts unchanged.

**Tech Stack:** Free Pascal, builder import tests, Try* build contract checks

---

### Task 1: Add focused RED regressions

**Files:**
- Modify: `tests/config/test_config_import_export.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`

**Step 1: Add manual JSON import regression**

- Import JSON containing:
  - `certificate_file`
  - `pkcs11_uri` with `module-path=...&pin-source=env:...`
  - top-level `pkcs11_pin`
  - no `pkcs11_pin_method`
- Assert:
  - `TryBuildServer` still fails without a real token
  - error must **not** mention environment-variable lookup, proving builder direct PIN overrides the URI source

**Step 2: Add manual INI import regression**

- Same contract for INI surface
- Again omit `pkcs11_pin_method`
- Assert environment-variable lookup is not the observed failure mode

**Step 3: Run focused RED**

Run:
`mkdir -p tmp/config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_import_export -FEtmp/config_import_export -otmp/config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/config_import_export/test_config_import_export`

Expected:
- FAIL because current import leaves `pkcs11_pin`-only configs at `pmNone`, so URI `pin-source` remains active

### Task 2: Minimal import fix

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Default `pkcs11_pin`-only imports to direct PIN**

- In `ImportFromJSON`:
  - if `pkcs11_pin` is present and `pkcs11_pin_method` is absent, set `FPKCS11PINMethod := pmValue`
- In `ImportFromINI`:
  - same rule

**Step 2: Keep scope narrow**

- Do not change explicit `pkcs11_pin_method` imports
- Do not serialize direct `pmValue` PINs on export
- Do not change callback/interactive runtime boundaries

### Task 3: Verification

**Files:**
- Test: `tests/config/test_config_import_export.pas`
- Test: `tests/config/test_context_builder_try.pas`

**Step 1: Re-run focused and adjacent regressions**

Run:
`mkdir -p tmp/config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_import_export -FEtmp/config_import_export -otmp/config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/config_import_export/test_config_import_export`

Run:
`mkdir -p tmp/context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_try -FEtmp/context_builder_try -otmp/context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/context_builder_try/test_context_builder_try`

Run:
`python3 scripts/compile_all_modules.py`

Expected:
- PASS

**Step 2: Run hygiene**

Run:
`git diff --check -- docs/plans/2026-03-20-context-builder-pkcs11-direct-pin-import-default.md src/fafafa.ssl.context.builder.pas tests/config/test_config_import_export.pas task_plan.md findings.md progress.md`

Expected:
- PASS

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record root cause and next queue**

- Note that manual import parity still lagged fluent/override direct-PIN semantics
- Record that `pkcs11_pin`-only import should behave like `.WithPKCS11PIN(...)`, not like unset PIN method
- Revisit whether validation should also detect impossible `pkcs11_pin` + `pmNone` states if any other path can still create them
