# Context Builder PKCS#11 PIN Source Serialization Contract Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Preserve non-secret builder PKCS#11 PIN-source state across `ExportToJSON` / `ImportFromJSON`, `ExportToINI` / `ImportFromINI`, and `Merge(...)`. Specifically, builder configurations using `pmEnvironment` or `pmFile` should keep their source-resolution behavior after round-trip or merge instead of silently degrading to `pmNone`.

**Architecture:** Add focused regressions in `tests/config/test_config_import_export.pas` and `tests/config/test_config_snapshot_clone.pas` that use builder PKCS#11 env/file source methods and then assert that `TryBuildServer` still fails with the same deterministic missing-source errors after JSON round-trip, INI round-trip, or merge. Apply the smallest safe fix in `src/fafafa.ssl.context.builder.pas` by serializing `pkcs11_pin_method` and `pkcs11_pin` only for the non-secret builder methods `pmEnvironment` and `pmFile`. Keep direct `pmValue` PINs out of export/import surfaces, and do not add callback plumbing or callback serialization.

**Tech Stack:** Free Pascal, builder import/export tests, merge tests, Try* build contract checks

---

### Task 1: Add focused RED regressions

**Files:**
- Modify: `tests/config/test_config_import_export.pas`
- Modify: `tests/config/test_config_snapshot_clone.pas`
- Reference: `tests/config/test_context_builder_try.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`

**Step 1: Write the failing tests**

- Add a JSON round-trip regression for builder `pmEnvironment`:
  - source builder uses certificate PEM + PKCS#11 URI + `WithPKCS11PINMethod(pmEnvironment)`
  - source value names a missing environment variable
  - after export/import, `TryBuildServer` should still fail with an environment-variable source-resolution error

- Add an INI round-trip regression for builder `pmFile`:
  - source builder uses certificate PEM + PKCS#11 URI + `WithPKCS11PINMethod(pmFile)`
  - source value points to a missing file
  - after export/import, `TryBuildServer` should still fail with a PIN-file source-resolution error

- Add a merge regression:
  - source builder uses builder `pmEnvironment`
  - destination merges source
  - merged builder should preserve the same missing-source failure class

**Step 2: Run focused RED**

Run:
`mkdir -p tmp/config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_import_export -FEtmp/config_import_export -otmp/config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/config_import_export/test_config_import_export`

Run:
`mkdir -p tmp/config_snapshot_clone && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_snapshot_clone -FEtmp/config_snapshot_clone -otmp/config_snapshot_clone/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/config_snapshot_clone/test_config_snapshot_clone`

Expected:
- FAIL because export/import/merge currently drop builder env/file PIN-source state and no longer reproduce the missing-source errors

### Task 2: Minimal non-secret state fix

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Extend only the safe builder state surface**

- For builder `pmEnvironment` / `pmFile`:
  - export `pkcs11_pin_method`
  - export `pkcs11_pin` source value
  - import both fields
  - merge both fields from exported JSON

**Step 2: Keep security/runtime boundaries**

- Do not export direct `pmValue` PIN contents
- Do not serialize callback functions or interactive state
- Do not change existing validation/build semantics outside restored env/file preservation

### Task 3: Verification

**Files:**
- Test: `tests/config/test_config_import_export.pas`
- Test: `tests/config/test_config_snapshot_clone.pas`

**Step 1: Re-run focused and adjacent regressions**

Run:
`mkdir -p tmp/config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_import_export -FEtmp/config_import_export -otmp/config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/config_import_export/test_config_import_export`

Run:
`mkdir -p tmp/config_snapshot_clone && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_snapshot_clone -FEtmp/config_snapshot_clone -otmp/config_snapshot_clone/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/config_snapshot_clone/test_config_snapshot_clone`

Run:
`mkdir -p tmp/context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_try -FEtmp/context_builder_try -otmp/context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/context_builder_try/test_context_builder_try`

Run:
`python3 scripts/compile_all_modules.py`

Expected:
- PASS

**Step 2: Run whitespace / patch hygiene**

Run:
`git diff --check -- src/fafafa.ssl.context.builder.pas tests/config/test_config_import_export.pas tests/config/test_config_snapshot_clone.pas docs/plans/2026-03-20-context-builder-pkcs11-pin-source-serialization-contract.md task_plan.md findings.md progress.md`

Expected:
- PASS

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record root cause and evidence**

- Note that builder env/file support became real runtime behavior, but import/export/merge still only preserved `pkcs11_uri`
- Record that the fix intentionally keeps direct `pmValue` PINs out of serialized surfaces

**Step 2: Roll next queue**

- Revisit whether `pkcs11_pin_method` should become externally transformable via `Override(...)`
- Keep direct-PIN export and callback/interactive builder support as separate security/design questions
