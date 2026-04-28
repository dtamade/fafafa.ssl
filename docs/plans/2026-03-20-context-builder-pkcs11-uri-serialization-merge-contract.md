# Context Builder PKCS#11 URI Serialization And Merge Contract Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Preserve non-secret PKCS#11 private-key source state (`pkcs11_uri`) across builder JSON/INI round-trips and `Merge(...)`, so imported or merged server configs stay valid like the original builder.

**Architecture:** Add focused regressions in `tests/config/test_config_import_export.pas` and `tests/config/test_config_snapshot_clone.pas` that use a real certificate PEM plus `UsePKCS11(...)`, then observe the server-validation contract after round-trip or merge. Apply the smallest safe fix in `src/fafafa.ssl.context.builder.pas` by extending JSON/INI export/import and merge parsing to carry `pkcs11_uri`. Do not serialize sensitive `pkcs11_pin` values in this batch.

**Tech Stack:** Free Pascal, builder validation tests, self-signed PEM fixtures

---

### Task 1: Add focused RED regressions

**Files:**
- Modify: `tests/config/test_config_import_export.pas`
- Modify: `tests/config/test_config_snapshot_clone.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`

**Step 1: Write the failing tests**

- Add JSON round-trip test:
  - builder with certificate PEM + `UsePKCS11('pkcs11:...')`
  - export/import JSON
  - assert imported builder still validates as a valid server config

- Add INI round-trip test:
  - same shape
  - export/import INI
  - assert imported builder still validates

- Add merge test:
  - source builder with certificate PEM + `UsePKCS11('pkcs11:...')`
  - merge into destination builder
  - assert merged builder still validates

**Step 2: Run focused RED**

Run:
`mkdir -p tmp/config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_import_export -FEtmp/config_import_export -otmp/config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/config_import_export/test_config_import_export`

Run:
`mkdir -p tmp/config_snapshot_clone && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_snapshot_clone -FEtmp/config_snapshot_clone -otmp/config_snapshot_clone/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/config_snapshot_clone/test_config_snapshot_clone`

Expected:
- FAIL because `pkcs11_uri` is silently dropped by JSON/INI export/import and by merge's JSON-backed field surface

### Task 2: Minimal builder fix

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Extend non-secret state surface**

- `ExportToJSON` / `ImportFromJSON` should carry `pkcs11_uri`
- `ExportToINI` / `ImportFromINI` should carry `pkcs11_uri`
- `Merge(...)` should restore `pkcs11_uri` from exported JSON just like other non-empty builder fields

**Step 2: Keep scope narrow**

- Do not add `pkcs11_pin` serialization in this batch
- Do not change validation wording or runtime load precedence

### Task 3: Verification

**Files:**
- Test: `tests/config/test_config_import_export.pas`
- Test: `tests/config/test_config_snapshot_clone.pas`
- Test: `tests/config/test_config_validation.pas`

**Step 1: Re-run focused and adjacent regressions**

Run:
- `mkdir -p tmp/config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_import_export -FEtmp/config_import_export -otmp/config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/config_import_export/test_config_import_export`
- `mkdir -p tmp/config_snapshot_clone && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_snapshot_clone -FEtmp/config_snapshot_clone -otmp/config_snapshot_clone/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/config_snapshot_clone/test_config_snapshot_clone`
- `mkdir -p tmp/config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_validation -FEtmp/config_validation -otmp/config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/config_validation/test_config_validation`

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

**Step 1: Record root cause and evidence**

- Note that `Clone` already preserved PKCS#11 state directly, but JSON/INI/merge paths still lost the non-secret URI and invalidated server configs.

**Step 2: Roll next queue**

- Re-evaluate whether `pkcs11_pin_method` needs non-secret serialization support or whether it should stay runtime-only until a safe external contract exists.
