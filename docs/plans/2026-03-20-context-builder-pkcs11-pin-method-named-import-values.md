# Context Builder PKCS#11 PIN Method Named Import Values Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Let human-authored builder imports express PKCS#11 PIN methods with symbolic names such as `pmEnvironment` / `pmFile`, not just numeric ordinals, while preserving the existing round-trip contract.

**Architecture:** Add focused regressions in `tests/config/test_config_import_export.pas` that import manual JSON and INI snippets containing named `pkcs11_pin_method` values, then assert that `TryBuildServer` still observes the correct env/file source-resolution failures. Apply the smallest safe fix in `src/fafafa.ssl.context.builder.pas` by reusing the existing tolerant PKCS#11 PIN-method parser in `ImportFromJSON` and `ImportFromINI`. Keep numeric ordinals working exactly as before.

**Tech Stack:** Free Pascal, builder import tests, Try* build contract checks

---

### Task 1: Add focused RED regressions

**Files:**
- Modify: `tests/config/test_config_import_export.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`

**Step 1: Add manual JSON import regression**

- Import a hand-written JSON object containing:
  - `certificate_file`
  - `pkcs11_uri`
  - `pkcs11_pin`
  - `pkcs11_pin_method: "pmEnvironment"`
- Assert:
  - import does not crash
  - `TryBuildServer` fails with environment-variable source-resolution semantics

**Step 2: Add manual INI import regression**

- Import a hand-written INI payload containing:
  - `certificate_file=...`
  - `pkcs11_uri=...`
  - `pkcs11_pin=...`
  - `pkcs11_pin_method=pmFile`
- Assert:
  - import does not crash
  - `TryBuildServer` fails with PIN-file source-resolution semantics

**Step 3: Run focused RED**

Run:
`mkdir -p tmp/config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_import_export -FEtmp/config_import_export -otmp/config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/config_import_export/test_config_import_export`

Expected:
- FAIL because manual named `pkcs11_pin_method` values are not yet accepted by JSON/INI import

### Task 2: Minimal import fix

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Reuse the tolerant parser**

- In `ImportFromJSON`, accept:
  - numeric `pkcs11_pin_method`
  - string `pkcs11_pin_method`
- In `ImportFromINI`, parse `pkcs11_pin_method` through the same helper so named values work

**Step 2: Keep scope narrow**

- Do not change export format; JSON/INI export can keep emitting ordinals
- Do not add callback support
- Do not widen secret export/import boundaries

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

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record root cause and next queue**

- Note that round-trip support existed, but hand-written import surfaces still lagged behind override parsing
- Roll next queue to the next external-config parity or order-sensitivity contract
