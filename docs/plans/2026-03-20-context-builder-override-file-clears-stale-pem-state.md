# Context Builder Override File Clears Stale PEM State Plan

**Goal:** Remove history-dependent `Override(...)` behavior where overriding `certificate_file` or `private_key_file` leaves stale PEM state active, so runtime build still uses old PEM instead of the explicitly overridden file selection.

**Architecture:** Add focused override regressions at two levels:

- transformation/export state:
  - overriding `certificate_file` clears stale `certificate_pem`
  - overriding `private_key_file` clears stale `private_key_pem`
- runtime build behavior:
  - overriding missing certificate/private-key file paths on top of valid PEM-backed state must now fail against those missing files instead of silently succeeding through stale PEM

Apply the smallest safe fix in `TSSLContextBuilderImpl.Override(...)` by mirroring fluent setter mutual exclusivity for file overrides only. Do not expand `Override(...)` to accept PEM fields in this batch, and do not modify `Merge(...)`.

## Task 1: Add focused RED regressions

**Files:**
- Modify: `tests/test_transformation_methods.pas`
- Modify: `tests/config/test_context_builder_pem_precedence_regression.pas`

**Steps:**
- In transformation tests:
  - prove `Override('certificate_file', ...)` clears exported `certificate_pem`
  - prove `Override('private_key_file', ...)` clears exported `private_key_pem`
- In runtime precedence tests:
  - start from valid PEM-backed builder state
  - override missing certificate/private-key file paths
  - assert build now fails on those missing file paths

**Run:**
- `mkdir -p tmp/transformation_methods && fpc -B -Fu./src -FUtmp/transformation_methods -FEtmp/transformation_methods -otmp/transformation_methods/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/transformation_methods/test_transformation_methods`
- `mkdir -p tmp/context_builder_pem_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_pem_precedence -FEtmp/context_builder_pem_precedence -otmp/context_builder_pem_precedence/test_context_builder_pem_precedence_regression tests/config/test_context_builder_pem_precedence_regression.pas && ./tmp/context_builder_pem_precedence/test_context_builder_pem_precedence_regression`

**Expected:**
- FAIL before the fix because stale PEM still survives file overrides.

## Task 2: Minimal override fix

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Steps:**
- In `Override(...)`:
  - overriding `certificate_file` clears `FCertificatePEM`
  - overriding `private_key_file` clears `FPrivateKeyPEM`
- Keep unknown-field no-op semantics unchanged.
- Do not add new override fields in this batch.

**Run:**
- rerun the two focused regression commands above

**Expected:**
- PASS

## Task 3: Adjacent verification

**Files:**
- Test: `tests/test_transformation_methods.pas`
- Test: `tests/config/test_context_builder_pem_precedence_regression.pas`
- Test: `tests/config/test_context_builder_try.pas`
- Test: `tests/config/test_config_import_export.pas`
- Test: `tests/config/test_config_validation.pas`

**Run:**
- `mkdir -p tmp/transformation_methods && fpc -B -Fu./src -FUtmp/transformation_methods -FEtmp/transformation_methods -otmp/transformation_methods/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/transformation_methods/test_transformation_methods`
- `mkdir -p tmp/context_builder_pem_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_pem_precedence -FEtmp/context_builder_pem_precedence -otmp/context_builder_pem_precedence/test_context_builder_pem_precedence_regression tests/config/test_context_builder_pem_precedence_regression.pas && ./tmp/context_builder_pem_precedence/test_context_builder_pem_precedence_regression`
- `mkdir -p tmp/context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_try -FEtmp/context_builder_try -otmp/context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/context_builder_try/test_context_builder_try`
- `mkdir -p tmp/config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_import_export -FEtmp/config_import_export -otmp/config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/config_import_export/test_config_import_export`
- `mkdir -p tmp/config_validation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_validation -FEtmp/config_validation -otmp/config_validation/test_config_validation tests/config/test_config_validation.pas && ./tmp/config_validation/test_config_validation`
- `python3 scripts/compile_all_modules.py`

**Expected:**
- PASS

## Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Steps:**
- Record root cause as override-state mutual-exclusion drift.
- Roll next queue to `Merge(...)` / `ApplyPreset(...)` file-vs-PEM mutual-exclusion parity.
