# Context Builder Override PEM Source Parity Plan

**Goal:** Restore `TSSLContextBuilder.Override(...)` parity for PEM-based certificate/private-key selection so override-configured PEM sources behave like the dedicated builder APIs and clear stale file state instead of being silently ignored.

**Architecture:** Cover the gap at two levels:

- transformation/export state:
  - `Override('certificate_pem', ...)` should become export-visible and clear stale `certificate_file`
  - `Override('private_key_pem', ...)` should become export-visible and clear stale `private_key_file`
- runtime precedence behavior:
  - overriding PEM on top of missing certificate/private-key file state must now succeed instead of still failing on stale file paths

Apply the smallest safe fix in `TSSLContextBuilderImpl.Override(...)` only. Do not change build precedence, and do not broaden unrelated override fields in this batch.

## Task 1: Add focused RED regressions

**Files:**
- Modify: `tests/test_transformation_methods.pas`
- Modify: `tests/config/test_context_builder_pem_precedence_regression.pas`

**Steps:**
- In transformation tests:
  - prove `Override('certificate_pem', ...)` clears stale `certificate_file`
  - prove `Override('private_key_pem', ...)` clears stale `private_key_file`
- In runtime precedence tests:
  - start from valid PEM payload plus missing file-backed builder state
  - override certificate/private-key PEM values
  - assert client/server build succeeds because override PEM now clears stale file state

**Run:**
- `mkdir -p tmp/transformation_methods && fpc -B -Fu./src -FUtmp/transformation_methods -FEtmp/transformation_methods -otmp/transformation_methods/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/transformation_methods/test_transformation_methods`
- `mkdir -p tmp/context_builder_pem_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_pem_precedence -FEtmp/context_builder_pem_precedence -otmp/context_builder_pem_precedence/test_context_builder_pem_precedence_regression tests/config/test_context_builder_pem_precedence_regression.pas && ./tmp/context_builder_pem_precedence/test_context_builder_pem_precedence_regression`

**Expected:**
- FAIL before the fix because `Override(...)` still ignores PEM fields and stale file state remains active.

## Task 2: Minimal override fix

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Steps:**
- In `Override(...)`:
  - recognize `certificate_pem` and assign it while clearing `FCertificateFile`
  - recognize `private_key_pem` and assign it while clearing `FPrivateKeyFile`
- Keep unknown-field no-op semantics unchanged.
- Do not add password/secret fields in this batch.

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
- Record root cause as override-surface PEM parity gap.
- Roll next queue to the next highest-value builder/external-config contract gap after PEM override parity is closed.
