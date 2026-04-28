# Context Builder Import File Clears Stale PEM State Plan

**Goal:** Remove history-dependent builder behavior where manual JSON / INI import of `certificate_file` or `private_key_file` can leave stale PEM state active, causing runtime build to keep using old PEM instead of the newly imported file selection.

**Architecture:** Add focused regressions that start from valid PEM-backed builder state, then import missing file paths through JSON / INI. The expected contract is that import mirrors fluent setter mutual exclusivity:

- `certificate_file` clears stale `certificate_pem`
- `certificate_pem` clears stale `certificate_file`
- `private_key_file` clears stale `private_key_pem`
- `private_key_pem` clears stale `private_key_file`

Keep the fix local to import assignment semantics in `TSSLContextBuilder`; do not change build precedence, secret export boundaries, or merge behavior in this batch.

## Task 1: Add focused RED regressions

**Files:**
- Modify: `tests/config/test_context_builder_pem_precedence_regression.pas`

**Steps:**
- Start from valid certificate/key PEM state on the builder.
- Import missing `certificate_file` and `private_key_file` values through both JSON and INI.
- Prove imported file selection clears stale PEM by expecting build failure against the missing file paths.

**Run:**
- `mkdir -p tmp/context_builder_pem_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_pem_precedence -FEtmp/context_builder_pem_precedence -otmp/context_builder_pem_precedence/test_context_builder_pem_precedence_regression tests/config/test_context_builder_pem_precedence_regression.pas && ./tmp/context_builder_pem_precedence/test_context_builder_pem_precedence_regression`

**Expected:**
- FAIL before the fix because stale PEM still wins and build succeeds.

## Task 2: Minimal import fix

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Steps:**
- In `ImportFromJSON`:
  - importing `certificate_file` clears `FCertificatePEM`
  - importing `certificate_pem` clears `FCertificateFile`
  - importing `private_key_file` clears `FPrivateKeyPEM`
  - importing `private_key_pem` clears `FPrivateKeyFile`
- In `ImportFromINI`, apply the same mutual-exclusion semantics.

**Run:**
- rerun the focused regression command above

**Expected:**
- PASS

## Task 3: Adjacent verification

**Files:**
- Test: `tests/config/test_context_builder_pem_precedence_regression.pas`
- Test: `tests/config/test_config_import_export.pas`
- Test: `tests/config/test_context_builder_try.pas`
- Test: `tests/config/test_config_validation.pas`

**Run:**
- `mkdir -p tmp/context_builder_pem_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_pem_precedence -FEtmp/context_builder_pem_precedence -otmp/context_builder_pem_precedence/test_context_builder_pem_precedence_regression tests/config/test_context_builder_pem_precedence_regression.pas && ./tmp/context_builder_pem_precedence/test_context_builder_pem_precedence_regression`
- `mkdir -p tmp/config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_import_export -FEtmp/config_import_export -otmp/config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/config_import_export/test_config_import_export`
- `mkdir -p tmp/context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_try -FEtmp/context_builder_try -otmp/context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/context_builder_try/test_context_builder_try`
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
- Record root cause as import-state mutual exclusivity drift, not build precedence drift.
- Capture RED/GREEN evidence and roll the next import/merge audit queue forward.
