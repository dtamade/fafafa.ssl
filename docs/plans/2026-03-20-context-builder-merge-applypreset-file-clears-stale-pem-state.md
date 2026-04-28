# Context Builder Merge/ApplyPreset File-vs-PEM Mutual-Exclusion Plan

**Goal:** Remove history-dependent `Merge(...)` / `ApplyPreset(...)` behavior where merging file-based certificate or private-key selection leaves stale PEM state active, and where merging PEM selection leaves stale file state behind. After the fix, these mutation surfaces must match the existing fluent setter/import/override mutual-exclusion contract.

**Architecture:** Cover the drift at two levels:

- merge/apply-preset export-state semantics:
  - merged `certificate_file` clears stale `certificate_pem`
  - merged `private_key_file` clears stale `private_key_pem`
  - merged `certificate_pem` clears stale `certificate_file`
  - merged `private_key_pem` clears stale `private_key_file`
- runtime precedence behavior:
  - merging missing certificate/private-key file paths on top of valid PEM-backed state must now fail against those merged file paths instead of silently succeeding through stale PEM

Apply the smallest safe fix in `TSSLContextBuilderImpl.Merge(...)` only. Keep `ApplyPreset(...)` as a thin wrapper over `Merge(...)`. Do not change build precedence, secret export boundaries, or broaden `Override(...)` in this batch.

## Task 1: Add focused RED regressions

**Files:**
- Modify: `tests/config/test_config_snapshot_clone.pas`
- Modify: `tests/config/test_batch_config.pas`
- Modify: `tests/config/test_context_builder_pem_precedence_regression.pas`

**Steps:**
- In merge snapshot tests:
  - prove merged file fields clear stale PEM state
  - prove merged PEM fields clear stale file state
- In batch/apply-preset tests:
  - prove `ApplyPreset(...)` inherits the same mutual-exclusion behavior
- In runtime precedence tests:
  - start from valid PEM-backed builder state
  - merge missing certificate/private-key file paths
  - assert client/server build now fails on those merged file paths

**Run:**
- `mkdir -p tmp/config_snapshot_clone && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_snapshot_clone -FEtmp/config_snapshot_clone -otmp/config_snapshot_clone/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/config_snapshot_clone/test_config_snapshot_clone`
- `mkdir -p tmp/batch_config && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/batch_config -FEtmp/batch_config -otmp/batch_config/test_batch_config tests/config/test_batch_config.pas && ./tmp/batch_config/test_batch_config`
- `mkdir -p tmp/context_builder_pem_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_pem_precedence -FEtmp/context_builder_pem_precedence -otmp/context_builder_pem_precedence/test_context_builder_pem_precedence_regression tests/config/test_context_builder_pem_precedence_regression.pas && ./tmp/context_builder_pem_precedence/test_context_builder_pem_precedence_regression`

**Expected:**
- FAIL before the fix because merge/apply-preset still preserve contradictory file+PEM state and runtime still consumes stale PEM over merged file paths.

## Task 2: Minimal merge fix

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Steps:**
- In `Merge(...)`:
  - when source carries non-empty `certificate_file`, assign it and clear `FCertificatePEM`
  - when source carries non-empty `certificate_pem`, assign it and clear `FCertificateFile`
  - when source carries non-empty `private_key_file`, assign it and clear `FPrivateKeyPEM`
  - when source carries non-empty `private_key_pem`, assign it and clear `FPrivateKeyFile`
- Keep all other merge semantics unchanged.
- Do not add new fields to `ApplyPreset(...)`; it should continue delegating to `Merge(...)`.

**Run:**
- rerun the three focused regression commands above

**Expected:**
- PASS

## Task 3: Adjacent verification

**Files:**
- Test: `tests/config/test_config_snapshot_clone.pas`
- Test: `tests/config/test_batch_config.pas`
- Test: `tests/config/test_context_builder_pem_precedence_regression.pas`
- Test: `tests/config/test_context_builder_try.pas`
- Test: `tests/config/test_config_import_export.pas`
- Test: `tests/config/test_config_validation.pas`

**Run:**
- `mkdir -p tmp/config_snapshot_clone && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_snapshot_clone -FEtmp/config_snapshot_clone -otmp/config_snapshot_clone/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/config_snapshot_clone/test_config_snapshot_clone`
- `mkdir -p tmp/batch_config && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/batch_config -FEtmp/batch_config -otmp/batch_config/test_batch_config tests/config/test_batch_config.pas && ./tmp/batch_config/test_batch_config`
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
- Record root cause as merge-surface mutual-exclusion drift inherited by `ApplyPreset(...)`.
- Roll next queue to the next highest-value builder mutation gap after merge/apply-preset parity is closed.
