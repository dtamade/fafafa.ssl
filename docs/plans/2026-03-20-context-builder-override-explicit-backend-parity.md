# Context Builder Override Explicit Backend Parity Plan

**Goal:** Restore `TSSLContextBuilder.Override(...)` parity for explicit backend selection so transform/override-based configuration can express the same backend pinning contract as `.WithBackend(...)`.

**Architecture:** Cover the gap at two levels:

- transformation/export state:
  - overriding `explicit_backend` should become export-visible
  - overriding `explicit_backend` on top of auto-backend state should clear stale `auto_select_backend`
- runtime behavior:
  - overriding `explicit_backend` on top of unmet auto-backend requirements should pin the requested backend and bypass stale auto-selection failure

Apply the smallest safe fix in `TSSLContextBuilderImpl.Override(...)` only:
- recognize `explicit_backend`
- accept case-insensitive backend names such as `sslFreePascal` / `sslWinSSL` plus numeric ordinals
- mirror `.WithBackend(...)` semantics by setting explicit backend state and clearing auto-selection
- keep invalid values as no-op

Do not add `auto_select_backend` / `backend_requirements` override parsing in this batch.

## Task 1: Add focused RED regressions

**Files:**
- Modify: `tests/test_transformation_methods.pas`
- Modify: `tests/config/test_context_builder_try.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`

**Steps:**
- Add an export-state regression proving:
  - `RequirePKCS11Support` creates auto-backend state
  - `Override('explicit_backend', 'sslWinSSL')` must replace that state with explicit backend export
- Add a runtime regression proving:
  - unmet auto-backend requirements fail before override
  - `Override('explicit_backend', 'sslFreePascal')` must now build successfully by clearing stale auto-selection

**Run:**
- `mkdir -p tmp/transformation_methods && fpc -B -Fu./src -FUtmp/transformation_methods -FEtmp/transformation_methods -otmp/transformation_methods/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/transformation_methods/test_transformation_methods`
- `mkdir -p tmp/context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_try -FEtmp/context_builder_try -otmp/context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/context_builder_try/test_context_builder_try`

**Expected:**
- FAIL before the fix because `Override(...)` still ignores `explicit_backend` and stale auto-selection remains active

## Task 2: Minimal override fix

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Steps:**
- Add a narrow backend parser for `explicit_backend` that accepts:
  - enum ordinals
  - case-insensitive symbolic names
- In `Override(...)`:
  - recognize `explicit_backend`
  - set `FExplicitBackend`
  - set `FExplicitBackendSet := True`
  - set `FAutoSelectBackend := False`
- Keep unknown-field and invalid-value no-op semantics unchanged.

## Task 3: Adjacent verification

**Files:**
- Test: `tests/test_transformation_methods.pas`
- Test: `tests/config/test_context_builder_try.pas`
- Test: `tests/config/test_config_import_export.pas`
- Test: `tests/config/test_config_snapshot_clone.pas`

**Run:**
- `mkdir -p tmp/transformation_methods && fpc -B -Fu./src -FUtmp/transformation_methods -FEtmp/transformation_methods -otmp/transformation_methods/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/transformation_methods/test_transformation_methods`
- `mkdir -p tmp/context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_try -FEtmp/context_builder_try -otmp/context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/context_builder_try/test_context_builder_try`
- `mkdir -p tmp/config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_import_export -FEtmp/config_import_export -otmp/config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/config_import_export/test_config_import_export`
- `mkdir -p tmp/config_snapshot_clone && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_snapshot_clone -FEtmp/config_snapshot_clone -otmp/config_snapshot_clone/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/config_snapshot_clone/test_config_snapshot_clone`
- `python3 scripts/compile_all_modules.py`

**Expected:**
- PASS

## Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Steps:**
- Record root cause:
  - builder/export/import/merge already preserved explicit backend state
  - override/transform still could not express the same runtime-significant contract
  - stale auto-backend state could silently mask explicit backend intent
- Queue the next highest-value override/external-config audit batch after this one.
