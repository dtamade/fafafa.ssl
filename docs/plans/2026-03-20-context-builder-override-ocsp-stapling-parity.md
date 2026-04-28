# Context Builder Override OCSP Stapling Parity Plan

**Goal:** Restore `TSSLContextBuilder.Override(...)` parity for OCSP stapling state and normalize the OCSP disable path so override/transform-based configuration and fluent builder calls share one self-consistent boolean contract.

**Architecture:** Cover the gap at two levels:

- transformation/export state:
  - overriding `ocsp_stapling_required=true` should become export-visible and also imply `ocsp_stapling_enabled=true`
  - overriding `ocsp_stapling_enabled=false` on top of required state should clear both enabled and required
- fluent/builder state:
  - `.WithOCSPStaplingRequired(True).WithOCSPStapling(False)` should also clear both enabled and required instead of letting stale required state reassert itself
- runtime behavior:
  - override-configured OCSP-required state must persist into built context options as both:
    - `ssoRequireOCSPStapling`
    - `ssoEnableOCSPStapling`

Apply the smallest safe fix in `TSSLContextBuilderImpl`:
- recognize `ocsp_stapling_enabled`
- recognize `ocsp_stapling_required`
- route override updates through the same OCSP state transitions used by fluent builder calls
- normalize `WithOCSPStapling(False)` so it clears stale required state before `SyncOCSPStaplingOptions` runs
- keep unknown-field no-op semantics unchanged

Do not change import/export/merge/build semantics in this batch.

## Task 1: Add focused RED regressions

**Files:**
- Modify: `tests/test_transformation_methods.pas`
- Modify: `tests/config/test_context_builder_try.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`

**Steps:**
- Add export-state regressions proving:
  - `Override('ocsp_stapling_required', 'true')` makes both OCSP fields export-visible as enabled+required
  - `Override('ocsp_stapling_enabled', 'false')` clears stale required state
- Add a fluent-state regression proving:
  - `.WithOCSPStaplingRequired(True).WithOCSPStapling(False)` clears stale required state instead of preserving contradictory OCSP booleans
- Add runtime regression proving:
  - override-configured OCSP-required state persists into built context options

**Run:**
- `mkdir -p tmp/transformation_methods && fpc -B -Fu./src -FUtmp/transformation_methods -FEtmp/transformation_methods -otmp/transformation_methods/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/transformation_methods/test_transformation_methods`
- `mkdir -p tmp/context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_try -FEtmp/context_builder_try -otmp/context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/context_builder_try/test_context_builder_try`

**Expected:**
- FAIL before the fix because the OCSP disable path still depends on stale option state

## Task 2: Minimal builder fix

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Steps:**
- Extend `Override(...)` to recognize:
  - `ocsp_stapling_enabled`
  - `ocsp_stapling_required`
- Route override OCSP updates through the same builder state transitions used by the fluent API.
- In `WithOCSPStapling(False)`, clear stale required state before `SyncOCSPStaplingOptions` runs.

## Task 3: Adjacent verification

**Files:**
- Test: `tests/test_transformation_methods.pas`
- Test: `tests/config/test_context_builder_try.pas`
- Test: `tests/config/test_config_import_export.pas`
- Test: `tests/config/test_config_snapshot_clone.pas`
- Test: `tests/test_ocsp_stapling_integration.lpr`

**Run:**
- `mkdir -p tmp/transformation_methods && fpc -B -Fu./src -FUtmp/transformation_methods -FEtmp/transformation_methods -otmp/transformation_methods/test_transformation_methods tests/test_transformation_methods.pas && ./tmp/transformation_methods/test_transformation_methods`
- `mkdir -p tmp/context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_try -FEtmp/context_builder_try -otmp/context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/context_builder_try/test_context_builder_try`
- `mkdir -p tmp/config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_import_export -FEtmp/config_import_export -otmp/config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/config_import_export/test_config_import_export`
- `mkdir -p tmp/config_snapshot_clone && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_snapshot_clone -FEtmp/config_snapshot_clone -otmp/config_snapshot_clone/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/config_snapshot_clone/test_config_snapshot_clone`
- `mkdir -p tmp/ocsp_stapling_integration && fpc -B -Fu./src -FUtmp/ocsp_stapling_integration -FEtmp/ocsp_stapling_integration -otmp/ocsp_stapling_integration/test_ocsp_stapling_integration tests/test_ocsp_stapling_integration.lpr && ./tmp/ocsp_stapling_integration/test_ocsp_stapling_integration`
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
  - stale OCSP option state could reassert itself during disable flows
  - override parity and fluent disable semantics had to converge on one state machine
- Record any independent verification findings surfaced while refreshing OCSP coverage.
- Queue the next highest-value override/external-config audit batch after this one.
