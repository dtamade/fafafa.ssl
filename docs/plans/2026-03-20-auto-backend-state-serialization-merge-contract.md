# Auto Backend State Serialization and Merge Contract Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Preserve `TSSLContextBuilder` auto-backend selection state across `ExportToJSON` / `ImportFromJSON`, `ExportToINI` / `ImportFromINI`, and `Merge`, so requirement-driven selection behavior remains observable after round-trip and composition.

**Architecture:** Treat this as the auto-selection counterpart to the earlier explicit-backend batch.
- Scope in:
  - `FAutoSelectBackend`
  - `FBackendRequirements`
  - JSON round-trip
  - INI round-trip
  - merge-from-source semantics
- Scope out:
  - selector ranking heuristics
  - clone/reset semantics (already covered)
  - legacy context-level SNI compatibility

Use the now-stable unmet requirement signal:
- `Requirements := CreateDefaultRequirements(optBalanced)`
- `Requirements.MinSecurityScore := 95`

Current harness with only `fafafa.ssl.freepascal.lib` registered in config tests gives:
- correct behavior when auto-state is preserved: `TryBuildClient` fails with “no suitable backend”
- current drift if auto-state is lost: builder falls back to normal default selection and succeeds

**Tech Stack:** Free Pascal, `tests/config/test_config_import_export.pas`, `tests/config/test_config_snapshot_clone.pas`, `src/fafafa.ssl.context.builder.pas`

---

### Task 1: RED - Add focused regression tests

**Files:**
- Modify: `tests/config/test_config_import_export.pas`
- Modify: `tests/config/test_config_snapshot_clone.pas`

**Step 1: Add JSON / INI round-trip tests**

- Add a JSON round-trip test proving unmet auto-backend requirements remain observable after export/import.
- Add an INI round-trip test proving the same.

**Step 2: Add merge test**

- Add a merge test proving a source builder with unmet auto-backend requirements transfers that active selection mode into the destination.

**Step 3: Run focused RED**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export`

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/test_config_snapshot_clone`

Expected:
- FAIL
- imported builders silently lose auto-backend requirements
- merged builders silently lose auto-backend requirements

### Task 2: GREEN - Preserve auto-backend state in serialization/import/merge

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Extend builder state surface**

- Export active auto-selection mode and full `TSSLRequirements` state to JSON.
- Import the same state from JSON.
- Export/import the same state to/from INI.
- Make `Merge(...)` apply the source auto-selection mode and requirements when source exported them.

**Step 2: Preserve active-mode semantics**

- When auto-selection is active, preserve that mode rather than reviving stale explicit backend pins.
- Do not change default behavior for builders that never enabled auto-selection.

**Step 3: Re-run focused regressions**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export`

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/test_config_snapshot_clone`

Expected:
- PASS

### Task 3: Adjacent verification

**Files:**
- Test: `tests/config/test_context_builder_try.pas`
- Test: `tests/test_backend_selector_minimum_score_filtering.pas`
- Test: `tests/config/test_preset_configurations.pas`

**Step 1: Re-run adjacent tests**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/test_context_builder_try`

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_backend_selector_minimum_score_filtering tests/test_backend_selector_minimum_score_filtering.pas && ./tmp/test_backend_selector_minimum_score_filtering`

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_preset_configurations tests/config/test_preset_configurations.pas && ./tmp/test_preset_configurations`

Expected:
- PASS

### Task 4: Full verification

**Step 1: Re-run config audit**

Run:
`mkdir -p tmp/config_audit_logs_auto_backend_state && audit_rc=0; for f in tests/config/*.pas; do name=$(basename "$f" .pas); log="tmp/config_audit_logs_auto_backend_state/${name}.log"; if fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/${name} "$f" >"$log" 2>&1 && ./tmp/${name} >>"$log" 2>&1; then printf 'PASS %s\n' "$name"; else printf 'FAIL %s\n' "$name"; audit_rc=1; fi; done; exit $audit_rc`

**Step 2: Re-run full module compile gate**

Run:
`python3 -u scripts/compile_all_modules.py`

Expected:
- all `tests/config/*.pas` pass
- compile gate passes

### Task 5: Documentation and next queue

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record evidence**

- capture the auto-state root cause
- record the focused RED/green evidence
- note any intentional compatibility choices

**Step 2: Queue the next batch**

- after state serialization is closed, the next likely review target is:
  - legacy context-level SNI runtime contract
