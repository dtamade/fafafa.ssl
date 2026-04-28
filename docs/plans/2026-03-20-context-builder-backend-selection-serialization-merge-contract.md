# Context Builder Backend Selection Serialization and Merge Contract Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Lock down how `TSSLContextBuilder` preserves explicit backend selection across `ExportToJSON` / `ImportFromJSON`, `ExportToINI` / `ImportFromINI`, and `Merge`, then fix the current drift with the smallest safe change.

**Architecture:** Treat this as a focused builder-state contract batch, not a broad configuration redesign.
- Scope in:
  - explicit backend pin set by `.WithBackend(...)`
  - JSON round-trip
  - INI round-trip
  - merge-from-source semantics
- Scope out for this batch:
  - `FAutoSelectBackend`
  - `FBackendRequirements`
  - backend auto-selection requirement serialization
  - broader preset/default config redesign

Use an intentionally unavailable backend in the current Linux harness (`sslWinSSL`) so lost backend state becomes directly observable:
- if the explicit backend pin survives, `TryBuildClient` should fail
- if the pin is dropped, the builder falls back to default selection and build may succeed

**Tech Stack:** Free Pascal, `tests/config/test_config_import_export.pas`, `tests/config/test_config_snapshot_clone.pas`, `src/fafafa.ssl.context.builder.pas`

---

### Task 1: RED - Add focused regression tests

**Files:**
- Modify: `tests/config/test_config_import_export.pas`
- Modify: `tests/config/test_config_snapshot_clone.pas`

**Step 1: Add JSON / INI round-trip tests**

- Add a JSON round-trip test proving `.WithBackend(sslWinSSL)` remains observable after export/import.
- Add an INI round-trip test proving `.WithBackend(sslWinSSL)` remains observable after export/import.

**Step 2: Add merge test**

- Add a merge test proving a source builder with `.WithBackend(sslWinSSL)` transfers explicit backend state into the destination builder.

**Step 3: Run focused RED**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export`

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/test_config_snapshot_clone`

Expected:
- FAIL
- imported builders should silently lose explicit backend pin
- merged builders should silently lose explicit backend pin

### Task 2: GREEN - Preserve explicit backend state in serialization/import/merge

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Extend builder state surface minimally**

- `ExportToJSON` should include explicit backend selection when set.
- `ImportFromJSON` should restore explicit backend selection when present.
- `ExportToINI` / `ImportFromINI` should do the same.
- `Merge` should apply explicit backend selection from source when source exported one.

**Step 2: Keep semantics narrow**

- Do not serialize auto-selection requirements in this batch.
- Do not change default builder behavior when no explicit backend is configured.

**Step 3: Re-run focused regressions**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export`

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/test_config_snapshot_clone`

Expected:
- PASS

### Task 3: Adjacent config regressions

**Files:**
- Test: `tests/config/test_preset_configurations.pas`
- Test: `tests/config/test_context_builder_try.pas`

**Step 1: Re-run adjacent config tests**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_preset_configurations tests/config/test_preset_configurations.pas && ./tmp/test_preset_configurations`

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/test_context_builder_try`

Expected:
- PASS

### Task 4: Full verification

**Step 1: Re-run config audit**

Run:
`mkdir -p tmp/config_audit_logs_backend_serialization && audit_rc=0; for f in tests/config/*.pas; do name=$(basename "$f" .pas); log="tmp/config_audit_logs_backend_serialization/${name}.log"; if fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/${name} "$f" >"$log" 2>&1 && ./tmp/${name} >>"$log" 2>&1; then printf 'PASS %s\n' "$name"; else printf 'FAIL %s\n' "$name"; audit_rc=1; fi; done; exit $audit_rc`

**Step 2: Re-run full module compile gate**

Run:
`python3 -u scripts/compile_all_modules.py`

Expected:
- all `tests/config/*.pas` pass
- compile gate passes

### Task 5: Documentation of findings and next queue

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record outcome**

- capture the root cause
- capture the exact verification evidence
- record what remains intentionally deferred

**Step 2: Queue the next review batch**

- decide whether the next highest-value review target is:
  - backend auto-selection requirement serialization
  - legacy context-level SNI runtime contract
  - another config-surface drift exposed by the full audit
