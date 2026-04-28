# Auto Backend Requirements Enforcement Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Ensure `TSSLContextBuilder` auto-backend selection fails when no available backend satisfies required capabilities, instead of silently picking the highest-scoring available backend anyway.

**Architecture:** Keep this batch tightly focused on selector enforcement.
- Scope in:
  - `SelectBestBackend(...)`
  - `TryBuildClient` observable contract through builder auto-selection
  - a requirement that is unsatisfied in the current config harness (`RequirePKCS11Support`)
- Scope out:
  - serialization/import/merge of auto-selection requirements
  - selector ranking heuristics
  - broader backend capability redesign

Current Linux config harness registers `sslFreePascal`, whose capability matrix reports `SupportsPKCS11 = False`. That makes `.RequirePKCS11Support` a stable observable RED:
- correct behavior: `TryBuildClient` should fail with “no suitable backend”
- current drift: builder still succeeds because selector ignores the “meets minimum requirements” gate

**Tech Stack:** Free Pascal, `tests/config/test_context_builder_try.pas`, `src/fafafa.ssl.backend.selector.pas`

---

### Task 1: RED - Add focused requirement enforcement regression

**Files:**
- Modify: `tests/config/test_context_builder_try.pas`

**Step 1: Add focused test**

- Add a test asserting `TSSLContextBuilder.Create.RequirePKCS11Support.TryBuildClient(...)` fails in the current config harness.
- Assert:
  - result is error
  - returned context is `nil`
  - error message mentions no suitable backend for requirements

**Step 2: Run focused RED**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/test_context_builder_try`

Expected:
- FAIL
- builder still succeeds even though no registered backend supports PKCS#11

### Task 2: GREEN - Enforce requirement gate in selector

**Files:**
- Modify: `src/fafafa.ssl.backend.selector.pas`

**Step 1: Fix selector behavior**

- `SelectBestBackend(...)` should only consider backends whose match details report `MeetsMinimumRequirements = True`.
- Preserve existing “best score wins” behavior among qualifying backends.

**Step 2: Re-run focused regression**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/test_context_builder_try`

Expected:
- PASS

### Task 3: Adjacent verification

**Files:**
- Test: `tests/config/test_config_import_export.pas`
- Test: `tests/config/test_preset_configurations.pas`

**Step 1: Re-run adjacent config tests**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export`

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_preset_configurations tests/config/test_preset_configurations.pas && ./tmp/test_preset_configurations`

Expected:
- PASS

### Task 4: Full verification

**Step 1: Re-run config audit**

Run:
`mkdir -p tmp/config_audit_logs_auto_backend_requirements && audit_rc=0; for f in tests/config/*.pas; do name=$(basename "$f" .pas); log="tmp/config_audit_logs_auto_backend_requirements/${name}.log"; if fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/${name} "$f" >"$log" 2>&1 && ./tmp/${name} >>"$log" 2>&1; then printf 'PASS %s\n' "$name"; else printf 'FAIL %s\n' "$name"; audit_rc=1; fi; done; exit $audit_rc`

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

**Step 1: Record root cause and evidence**

- capture why `SelectBestBackend` drifted from the documented requirement semantics
- capture the focused RED/green evidence

**Step 2: Queue the next batch**

- review whether the next highest-value backend-selection batch is:
  - auto-selection requirement serialization/import/merge
  - `SelectBestBackends` / `MeetsMinimumRequirements` consistency on minimum-score gates
