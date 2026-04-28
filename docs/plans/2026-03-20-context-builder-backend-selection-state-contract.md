# Context Builder Backend Selection State Contract Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Lock down how `TSSLContextBuilder` snapshot/reset operations handle backend selection state, then fix any drift so `Clone` preserves explicit backend choices and `Reset` truly restores constructor defaults.

**Architecture:** Use focused config tests to distinguish two contracts:
- `Clone` should preserve explicit backend selection, just like it preserves certificate/protocol/session fields.
- `Reset` should restore the same backend-selection defaults as a fresh builder, rather than leaking a previously pinned backend.

Use an intentionally unavailable backend in the test (`sslWinSSL` in the current test harness) so contract drift produces an observable result instead of silently falling back to auto-detect.

**Tech Stack:** Free Pascal, `tests/config/test_config_snapshot_clone.pas`, `src/fafafa.ssl.context.builder.pas`

---

### Task 1: RED - Add focused backend-state regression tests

**Files:**
- Modify: `tests/config/test_config_snapshot_clone.pas`

**Step 1: Add clone/reset backend-state tests**

- Add a focused test asserting `Clone` preserves an explicit backend pin.
- Add a focused test asserting `Reset` clears a previously pinned backend and returns to constructor defaults.

**Step 2: Run focused RED**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/test_config_snapshot_clone`

Expected:
- FAIL
- Clone test should show the cloned builder no longer behaves like the original explicit-backend builder
- Reset test should show reset still leaks the old backend pin

### Task 2: GREEN - Fix builder state copying/reset

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Make backend-selection state explicit in snapshot/reset logic**

- Update `Clone` to copy all backend-selection fields:
  - `FAutoSelectBackend`
  - `FBackendRequirements`
  - `FExplicitBackend`
  - `FExplicitBackendSet`
- Update `Reset` to restore the same backend-selection defaults as `constructor Create`.

**Step 2: Re-run focused regression**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/test_config_snapshot_clone`

Expected:
- PASS

### Task 3: Adjacent config regressions

**Files:**
- Test: `tests/config/test_config_snapshot_clone.pas`
- Test: `tests/config/test_preset_configurations.pas`
- Test: `tests/config/test_context_builder_try.pas`

**Step 1: Re-run adjacent config tests**

Run:
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_preset_configurations tests/config/test_preset_configurations.pas && ./tmp/test_preset_configurations`
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/test_context_builder_try`

Expected:
- PASS

### Task 4: Compile verification and writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run compile verification**

Run:
`python3 -u scripts/compile_all_modules.py`

Expected:
- PASS

**Step 2: Record the semantic contract**

- Note whether backend-selection state is part of the builder snapshot contract.
- Roll the next queue based on whether `Merge` / import-export still need backend-state coverage.
