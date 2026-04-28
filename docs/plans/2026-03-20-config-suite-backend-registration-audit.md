# Config Suite Backend Registration Audit Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Audit `tests/config` for remaining ambient backend-registration dependencies and harden any failing runtime tests so they run against an explicit backend in the current environment.

**Architecture:** First run the config test suite to collect actual failures. Then fix only the failing test harnesses, not production runtime code, by linking the correct backend library and pinning runtime build paths to `sslFreePascal` where needed. Keep the batch focused on test reliability.

**Tech Stack:** Free Pascal, Pascal test programs, focused shell audit loop

---

### Task 1: Audit the config test suite

**Files:**
- Audit: `tests/config/*.pas`

**Step 1: Run the suite**

- Compile and run each Pascal test under `tests/config/`.
- Record which tests fail, and capture whether the failure is due to:
  - missing backend registration
  - real builder/config logic
  - unrelated environmental issues

**Step 2: Confirm the failing pattern**

Run:
`for f in tests/config/*.pas; do ...; done`

Expected:
- zero or more failing tests
- if failures are backend-noise, messages should mention no SSL library available or equivalent runtime selection issues

### Task 2: Harden failing test harnesses

**Files:**
- Modify only the failing tests identified in Task 1

**Step 1: Apply minimal harness fixes**

- Add `fafafa.ssl.freepascal.lib` to `uses` when a test needs explicit backend registration.
- Pin the runtime path with `.WithBackend(sslFreePascal)` only in the cases that actually build contexts.

**Step 2: Re-run the failing tests**

- Re-run just the previously failing tests until they pass for the right reason.

### Task 3: Re-run focused config regressions

**Files:**
- Test: each previously failing `tests/config/*.pas`
- Regression: recently touched config tests

**Step 1: Re-run the hardened subset**

- Re-run all tests that failed in Task 1.

**Step 2: Re-run the most recently touched config tests**

Run:
- `tests/config/test_config_validation.pas`
- `tests/config/test_config_import_export.pas`
- `tests/config/test_context_builder_try.pas`

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

**Step 2: Record what was learned**

- Note whether the remaining failures were real code defects or just ambient backend-registration drift in the test harness.
- Roll the next queue based on what the suite audit reveals.
