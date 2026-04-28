# Context Builder SNI Deprecation Warning Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Make `TSSLContextBuilder.ValidateClient` warn when `.WithSNI(...)` configures deprecated context-level SNI, while preserving the current backward-compatible runtime behavior.

**Architecture:** Keep compatibility semantics intact. Add a focused validation regression for `WithSNI(...)`, then update validation only so the builder surfaces the same guidance already documented elsewhere: prefer connection-level `ISSLClientConnection.SetServerName(...)` or `TSSLConnector.Connect*(..., ServerName)`.

**Tech Stack:** Free Pascal, builder validation tests

---

### Task 1: Add focused RED regression

**Files:**
- Modify: `tests/config/test_config_validation.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`

**Step 1: Write the failing test**

- Add a client validation test using:
  - `WithTLS12And13`
  - `WithVerifyPeer`
  - `WithSystemRoots`
  - `WithSNI('example.com')`
- Assert:
  - config remains valid
  - validation emits a warning that context-level SNI is deprecated

**Step 2: Run test to verify it fails**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation`

Expected:
- FAIL because validation does not yet mention deprecated context-level SNI

### Task 2: Minimal validation fix

**Files:**
- Modify: `src/fafafa.ssl.context.builder.pas`

**Step 1: Add the warning**

- In `ValidateClient`, when `FServerName <> ''`:
  - add one warning that `.WithSNI(...)` configures deprecated context-level SNI
  - point readers to per-connection SNI via `ISSLClientConnection.SetServerName(...)`

**Step 2: Re-run the RED test**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_validation tests/config/test_config_validation.pas && ./tmp/test_config_validation`

Expected:
- PASS

### Task 3: Focused regression verification

**Files:**
- Test: `tests/config/test_config_validation.pas`
- Test: `tests/config/test_config_import_export.pas`
- Test: `tests/config/test_context_builder_try.pas`

**Step 1: Run adjacent regressions**

Run:
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/test_config_import_export`
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/test_context_builder_try`

Expected:
- PASS

**Step 2: Run compile verification**

Run:
`python3 -u scripts/compile_all_modules.py`

Expected:
- PASS

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record why this change was chosen**

- Note that removing context-level SNI entirely is a compatibility decision, but missing validation guidance is a low-risk drift that can be fixed immediately.

**Step 2: Roll the next queue**

- Keep the larger runtime compatibility question queued for a separate architectural batch if it still matters after validation alignment.
