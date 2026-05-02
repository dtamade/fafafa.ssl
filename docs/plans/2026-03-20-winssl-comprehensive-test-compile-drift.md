# WinSSL Comprehensive Test Compile Drift Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Repair compile drifts surfaced by focused verification of the WinSSL comprehensive context-labeling batch, without changing the behavioral intent of those tests.

**Architecture:** Treat this as a narrow test-only API-alignment batch. Use the failed focused compile commands as RED evidence, then update the affected tests to match the current public API:
- use optional native-handle access instead of direct `ISSLContext.GetNativeHandle`
- replace removed/renamed helper calls in `tests/unit/test_winssl_comprehensive.pas`
- align callback setup with the current `of object` callback signatures

**Tech Stack:** Pascal tests, focused Win64 cross-compile verification

---

### Task 1: RED - Confirm current compile failures

**Files:**
- Reference: `tests/winssl/test_winssl_context_comprehensive.pas`
- Reference: `tests/unit/test_winssl_comprehensive.pas`

**Step 1: Use focused compile failures as RED evidence**

Run:
- `fpc -Twin64 -Fu./src -otmp/test_winssl_context_comprehensive.exe tests/winssl/test_winssl_context_comprehensive.pas`
- `fpc -Twin64 -Fu./src -otmp/test_unit_winssl_comprehensive.exe tests/unit/test_winssl_comprehensive.pas`

Expected:
- `test_winssl_context_comprehensive` fails on removed `ISSLContext.GetNativeHandle`
- `test_unit_winssl_comprehensive` fails on a cluster of stale API names/signatures

### Task 2: GREEN - Apply minimal API-alignment fixes

**Files:**
- Modify: `tests/winssl/test_winssl_context_comprehensive.pas`
- Modify: `tests/unit/test_winssl_comprehensive.pas`

**Step 1: Fix native-handle access**

- In `test_winssl_context_comprehensive`, use `Supports(..., ISSLNativeHandleAccess, ...)`.
- Keep the test intent: assert that WinSSL context exposes a non-nil native handle.

**Step 2: Fix stale API drift in unit comprehensive test**

- Import the unit that exposes `TSSLOptions` and option flags.
- Replace removed `IsAvailable` with a current availability/initialization check.
- Replace removed certificate-store counter call with the current count API.
- Align callback helpers with the current `TSSLVerifyCallback` / `TSSLPasswordCallback` signatures.
- Keep the test scope as API coverage; do not redesign the suite.

### Task 3: Focused verification

**Files:**
- Test: `tests/winssl/test_winssl_context_comprehensive.pas`
- Test: `tests/unit/test_winssl_comprehensive.pas`

**Step 1: Re-run focused checks**

Run:
- `fpc -Twin64 -Fu./src -otmp/test_winssl_context_comprehensive.exe tests/winssl/test_winssl_context_comprehensive.pas`
- `fpc -Twin64 -Fu./src -otmp/test_unit_winssl_comprehensive.exe tests/unit/test_winssl_comprehensive.pas`

Expected:
- both files cross-compile successfully on the local Linux host targeting Win64

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record the root causes**

- `ISSLContext.GetNativeHandle` moved behind `ISSLNativeHandleAccess`
- the older unit comprehensive suite still referenced multiple retired names/signatures

**Step 2: Roll the next queue**

- After this drift batch, continue classifying the remaining framework/server-side context-level SNI files.
