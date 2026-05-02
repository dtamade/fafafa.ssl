# API Reference Connection-Level SNI Omissions Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Align the active client-side examples in `docs/reference/API_REFERENCE.md` with the canonical per-connection SNI path so the API reference no longer shows `CreateConnection(...)` followed by `Connect` without setting hostname on the client connection.

**Architecture:** Keep this batch docs-only and scoped to one reference document. Add a focused shell contract over the client-side `CreateConnection(...)` examples in `docs/reference/API_REFERENCE.md`, then update those snippets with explicit `ISSLClientConnection.SetServerName(...)` guidance before `Connect`. Preserve the surrounding API-reference intent; do not redesign monitoring/session semantics, socket helpers, or runtime behavior.

**Tech Stack:** Markdown docs, shell contract (`rg`)

---

### Task 1: Add RED contract

**Files:**
- Add: `tests/scripts/test_api_reference_connection_level_sni_omissions_contract.sh`
- Reference: `docs/reference/API_REFERENCE.md`

**Step 1: Write the failing contract**

- Require explicit client-connection SNI setup for the API reference client examples:
  - generic `MySocket` connection snippets
  - WinSSL session reuse snippets
  - multi-host session cache snippet
  - the full end-to-end client example near the OpenSSL library walkthrough

**Step 2: Run RED**

Run: `bash tests/scripts/test_api_reference_connection_level_sni_omissions_contract.sh`

Expected:
- FAIL because the current API reference examples still omit explicit `ISSLClientConnection.SetServerName(...)`.

### Task 2: GREEN - Update API reference examples

**Files:**
- Modify: `docs/reference/API_REFERENCE.md`

**Step 1: Fix generic client snippets**

- In the `MySocket` examples:
  - keep the existing `ISSLConnection` variable
  - add `LServerName := 'example.com';`
  - add explicit `(LConn as ISSLClientConnection).SetServerName(LServerName);`
  - keep the surrounding API example intent unchanged

**Step 2: Fix WinSSL session snippets**

- In the basic session-resume snippet:
  - set connection-level SNI on `LConn1` before the first handshake
  - keep `LConn2.SetSession(...)` before setting SNI on `LConn2`
  - then handshake
- In the multi-host cache snippet:
  - keep `SetSession(...)` first when a cached session exists
  - set SNI from `LHost` before `Connect`

**Step 3: Fix the full client example**

- In the final end-to-end OpenSSL library example:
  - use the same `LServerName` value for `SetServerName(...)`
  - keep the later certificate hostname verification aligned with that same hostname

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_api_reference_connection_level_sni_omissions_contract.sh`
- Test: `docs/reference/API_REFERENCE.md`

**Step 1: Re-run contract**

Run: `bash tests/scripts/test_api_reference_connection_level_sni_omissions_contract.sh`

Expected:
- PASS

**Step 2: Check rendered guidance**

Run: `rg -n "ISSLClientConnection|SetServerName\\(|LServerName := 'example\\.com'" docs/reference/API_REFERENCE.md`

Expected:
- the selected API reference snippets now show explicit per-connection SNI guidance.

**Step 3: Diff hygiene**

Run: `git diff --check -- docs/plans/2026-03-24-api-reference-connection-level-sni-omissions.md tests/scripts/test_api_reference_connection_level_sni_omissions_contract.sh docs/reference/API_REFERENCE.md task_plan.md findings.md progress.md`

Expected:
- PASS
