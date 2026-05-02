# Secondary Guides Connection-Level SNI API Drift Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Align selected active secondary guides with the current client-SNI API contract so they no longer call `SetServerName(...)` directly on `ISSLConnection` variables.

**Architecture:** Keep this batch docs-only and focused on normal client connection flows. Add a shell contract over four selected guides where client snippets currently use `Conn.SetServerName(...)`, `Conn1.SetServerName(...)`, `Conn2.SetServerName(...)`, or `LConn.SetServerName(...)` directly on `ISSLConnection` variables. Then update those snippets to use explicit `ISSLClientConnection` casts before `Connect`, preserving the surrounding handshake/session/hostname-verification guidance. Do not reopen runtime SNI compatibility or generic capability docs in this batch.

**Tech Stack:** Markdown docs, shell contract (`rg`)

---

### Task 1: Add RED contract

**Files:**
- Add: `tests/scripts/test_secondary_guides_connection_level_sni_api_drift_contract.sh`
- Reference: `docs/guides/QUICKSTART.md`
- Reference: `docs/guides/COMMON_PITFALLS.md`
- Reference: `docs/guides/PERFORMANCE_PROFILING_GUIDE.md`
- Reference: `docs/guides/WINSSL_BEST_PRACTICES.md`

**Step 1: Write the failing contract**

- Fail if the selected guides still contain direct client SNI calls on generic connection variables:
  - `Conn.SetServerName(...)`
  - `Conn1.SetServerName(...)`
  - `Conn2.SetServerName(...)`
  - `LConn.SetServerName(...)`
- Also require the post-fix explicit client-connection pattern in those guides.

**Step 2: Run RED**

Run: `bash tests/scripts/test_secondary_guides_connection_level_sni_api_drift_contract.sh`

Expected:
- FAIL on the current direct `ISSLConnection.SetServerName(...)` drift.

### Task 2: GREEN - Update selected guides

**Files:**
- Modify: `docs/guides/QUICKSTART.md`
- Modify: `docs/guides/COMMON_PITFALLS.md`
- Modify: `docs/guides/PERFORMANCE_PROFILING_GUIDE.md`
- Modify: `docs/guides/WINSSL_BEST_PRACTICES.md`

**Step 1: Fix `docs/guides/QUICKSTART.md`**

- Replace the direct SNI setter calls in the WinSSL session reuse and multi-host cache examples with explicit `ISSLClientConnection` casts.
- Preserve the session-reuse flow; for the resumed connection keep `SetSession(...)` before `SetServerName(...)`.

**Step 2: Fix `docs/guides/COMMON_PITFALLS.md`**

- Update the “忘记设置 SNI” example so the “正确” path uses a client-connection cast before `Connect`.

**Step 3: Fix `docs/guides/PERFORMANCE_PROFILING_GUIDE.md`**

- Update the handshake benchmark snippet to set SNI through `ISSLClientConnection`.

**Step 4: Fix `docs/guides/WINSSL_BEST_PRACTICES.md`**

- Update the hostname-verification snippet to set SNI through `ISSLClientConnection`.

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_secondary_guides_connection_level_sni_api_drift_contract.sh`
- Test: `docs/guides/QUICKSTART.md`
- Test: `docs/guides/COMMON_PITFALLS.md`
- Test: `docs/guides/PERFORMANCE_PROFILING_GUIDE.md`
- Test: `docs/guides/WINSSL_BEST_PRACTICES.md`

**Step 1: Re-run contract**

Run: `bash tests/scripts/test_secondary_guides_connection_level_sni_api_drift_contract.sh`

Expected:
- PASS

**Step 2: Check rendered guidance**

Run: `rg -n "ISSLClientConnection|as ISSLClientConnection|SetServerName\\(" docs/guides/QUICKSTART.md docs/guides/COMMON_PITFALLS.md docs/guides/PERFORMANCE_PROFILING_GUIDE.md docs/guides/WINSSL_BEST_PRACTICES.md`

Expected:
- selected guides now show explicit client-connection SNI configuration.

**Step 3: Diff hygiene**

Run: `git diff --check -- docs/plans/2026-03-24-secondary-guides-connection-level-sni-api-drift.md tests/scripts/test_secondary_guides_connection_level_sni_api_drift_contract.sh docs/guides/QUICKSTART.md docs/guides/COMMON_PITFALLS.md docs/guides/PERFORMANCE_PROFILING_GUIDE.md docs/guides/WINSSL_BEST_PRACTICES.md task_plan.md findings.md progress.md`

Expected:
- PASS
