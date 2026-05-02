# Migration And Troubleshooting Connection-Level SNI Omissions Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Align selected active migration and troubleshooting client snippets with the canonical per-connection SNI path so they no longer show `CreateConnection(...)` followed by `Connect` without setting hostname on the client connection.

**Architecture:** Keep this batch docs-only and narrow. Add a focused shell contract over the selected snippets in `docs/guides/MIGRATION_GUIDE.md` and `docs/guides/TROUBLESHOOTING.md`, then update those examples with explicit `ISSLClientConnection` casts before `Connect`. Preserve the surrounding migration/session-reuse intent; do not redesign exception handling, verification policy, or socket helpers.

**Tech Stack:** Markdown docs, shell contract (`rg`)

---

### Task 1: Add RED contract

**Files:**
- Add: `tests/scripts/test_migration_troubleshooting_connection_level_sni_omissions_contract.sh`
- Reference: `docs/guides/MIGRATION_GUIDE.md`
- Reference: `docs/guides/TROUBLESHOOTING.md`

**Step 1: Write the failing contract**

- Require explicit client-connection SNI setup in the selected snippets:
  - OpenSSL migration client example
  - Indy migration client example
  - native OpenSSL C API migration example
  - troubleshooting session-resumption snippet

**Step 2: Run RED**

Run: `bash tests/scripts/test_migration_troubleshooting_connection_level_sni_omissions_contract.sh`

Expected:
- FAIL because the current snippets still omit explicit `ISSLClientConnection.SetServerName(...)`.

### Task 2: GREEN - Update selected docs

**Files:**
- Modify: `docs/guides/MIGRATION_GUIDE.md`
- Modify: `docs/guides/TROUBLESHOOTING.md`

**Step 1: Fix `docs/guides/MIGRATION_GUIDE.md`**

- In all three `fafafa.ssl` client snippets:
  - keep the existing `ISSLConnection` variable
  - add explicit `(LConn as ISSLClientConnection).SetServerName('example.com');`
  - keep the migration flow otherwise unchanged

**Step 2: Fix `docs/guides/TROUBLESHOOTING.md`**

- In the session-resumption snippet:
  - set connection-level SNI on `LConn1` before the first handshake
  - keep `LConn2.SetSession(...)` before setting SNI on `LConn2`
  - then handshake

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_migration_troubleshooting_connection_level_sni_omissions_contract.sh`
- Test: `docs/guides/MIGRATION_GUIDE.md`
- Test: `docs/guides/TROUBLESHOOTING.md`

**Step 1: Re-run contract**

Run: `bash tests/scripts/test_migration_troubleshooting_connection_level_sni_omissions_contract.sh`

Expected:
- PASS

**Step 2: Check rendered guidance**

Run: `rg -n "ISSLClientConnection|SetServerName\\(" docs/guides/MIGRATION_GUIDE.md docs/guides/TROUBLESHOOTING.md`

Expected:
- selected snippets now show explicit per-connection SNI guidance.

**Step 3: Diff hygiene**

Run: `git diff --check -- docs/plans/2026-03-24-migration-troubleshooting-connection-level-sni-omissions.md tests/scripts/test_migration_troubleshooting_connection_level_sni_omissions_contract.sh docs/guides/MIGRATION_GUIDE.md docs/guides/TROUBLESHOOTING.md task_plan.md findings.md progress.md`

Expected:
- PASS
