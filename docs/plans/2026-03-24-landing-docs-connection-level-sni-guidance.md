# Landing Docs Connection-Level SNI Guidance Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Align the most visible landing/client-setup docs with the project's canonical per-connection SNI guidance so normal client flows no longer imply `CreateConnection(...)` followed by `Connect` is sufficient without setting hostname on the connection.

**Architecture:** Keep this batch docs-only. Add a focused shell contract over three primary docs: `docs/README.md`, `docs/guides/INTEGRATION_GUIDE.md`, and `docs/guides/USER_GUIDE.md`. Then update only their client-side snippets/prose to explicitly cast to `ISSLClientConnection` and call `SetServerName(...)` before `Connect`, matching the already-canonical examples in `docs/INTEGRATION_GUIDE.md` and `docs/guides/GETTING_STARTED.md`.

**Tech Stack:** Markdown docs, shell contract (`rg`)

---

### Task 1: Add RED contract

**Files:**
- Add: `tests/scripts/test_landing_docs_connection_level_sni_guidance_contract.sh`
- Reference: `docs/README.md`
- Reference: `docs/guides/INTEGRATION_GUIDE.md`
- Reference: `docs/guides/USER_GUIDE.md`
- Reference: `docs/INTEGRATION_GUIDE.md`
- Reference: `docs/guides/GETTING_STARTED.md`

**Step 1: Write the failing contract**

- Require each targeted doc to show the client-side connection-level SNI pattern:
  - `ISSLClientConnection`
  - cast from `CreateConnection(...)` result
  - `SetServerName(...)` before `Connect`
- Keep scope limited to those three landing docs.

**Step 2: Run RED**

Run: `bash tests/scripts/test_landing_docs_connection_level_sni_guidance_contract.sh`

Expected:
- FAIL because the current quickstart/integration/client snippets still omit explicit per-connection SNI.

### Task 2: GREEN - Update landing docs

**Files:**
- Modify: `docs/README.md`
- Modify: `docs/guides/INTEGRATION_GUIDE.md`
- Modify: `docs/guides/USER_GUIDE.md`

**Step 1: Update `docs/README.md` quickstart**

- Add `ClientConn: ISSLClientConnection`.
- Cast after `CreateConnection(YourSocket)`.
- Call `ClientConn.SetServerName('example.com')`.
- Add one short note that SNI/hostname is connection-level, not shared-context configuration.

**Step 2: Update `docs/guides/INTEGRATION_GUIDE.md` blocking client example**

- Introduce `LClientConn: ISSLClientConnection`.
- Configure SNI on the connection before `Connect`.
- Keep the blocking integration flow and read/write example otherwise unchanged.

**Step 3: Update `docs/guides/USER_GUIDE.md` HTTPS client example**

- Introduce `LClientConn: ISSLClientConnection`.
- Configure `SetServerName('example.com')` before `Connect`.
- Keep the rest of the example intact; do not redesign the server or certificate-management sections.

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_landing_docs_connection_level_sni_guidance_contract.sh`
- Test: `docs/README.md`
- Test: `docs/guides/INTEGRATION_GUIDE.md`
- Test: `docs/guides/USER_GUIDE.md`

**Step 1: Re-run contract**

Run: `bash tests/scripts/test_landing_docs_connection_level_sni_guidance_contract.sh`

Expected:
- PASS

**Step 2: Check rendered guidance**

Run: `rg -n "ISSLClientConnection|SetServerName\\(|连接级|per-connection" docs/README.md docs/guides/INTEGRATION_GUIDE.md docs/guides/USER_GUIDE.md`

Expected:
- targeted docs now show the canonical client-side SNI pattern.

**Step 3: Diff hygiene**

Run: `git diff --check -- docs/plans/2026-03-24-landing-docs-connection-level-sni-guidance.md tests/scripts/test_landing_docs_connection_level_sni_guidance_contract.sh docs/README.md docs/guides/INTEGRATION_GUIDE.md docs/guides/USER_GUIDE.md task_plan.md findings.md progress.md`

Expected:
- PASS
