# Active Docs Context-Level SNI Cleanup Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove deprecated context-level `SetServerName(...)` guidance from active non-archive docs that still present it as the recommended path.

**Architecture:** Treat this as a docs-drift batch, not an API-removal batch. Add a focused shell contract for the selected active docs, then update those snippets so SNI is configured on the connection before `Connect`. Do not touch archive reports or compatibility-oriented tests in this batch.

**Tech Stack:** Markdown docs, shell contract test, focused `rg` verification

---

### Task 1: Add focused RED contract

**Files:**
- Add: `tests/scripts/test_active_docs_no_context_level_sni_guidance_contract.sh`

**Step 1: Write the failing contract**

- Limit scope to the currently active docs that still teach context-level SNI:
  - `docs/CA_CERTIFICATE_AUTO_LOADING.md`
  - `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
- Fail if those files still use:
  - `Context.SetServerName(...)`
  - `Ctx.SetServerName(...)`
  - `LCtx.SetServerName(...)`
  - `LContext.SetServerName(...)`

**Step 2: Run the contract to confirm RED**

Run:
`bash tests/scripts/test_active_docs_no_context_level_sni_guidance_contract.sh`

Expected:
- FAIL on the current stale doc snippets

### Task 2: Update the active docs

**Files:**
- Modify: `docs/CA_CERTIFICATE_AUTO_LOADING.md`
- Modify: `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`

**Step 1: Replace stale guidance**

- For connection snippets:
  - create the connection first
  - configure SNI on the connection
  - then call `Connect`
- For prose-only guidance:
  - explicitly say SNI/hostname is a connection-level setting

**Step 2: Re-run the contract**

Run:
`bash tests/scripts/test_active_docs_no_context_level_sni_guidance_contract.sh`

Expected:
- PASS

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_active_docs_no_context_level_sni_guidance_contract.sh`
- Test: `docs/CA_CERTIFICATE_AUTO_LOADING.md`
- Test: `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`

**Step 1: Run targeted grep**

Run:
`rg -n '\b(Context|Ctx|LCtx|LContext)\.SetServerName\(' docs/CA_CERTIFICATE_AUTO_LOADING.md docs/ZERO_DEPENDENCY_DEPLOYMENT.md`

Expected:
- No matches

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record why this batch was chosen**

- Note that runtime compatibility is still intentionally preserved, but active docs should not keep teaching the deprecated shared-context hostname path.

**Step 2: Roll the next queue**

- Keep the larger runtime compatibility boundary queued separately.
- After active-doc cleanup, re-audit remaining non-archive docs/examples for whether they are:
  - correct connection-level guidance
  - or explicit compatibility coverage that should be labeled as such
