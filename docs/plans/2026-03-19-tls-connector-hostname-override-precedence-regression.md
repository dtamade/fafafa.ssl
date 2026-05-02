# TLS Connector Hostname Override Precedence Regression Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Restore `TSSLConnector.ConnectStream/ConnectSocket(..., '')` so an explicit empty hostname clears inherited context `ServerName` instead of silently keeping the fallback.

**Architecture:** Add a focused mock-based Pascal regression test for the connector path, then apply the smallest possible fix in `TSSLConnector.ApplyClientOptions(...)`. Keep the behavior aligned with the already-hardened builder precedence: `connection override > context default > empty`.

**Tech Stack:** Free Pascal, Pascal unit tests, mock `ISSLContext` / `ISSLClientConnection`

---

### Task 1: Add connector precedence RED

**Files:**
- Add: `tests/test_tls_connector_hostname_override_precedence.pas`
- Reference: `tests/test_connection_builder_hostname_precedence.pas`

**Step 1: Write the failing test**

- Create a mock `ISSLContext` that injects context default `ServerName` into each new mock client connection.
- Create a mock `ISSLClientConnection` whose `SetServerName/GetServerName` is directly observable.
- Add two connector cases:
  - `ConnectStream(..., 'override.example')` overrides inherited fallback
  - `ConnectStream(..., '')` explicitly clears inherited fallback

**Step 2: Run test to verify it fails**

Run:
`fpc -Fu./src -otmp/test_tls_connector_hostname_override_precedence tests/test_tls_connector_hostname_override_precedence.pas && ./tmp/test_tls_connector_hostname_override_precedence`

Expected:
- FAIL with the empty-override case still showing inherited `ctx.example.com`

### Task 2: Minimal connector fix

**Files:**
- Modify: `src/fafafa.ssl.tls.pas`

**Step 1: Write minimal implementation**

- Update `TSSLConnector.ApplyClientOptions(...)` so supported client connections always receive `SetServerName(AServerName)`, including `''`.
- Preserve the existing unsupported-backend error only for non-empty hostnames.

**Step 2: Run the RED test again**

Run:
`fpc -Fu./src -otmp/test_tls_connector_hostname_override_precedence tests/test_tls_connector_hostname_override_precedence.pas && ./tmp/test_tls_connector_hostname_override_precedence`

Expected:
- PASS

### Task 3: Focused regression verification

**Files:**
- Test: `tests/test_tls_connector_hostname_override_precedence.pas`
- Test: `tests/test_connection_builder_hostname_precedence.pas`
- Test: `tests/test_new_api.pas`

**Step 1: Run adjacent regressions**

Run:
- `fpc -Fu./src -otmp/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence`
- `fpc -Fu./src -otmp/test_new_api tests/test_new_api.pas && ./tmp/test_new_api`

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

**Step 1: Record root cause and RED/GREEN evidence**

- Note that the focused connector test was missing from the current tree while `ApplyClientOptions(...)` had regressed to the old `AServerName <> ''` guard.

**Step 2: Mark batch complete and roll next queue**

- Next queue should leave this connector precedence gap and return to the next backend behavior module.
