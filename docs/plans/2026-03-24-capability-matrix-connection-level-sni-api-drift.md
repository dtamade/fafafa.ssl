# Capability Matrix Connection-Level SNI API Drift Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Align the active capability/matrix docs with the current client-SNI API contract so they no longer call `SetServerName(...)` directly on `ISSLConnection` variables.

**Architecture:** Keep this batch docs-only. Split the targeted files into two shapes: straightforward backend/client examples, and one generic `ConfigureConnection(Conn: ISSLConnection)` capability snippet. For backend/client examples, replace direct `Conn.SetServerName(...)` calls with explicit `ISSLClientConnection` casts before `Connect`. For the generic capability snippet, keep the generic `ISSLConnection` parameter but use `Supports(..., ISSLClientConnection, ...)` before applying SNI. Do not mix in runtime or builder changes.

**Tech Stack:** Markdown docs, shell contract (`rg`)

---

### Task 1: Add RED contract

**Files:**
- Add: `tests/scripts/test_capability_matrix_connection_level_sni_api_drift_contract.sh`
- Reference: `docs/CAPABILITY_MATRIX_GUIDE.md`
- Reference: `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
- Reference: `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`

**Step 1: Write the failing contract**

- Fail if the selected docs still contain direct SNI setter calls on generic connection variables:
  - `Conn.SetServerName(...)`
  - `Conn1.SetServerName(...)`
  - `Conn2.SetServerName(...)`
- Require the post-fix explicit client-connection pattern:
  - client cast in backend examples
  - `Supports(..., ISSLClientConnection, ...)` in the generic capability snippet

**Step 2: Run RED**

Run: `bash tests/scripts/test_capability_matrix_connection_level_sni_api_drift_contract.sh`

Expected:
- FAIL on the current direct `ISSLConnection.SetServerName(...)` drift.

### Task 2: GREEN - Update selected docs

**Files:**
- Modify: `docs/CAPABILITY_MATRIX_GUIDE.md`
- Modify: `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
- Modify: `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`

**Step 1: Fix backend/client examples**

- In the MbedTLS and WinSSL backend capability matrix examples:
  - keep the `ISSLConnection` variables
  - use explicit `ISSLClientConnection` casts before `Connect`
  - in the WinSSL session reuse example, keep `SetSession(...)` before `SetServerName(...)`

**Step 2: Fix the generic capability snippet**

- In `docs/CAPABILITY_MATRIX_GUIDE.md`:
  - keep `procedure ConfigureConnection(Conn: ISSLConnection)`
  - add a local `ClientConn: ISSLClientConnection`
  - only call `ClientConn.SetServerName(...)` when `Supports(Conn, ISSLClientConnection, ClientConn)` succeeds

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_capability_matrix_connection_level_sni_api_drift_contract.sh`
- Test: `docs/CAPABILITY_MATRIX_GUIDE.md`
- Test: `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
- Test: `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`

**Step 1: Re-run contract**

Run: `bash tests/scripts/test_capability_matrix_connection_level_sni_api_drift_contract.sh`

Expected:
- PASS

**Step 2: Check rendered guidance**

Run: `rg -n "ISSLClientConnection|Supports\\(|SetServerName\\(" docs/CAPABILITY_MATRIX_GUIDE.md docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`

Expected:
- selected docs now show either explicit client casts or guarded generic-client support.

**Step 3: Diff hygiene**

Run: `git diff --check -- docs/plans/2026-03-24-capability-matrix-connection-level-sni-api-drift.md tests/scripts/test_capability_matrix_connection_level_sni_api_drift_contract.sh docs/CAPABILITY_MATRIX_GUIDE.md docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md task_plan.md findings.md progress.md`

Expected:
- PASS
