# FreePascal Context ServerName Inheritance Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Restore legacy context-level `ServerName` compatibility for the FreePascal backend so `BuildClient.WithSNI(...)` and `Ctx.SetServerName(...)` are inherited by newly created client connections, matching the existing behavior of other backends.

**Architecture:** Treat this as a narrow backend-consistency fix, not an API-removal batch. Add a focused FreePascal-only regression that observes `ISSLClientConnection.GetServerName` immediately after connection creation for both socket and stream constructors, then update the two `TFreePascalConnection.Create(...)` overloads to copy the context default server name into the connection state.

**Tech Stack:** Free Pascal, backend-specific regression test, no network handshake required

---

### Task 1: Add focused RED regression

**Files:**
- Add: `tests/test_freepascal_context_server_name_inheritance.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`

**Step 1: Write the failing test**

- Add one builder-path case:
  - `WithBackend(sslFreePascal)`
  - `WithSNI('ctx.example.com')`
  - `BuildClient`
  - `CreateConnection(THandle(-1))`
  - assert the resulting `ISSLClientConnection.GetServerName` inherits `ctx.example.com`
- Add one direct-context stream case:
  - `TSSLFactory.CreateContext(sslCtxClient, sslFreePascal)`
  - `Ctx.SetServerName('stream.example.com')`
  - `CreateConnection(TMemoryStream.Create)`
  - assert the resulting `ISSLClientConnection.GetServerName` inherits `stream.example.com`

**Step 2: Run the test to confirm RED**

Run:
`fpc -Fu./src -otmp/test_freepascal_context_server_name_inheritance tests/test_freepascal_context_server_name_inheritance.pas && ./tmp/test_freepascal_context_server_name_inheritance`

Expected:
- FAIL because current `TFreePascalConnection.Create(...)` overloads initialize `FServerName := ''` and never copy the context default

### Task 2: Minimal backend fix

**Files:**
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Restore constructor inheritance**

- In both `TFreePascalConnection.Create(...)` overloads:
  - keep the current initialization logic
  - but when `AContext.GetServerName <> ''`, copy that value into the new connection before handshake
- Do not redesign `WithSNI`, `BuildClient`, or connector precedence in this batch

**Step 2: Re-run the RED test**

Run:
`fpc -Fu./src -otmp/test_freepascal_context_server_name_inheritance tests/test_freepascal_context_server_name_inheritance.pas && ./tmp/test_freepascal_context_server_name_inheritance`

Expected:
- PASS

### Task 3: Focused regression verification

**Files:**
- Test: `tests/test_freepascal_context_server_name_inheritance.pas`
- Test: `tests/test_tls_connector_hostname_override_precedence.pas`
- Test: `tests/test_connection_builder_hostname_precedence.pas`

**Step 1: Run adjacent regressions**

Run:
- `fpc -Fu./src -otmp/test_tls_connector_hostname_override_precedence tests/test_tls_connector_hostname_override_precedence.pas && ./tmp/test_tls_connector_hostname_override_precedence`
- `fpc -Fu./src -otmp/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence`

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

**Step 1: Record the root cause**

- Note that the compatibility boundary was not merely architectural debate:
  - OpenSSL / MbedTLS / WolfSSL already inherited context `ServerName`
  - FreePascal client connections did not

**Step 2: Roll the next queue**

- Revisit whether WinSSL / remaining factory paths need explicit contract tests for the same compatibility boundary.
- Keep the longer-term question of removing context-level SNI as a separate architectural decision.
