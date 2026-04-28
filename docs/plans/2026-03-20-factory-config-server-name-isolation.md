# Factory Config ServerName Isolation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Ensure `TSSLFactory.CreateContext(const AConfig: TSSLConfig)` treats `AConfig` as one-shot context input instead of mutating the shared library default config and leaking `ServerName` into later contexts.

**Architecture:** Add a focused factory regression around the FreePascal backend because it is available in the local Linux harness and now reliably inherits context `ServerName` into connections. Lock two contracts:
- explicit `ISSLLibrary.SetDefaultConfig(...)` should still affect later default-path contexts
- one-shot `TSSLFactory.CreateContext(const AConfig)` must not persist `ServerName` into the shared default config used by later `CreateContext(AContextType, ALibType)`

**Tech Stack:** Free Pascal, factory regression test, no network handshake required

---

### Task 1: Add focused RED regression

**Files:**
- Add: `tests/test_factory_config_server_name_isolation.pas`
- Reference: `src/fafafa.ssl.factory.pas`

**Step 1: Write the failing test**

- Use `sslFreePascal` so the test is Linux-safe and does not depend on external TLS libraries
- Save the current library default config at the start of each case and restore it in `finally`
- Add one passing control case:
  - explicit `Lib.SetDefaultConfig(...)` with `ServerName := 'default.example.com'`
  - `TSSLFactory.CreateContext(sslCtxClient, sslFreePascal)`
  - connection inherits `default.example.com`
- Add one isolation case:
  - call `TSSLFactory.CreateContext(AConfig)` with `ServerName := 'sticky.example.com'`
  - confirm that context/connection inherit it for that one call
  - then call `TSSLFactory.CreateContext(sslCtxClient, sslFreePascal)`
  - assert the later context/connection do **not** inherit `sticky.example.com`

**Step 2: Run the test to confirm RED**

Run:
`fpc -Fu./src -otmp/test_factory_config_server_name_isolation tests/test_factory_config_server_name_isolation.pas && ./tmp/test_factory_config_server_name_isolation`

Expected:
- FAIL because `CreateContext(const AConfig)` currently calls `LLib.SetDefaultConfig(LConfig)` on the shared library instance

### Task 2: Minimal factory fix

**Files:**
- Modify: `src/fafafa.ssl.factory.pas`

**Step 1: Stop mutating shared defaults in one-shot context creation**

- In `TSSLFactory.CreateContext(const AConfig)`:
  - keep `NormalizeConfigOptions(LConfig)`
  - keep applying the normalized config to the returned context
  - but do **not** write `LConfig` back through `LLib.SetDefaultConfig(...)`
- Preserve the explicit default-config path:
  - `ISSLLibrary.SetDefaultConfig(...)` + `CreateContext(AContextType, ALibType)` should still behave as before

**Step 2: Re-run the RED test**

Run:
`fpc -Fu./src -otmp/test_factory_config_server_name_isolation tests/test_factory_config_server_name_isolation.pas && ./tmp/test_factory_config_server_name_isolation`

Expected:
- PASS

### Task 3: Focused regression verification

**Files:**
- Test: `tests/test_factory_config_server_name_isolation.pas`
- Test: `tests/test_freepascal_context_server_name_inheritance.pas`
- Test: `tests/test_connection_builder_hostname_precedence.pas`

**Step 1: Run adjacent regressions**

Run:
- `fpc -Fu./src -otmp/test_freepascal_context_server_name_inheritance tests/test_freepascal_context_server_name_inheritance.pas && ./tmp/test_freepascal_context_server_name_inheritance`
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

- Note that the bug was not in explicit default-config usage itself.
- The drift came from reusing `SetDefaultConfig(...)` inside a one-shot factory helper on a cached library singleton.

**Step 2: Roll the next queue**

- Continue classifying remaining non-archive context-level SNI usage into compatibility coverage vs stale guidance.
- Revisit whether any other `TSSLConfig` fields besides `ServerName` need explicit isolation contracts.
