# High-Visibility SNI Example Cleanup Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove deprecated context-level `SetServerName(...)` guidance from the most visible examples and example-style tests, and align them with the project’s connection-level SNI contract.

**Architecture:** Treat this as a guidance cleanup batch, not an API-removal batch. Keep compatibility coverage elsewhere untouched. Add a focused grep contract for the selected files, then update those examples so they either:
- configure SNI on `ISSLClientConnection`, or
- stop suggesting context-level SNI in non-connection demos.

**Tech Stack:** Pascal example files, shell contract test, focused `rg` verification

---

### Task 1: Add focused RED contract

**Files:**
- Add: `tests/scripts/test_high_visibility_examples_no_context_level_sni_guidance_contract.sh`

**Step 1: Write the contract**

- Limit scope to high-visibility examples and example-style tests:
  - `examples/example_factory_usage.pas`
  - `examples/winssl_health_checker.pas`
  - `examples/winssl_https_downloader.pas`
  - `examples/winssl_rest_client.pas`
  - `tests/examples/test_basic.pas`
  - `tests/examples/test_certchain.pas`
  - `tests/examples/test_performance.pas`
  - `tests/examples/test_winssl.pas`
  - `tests/examples/test_winssl_debug.pas`
  - `tests/examples/test_winssl_simple.pas`
- Fail if any of those files still use:
  - `Context.SetServerName(...)`
  - `Ctx.SetServerName(...)`
  - `LCtx.SetServerName(...)`
  - `LContext.SetServerName(...)`

**Step 2: Run RED**

Run:
`bash tests/scripts/test_high_visibility_examples_no_context_level_sni_guidance_contract.sh`

Expected:
- FAIL
- report the stale files

### Task 2: GREEN - Update examples and example-style tests

**Files:**
- Modify: `examples/example_factory_usage.pas`
- Modify: `examples/winssl_health_checker.pas`
- Modify: `examples/winssl_https_downloader.pas`
- Modify: `examples/winssl_rest_client.pas`
- Modify: `tests/examples/test_basic.pas`
- Modify: `tests/examples/test_certchain.pas`
- Modify: `tests/examples/test_performance.pas`
- Modify: `tests/examples/test_winssl.pas`
- Modify: `tests/examples/test_winssl_debug.pas`
- Modify: `tests/examples/test_winssl_simple.pas`

**Step 1: Replace stale guidance**

- In real connection flows:
  - create the connection first
  - cast to `ISSLClientConnection`
  - call `SetServerName(...)` before `Connect`
- In context-only demos:
  - remove the context-level SNI step
  - replace it with short prose explaining SNI is connection-level

**Step 2: Re-run the contract**

Run:
`bash tests/scripts/test_high_visibility_examples_no_context_level_sni_guidance_contract.sh`

Expected:
- PASS

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_high_visibility_examples_no_context_level_sni_guidance_contract.sh`
- Test: `examples/example_factory_usage.pas`
- Test: `tests/examples/test_basic.pas`

**Step 1: Run focused checks**

Run:
- `rg -n 'ISSLClientConnection|SetServerName\\(' examples/example_factory_usage.pas examples/winssl_health_checker.pas examples/winssl_https_downloader.pas examples/winssl_rest_client.pas tests/examples/test_basic.pas tests/examples/test_certchain.pas tests/examples/test_performance.pas tests/examples/test_winssl.pas tests/examples/test_winssl_debug.pas tests/examples/test_winssl_simple.pas`
- `fpc -Fu./src -otmp/example_factory_usage examples/example_factory_usage.pas`
- `fpc -Fu./src -otmp/test_basic_example tests/examples/test_basic.pas`

Expected:
- updated files still show the new connection-level SNI path where relevant
- the two cross-platform Pascal files compile

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record the drift**

- Note that the targeted docs were already clean, but high-visibility examples still taught deprecated context-level SNI.
- Note that this is more harmful than dormant compatibility APIs because examples directly shape user code.

**Step 2: Roll the next queue**

- Return the queue to the next code-level contract question after the guidance cleanup:
  - `BuildClient` legacy context-level SNI compatibility boundary
