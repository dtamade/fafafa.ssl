# Connection-Flow Tests SNI Guidance Cleanup Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Remove deprecated context-level `SetServerName(...)` guidance from selected active connection-flow tests that exercise real client handshakes rather than explicit compatibility contracts.

**Architecture:** Treat this as a narrow test-guidance cleanup batch, not an API-removal or compatibility-contract batch. Add a focused grep contract for the selected files, then update each test to configure SNI on `ISSLClientConnection` after `CreateConnection(...)` and before `Connect`. Keep tests that intentionally cover context fallback or precedence semantics untouched.

**Tech Stack:** Pascal tests, shell contract test, focused compile verification

---

### Task 1: Add focused RED contract

**Files:**
- Add: `tests/scripts/test_connection_flow_tests_no_context_level_sni_guidance_contract.sh`

**Step 1: Write the contract**

- Limit scope to active connection-flow tests that currently use context-level SNI but are not named as compatibility contracts:
  - `tests/mbedtls/test_mbedtls_connection.pas`
  - `tests/mbedtls/test_mbedtls_simple_connection.pas`
  - `tests/integration/test_e2e_scenarios.pas`
  - `tests/integration/test_real_https_connection.pas`
- Fail if any of those files still use:
  - `Context.SetServerName(...)`
  - `Ctx.SetServerName(...)`
  - `LCtx.SetServerName(...)`
  - `LContext.SetServerName(...)`

**Step 2: Run RED**

Run:
`bash tests/scripts/test_connection_flow_tests_no_context_level_sni_guidance_contract.sh`

Expected:
- FAIL
- report the stale files

### Task 2: GREEN - Update selected connection-flow tests

**Files:**
- Modify: `tests/mbedtls/test_mbedtls_connection.pas`
- Modify: `tests/mbedtls/test_mbedtls_simple_connection.pas`
- Modify: `tests/integration/test_e2e_scenarios.pas`
- Modify: `tests/integration/test_real_https_connection.pas`

**Step 1: Replace stale guidance**

- In each real connection flow:
  - create the connection first
  - cast to `ISSLClientConnection`
  - call `SetServerName(...)` before `Connect`
- Preserve any non-SNI assertions that are still relevant:
  - protocol selection
  - session reuse ordering
  - ALPN / certificate / transfer checks

**Step 2: Re-run the contract**

Run:
`bash tests/scripts/test_connection_flow_tests_no_context_level_sni_guidance_contract.sh`

Expected:
- PASS

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_connection_flow_tests_no_context_level_sni_guidance_contract.sh`
- Test: `tests/mbedtls/test_mbedtls_connection.pas`
- Test: `tests/mbedtls/test_mbedtls_simple_connection.pas`
- Test: `tests/integration/test_e2e_scenarios.pas`
- Test: `tests/integration/test_real_https_connection.pas`

**Step 1: Run focused checks**

Run:
- `rg -n 'ISSLClientConnection|SetServerName\\(' tests/mbedtls/test_mbedtls_connection.pas tests/mbedtls/test_mbedtls_simple_connection.pas tests/integration/test_e2e_scenarios.pas tests/integration/test_real_https_connection.pas`
- `fpc -Fu./src -otmp/test_mbedtls_connection tests/mbedtls/test_mbedtls_connection.pas`
- `fpc -Fu./src -otmp/test_mbedtls_simple_connection tests/mbedtls/test_mbedtls_simple_connection.pas`
- `fpc -Fu./src -Fu./tests -otmp/test_e2e_scenarios tests/integration/test_e2e_scenarios.pas`
- `fpc -Fu./src -Fu./tests -otmp/test_real_https_connection tests/integration/test_real_https_connection.pas`

Expected:
- selected files show per-connection SNI setup
- selected tests compile on the local Linux harness

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

**Step 1: Record the classification**

- Note that these files were not explicit compatibility contracts.
- Note that they were still exercising real connection flows and therefore should follow the project’s recommended connection-level SNI path.

**Step 2: Roll the next queue**

- Continue classifying the remaining active context-level `SetServerName(...)` usages into:
  - intentional compatibility/API-surface coverage to keep and label
  - still-stale test/demo guidance to clean in later batches
