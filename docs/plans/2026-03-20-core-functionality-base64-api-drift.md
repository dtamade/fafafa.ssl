# Core Functionality Base64 API Drift Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Restore the broken example/benchmark tests that still call removed `TCryptoUtils` Base64 helpers by updating them to the current `TEncodingUtils` API, and fix the example smoke test harness so OpenSSL is registered locally.

**Architecture:** Treat this as a narrow test-only API-drift batch. Use the observed compile failure in `tests/examples/test_lib_core_functionality.pas` as RED, then apply the smallest possible updates:
- switch Base64 calls from `TCryptoUtils.*` to `TEncodingUtils.*`
- add the missing OpenSSL registration unit for the factory-based smoke test
- update the matching benchmark drift at the same time because it shares the same root cause

**Tech Stack:** Pascal tests/benchmarks, focused compile/run verification

---

### Task 1: Capture RED evidence

**Files:**
- `tests/examples/test_lib_core_functionality.pas`
- `tests/crypto/benchmark_base64_performance.pas`

**Step 1: Reproduce the failure**

Run:
`fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_lib_core_functionality tests/examples/test_lib_core_functionality.pas && ./tmp/test_lib_core_functionality`

Expected:
- compile failure on missing `TCryptoUtils.Base64Encode` / `TCryptoUtils.Base64DecodeString`
- after API correction, runtime still reveals the file needs explicit OpenSSL backend registration

### Task 2: Minimal GREEN fix

**Files:**
- Modify: `tests/examples/test_lib_core_functionality.pas`
- Modify: `tests/crypto/benchmark_base64_performance.pas`

**Step 1: Update to current Base64 API**

- Add `fafafa.ssl.encoding`
- Replace:
  - `TCryptoUtils.Base64Encode(...)` → `TEncodingUtils.Base64Encode(...)`
  - `TCryptoUtils.Base64DecodeString(...)` → `TEncodingUtils.Base64DecodeString(...)`
  - `TCryptoUtils.Base64Decode(...)` → `TEncodingUtils.Base64Decode(...)`

**Step 2: Fix local harness registration**

- In `tests/examples/test_lib_core_functionality.pas`, import `fafafa.ssl.openssl.backed`
- Keep the existing factory-based smoke flow unchanged otherwise

### Task 3: Focused verification

**Files:**
- Test: `tests/examples/test_lib_core_functionality.pas`
- Test: `tests/crypto/benchmark_base64_performance.pas`

**Step 1: Run focused checks**

Run:
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_lib_core_functionality tests/examples/test_lib_core_functionality.pas && ./tmp/test_lib_core_functionality`
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/benchmark_base64_performance tests/crypto/benchmark_base64_performance.pas && ./tmp/benchmark_base64_performance`

Expected:
- core functionality smoke test passes again
- benchmark compiles and runs with current Base64 API

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record the root cause**

- Note that the failure was not caused by the SNI-labeling comments.
- Root causes were:
  - Base64 API drift from `TCryptoUtils` to `TEncodingUtils`
  - missing OpenSSL backend registration in the factory-based smoke test

**Step 2: Roll the next queue**

- Keep scanning for similar API drift in active tests/benchmarks while continuing the SNI classification work.
