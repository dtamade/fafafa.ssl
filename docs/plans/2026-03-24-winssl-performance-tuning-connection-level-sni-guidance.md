# WinSSL Performance Tuning Connection-Level SNI Guidance Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Align the client-side connection examples in `docs/reference/WINSSL_PERFORMANCE_TUNING.md` with the canonical per-connection SNI path so the performance guide no longer shows WinSSL connection/session benchmarks that handshake without explicit hostname guidance.

**Architecture:** Keep this batch docs-only and scoped to one reference document. Add a focused shell contract over the selected WinSSL performance snippets, then update those examples in the smallest way that preserves the performance-tuning intent:
- direct session-reuse examples should set SNI explicitly on the client connection
- helper/pool/cache snippets that do not create the connection inline should document that per-connection SNI is already configured by the caller/acquire path
- benchmark/session-resume flows must keep `SetSession(...)` before `SetServerName(...)`

**Tech Stack:** Markdown docs, shell contract (`rg`)

---

### Task 1: Add RED contract

**Files:**
- Add: `tests/scripts/test_winssl_performance_tuning_connection_level_sni_guidance_contract.sh`
- Reference: `docs/reference/WINSSL_PERFORMANCE_TUNING.md`

**Step 1: Write the failing contract**

- Require explicit guidance for the selected snippet families:
  - basic WinSSL session reuse
  - session cache / connection-pool assumption notes
  - `MeasureConnection(AConn)` caller-responsibility note
  - memory-leak loops that actually call `Connect`
  - benchmark session-reuse call sites

**Step 2: Run RED**

Run: `bash tests/scripts/test_winssl_performance_tuning_connection_level_sni_guidance_contract.sh`

Expected:
- FAIL because the current performance guide still omits explicit per-connection SNI guidance in those selected snippets.

### Task 2: GREEN - Update the selected snippets

**Files:**
- Modify: `docs/reference/WINSSL_PERFORMANCE_TUNING.md`

**Step 1: Fix direct session-reuse and benchmark call sites**

- Add explicit `ISSLClientConnection.SetServerName(...)` to the direct `CreateConnection(...)` examples.
- Where the benchmark uses a reusable hostname, introduce `LHost := 'example.com';`.
- Preserve session-resume order:
  - `CreateConnection(...)`
  - `SetSession(...)` when resuming
  - `SetServerName(...)`
  - `Connect` / `MeasureConnection(...)`

**Step 2: Clarify helper/pool/cache responsibility**

- In helper-style snippets that do not create the connection inline:
  - add one short note that the connection already carries per-connection SNI for the target host
- In `MeasureConnection(AConn)`:
  - add a short caller-responsibility note before `Connect`

**Step 3: Fix memory-leak examples**

- Even in the “wrong” and “right” loop examples, set SNI before `Connect` so the examples do not model incomplete client handshakes.
- Keep the leak-management point unchanged.

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_winssl_performance_tuning_connection_level_sni_guidance_contract.sh`
- Test: `docs/reference/WINSSL_PERFORMANCE_TUNING.md`

**Step 1: Re-run contract**

Run: `bash tests/scripts/test_winssl_performance_tuning_connection_level_sni_guidance_contract.sh`

Expected:
- PASS

**Step 2: Check rendered guidance**

Run: `rg -n "ISSLClientConnection|SetServerName\\(|连接级 SNI|LHost := 'example\\.com'" docs/reference/WINSSL_PERFORMANCE_TUNING.md`

Expected:
- the selected WinSSL performance snippets now show explicit per-connection SNI guidance or caller-responsibility notes.

**Step 3: Diff hygiene**

Run: `git diff --check -- docs/plans/2026-03-24-winssl-performance-tuning-connection-level-sni-guidance.md tests/scripts/test_winssl_performance_tuning_connection_level_sni_guidance_contract.sh docs/reference/WINSSL_PERFORMANCE_TUNING.md task_plan.md findings.md progress.md`

Expected:
- PASS
