# Error Handling Best Practices URL-Driven SNI Guidance Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Align the URL-driven HTTPS client example in `docs/guides/ERROR_HANDLING_BEST_PRACTICES.md` with the canonical per-connection SNI path so it no longer shows `CreateConnection(...)` followed by `Connect` without an explicit hostname source on the client connection.

**Architecture:** Keep this batch docs-only and narrow. Add a focused shell contract over the `HTTPSClient(const AUrl: string)` example in `docs/guides/ERROR_HANDLING_BEST_PRACTICES.md`, then update that snippet to introduce an explicit `LHost` source and call `ISSLClientConnection.SetServerName(LHost)` before `Connect`. Preserve the three-pattern error-handling intent; do not redesign retry logic, request sending, or runtime behavior.

**Tech Stack:** Markdown docs, shell contract (`rg`)

---

### Task 1: Add RED contract

**Files:**
- Add: `tests/scripts/test_error_handling_best_practices_url_driven_sni_guidance_contract.sh`
- Reference: `docs/guides/ERROR_HANDLING_BEST_PRACTICES.md`

**Step 1: Write the failing contract**

- Require the URL-driven `HTTPSClient(...)` example to show:
  - an explicit `LHost` variable
  - a concrete example host assignment
  - explicit connection-level SNI through `ISSLClientConnection.SetServerName(LHost)`

**Step 2: Run RED**

Run: `bash tests/scripts/test_error_handling_best_practices_url_driven_sni_guidance_contract.sh`

Expected:
- FAIL because the current example still omits explicit host-source + connection-level SNI guidance.

### Task 2: GREEN - Update the selected snippet

**Files:**
- Modify: `docs/guides/ERROR_HANDLING_BEST_PRACTICES.md`

**Step 1: Introduce a local hostname source**

- In `HTTPSClient(const AUrl: string)`:
  - add `LHost: string;`
  - add `LHost := 'api.example.com';`
  - add one short note that real code should parse the hostname from `AUrl` before creating or reusing `LSocket`

**Step 2: Add connection-level SNI**

- Keep the existing `ISSLConnection` variable.
- Add `(LConnection as ISSLClientConnection).SetServerName(LHost);` before `Connect`.
- Keep the rest of the example focused on error-handling patterns rather than expanding transport details.

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_error_handling_best_practices_url_driven_sni_guidance_contract.sh`
- Test: `docs/guides/ERROR_HANDLING_BEST_PRACTICES.md`

**Step 1: Re-run contract**

Run: `bash tests/scripts/test_error_handling_best_practices_url_driven_sni_guidance_contract.sh`

Expected:
- PASS

**Step 2: Check rendered guidance**

Run: `rg -n "LHost|SetServerName\\(|AUrl" docs/guides/ERROR_HANDLING_BEST_PRACTICES.md`

Expected:
- the selected HTTPS client example now shows an explicit host source and per-connection SNI guidance.

**Step 3: Diff hygiene**

Run: `git diff --check -- docs/plans/2026-03-24-error-handling-best-practices-url-driven-sni-guidance.md tests/scripts/test_error_handling_best_practices_url_driven_sni_guidance_contract.sh docs/guides/ERROR_HANDLING_BEST_PRACTICES.md task_plan.md findings.md progress.md`

Expected:
- PASS
