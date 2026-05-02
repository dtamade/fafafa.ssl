# WinSSL Comprehensive Context SNI Labeling Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Explicitly label remaining WinSSL comprehensive tests that intentionally retain context-level `SetServerName(...)` as `ISSLContext` API-surface coverage, so later cleanup passes do not misclassify them as stale connection-flow guidance.

**Architecture:** Treat this as a semantics-preserving annotation batch. Add a focused shell contract requiring a shared API-surface marker in the selected WinSSL comprehensive files, then add short comments near the retained context-level `SetServerName(...)` calls. Do not change runtime behavior, assertions, sockets, or handshake flow.

**Tech Stack:** Pascal test comments, shell contract test, focused Win64 cross-compile verification

---

### Task 1: Add focused RED contract

**Files:**
- Add: `tests/scripts/test_winssl_comprehensive_context_level_sni_labels_contract.sh`

**Step 1: Write the contract**

- Limit scope to:
  - `tests/winssl/test_winssl_context_comprehensive.pas`
  - `tests/winssl/test_winssl_unit_comprehensive.pas`
  - `tests/unit/test_winssl_comprehensive.pas`
- Require each selected file to contain:
  - context-level `SetServerName(...)`
  - the shared marker `INTENTIONAL_API_SURFACE: context-level SNI setter coverage`

**Step 2: Run RED**

Run:
`bash tests/scripts/test_winssl_comprehensive_context_level_sni_labels_contract.sh`

Expected:
- FAIL because the selected files do not yet include the shared marker

### Task 2: GREEN - Label selected API-surface tests

**Files:**
- Modify: `tests/winssl/test_winssl_context_comprehensive.pas`
- Modify: `tests/winssl/test_winssl_unit_comprehensive.pas`
- Modify: `tests/unit/test_winssl_comprehensive.pas`

**Step 1: Add explicit API-surface labels**

- Add one short comment near the retained legacy SNI setter/getter coverage.
- Make the purpose explicit:
  - this is `ISSLContext.SetServerName(...)` API-surface coverage
  - this is not recommended connection-flow guidance

**Step 2: Re-run the contract**

Run:
`bash tests/scripts/test_winssl_comprehensive_context_level_sni_labels_contract.sh`

Expected:
- PASS

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_winssl_comprehensive_context_level_sni_labels_contract.sh`
- Test: `tests/winssl/test_winssl_context_comprehensive.pas`
- Test: `tests/winssl/test_winssl_unit_comprehensive.pas`
- Test: `tests/unit/test_winssl_comprehensive.pas`

**Step 1: Run focused checks**

Run:
- `bash tests/scripts/test_winssl_comprehensive_context_level_sni_labels_contract.sh`
- `fpc -Twin64 -Fu./src -otmp/test_winssl_context_comprehensive.exe tests/winssl/test_winssl_context_comprehensive.pas`
- `fpc -Twin64 -Fu./src -otmp/test_winssl_unit_comprehensive.exe tests/winssl/test_winssl_unit_comprehensive.pas`
- `fpc -Twin64 -Fu./src -otmp/test_unit_winssl_comprehensive.exe tests/unit/test_winssl_comprehensive.pas`

Expected:
- contract passes
- selected files still cross-compile successfully on the local Linux host targeting Win64

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record the classification**

- Note that these files do not establish real TCP/TLS connections around the retained `SetServerName(...)` calls.
- Note that they specifically cover WinSSL context setter/getter acceptance and context management behavior.

**Step 2: Roll the next queue**

- Continue with separate classification for:
  - `tests/test_mbedtls_framework.pas`
  - `tests/test_wolfssl_framework.pas`
  - `tests/mbedtls/test_mbedtls_server_accept.pas`
  - `tests/mbedtls/test_mbedtls_server_accept_simple.pas`
