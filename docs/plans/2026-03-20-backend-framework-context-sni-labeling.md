# Backend Framework Context SNI Labeling Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Explicitly label retained context-level `SetServerName(...)` usage in the backend framework tests as API-surface coverage, so later cleanup passes do not misclassify these framework configuration checks as stale connection-flow guidance.

**Architecture:** Treat this as a semantics-preserving annotation batch. Add a focused shell contract requiring a shared API-surface marker in the selected framework files, then add short comments near the retained context-level `SetServerName(...)` calls. Do not change assertions, connection mocks, or backend behavior.

**Tech Stack:** Pascal test comments, shell contract test, focused compile verification

---

### Task 1: Add focused RED contract

**Files:**
- Add: `tests/scripts/test_backend_framework_context_level_sni_labels_contract.sh`

**Step 1: Write the contract**

- Limit scope to:
  - `tests/test_mbedtls_framework.pas`
  - `tests/test_wolfssl_framework.pas`
- Require each selected file to contain:
  - context-level `SetServerName(...)`
  - the shared marker `INTENTIONAL_API_SURFACE: context-level SNI setter coverage`

**Step 2: Run RED**

Run:
`bash tests/scripts/test_backend_framework_context_level_sni_labels_contract.sh`

Expected:
- FAIL because the selected files do not yet include the shared marker

### Task 2: GREEN - Label selected framework tests

**Files:**
- Modify: `tests/test_mbedtls_framework.pas`
- Modify: `tests/test_wolfssl_framework.pas`

**Step 1: Add explicit API-surface labels**

- Add one short comment near the retained `SetServerName(...)` call.
- Make the purpose explicit:
  - this is backend framework/context setter coverage
  - this is not recommended per-connection handshake guidance

**Step 2: Re-run the contract**

Run:
`bash tests/scripts/test_backend_framework_context_level_sni_labels_contract.sh`

Expected:
- PASS

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_backend_framework_context_level_sni_labels_contract.sh`
- Test: `tests/test_mbedtls_framework.pas`
- Test: `tests/test_wolfssl_framework.pas`

**Step 1: Run focused checks**

Run:
- `bash tests/scripts/test_backend_framework_context_level_sni_labels_contract.sh`
- `mkdir -p tmp/framework_mbedtls tmp/framework_wolfssl`
- `fpc -B -Fu./src -FUtmp/framework_mbedtls -FEtmp/framework_mbedtls -otmp/framework_mbedtls/test_mbedtls_framework tests/test_mbedtls_framework.pas`
- `fpc -B -Fu./src -FUtmp/framework_wolfssl -FEtmp/framework_wolfssl -otmp/framework_wolfssl/test_wolfssl_framework tests/test_wolfssl_framework.pas`

Expected:
- contract passes
- selected framework files still compile in the local environment

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record the classification**

- Note that these framework tests only exercise backend context configuration and mock/diagnostic behavior.
- Note that they do not teach a real client connection flow around the retained `SetServerName(...)` calls.

**Step 2: Roll the next queue**

- Continue classifying any remaining backend/server-side context-level SNI hits after this labeling batch.
