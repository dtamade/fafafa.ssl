# API-Surface Tests Context-Level SNI Labeling Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Explicitly label selected non-connection tests that intentionally keep context-level `SetServerName(...)` as API-surface or validation coverage, so future cleanup passes do not mistake them for stale connection guidance.

**Architecture:** Treat this as a semantics-preserving test-annotation batch. Add a focused shell contract requiring a shared API-surface marker in the selected files, then add short comments near the retained context-level `SetServerName(...)` calls. Do not change runtime behavior, assertions, or test flow.

**Tech Stack:** Pascal test comments, shell contract test, focused compile/run verification

---

### Task 1: Add focused RED contract

**Files:**
- Add: `tests/scripts/test_api_surface_context_level_sni_labels_contract.sh`

**Step 1: Write the contract**

- Limit scope to selected API-surface / validation tests:
  - `tests/examples/test_lib_core_functionality.pas`
  - `tests/diagnostic/test_error_handling.pas`
  - `tests/diagnostic/test_error_handling_comprehensive.pas`
  - `tests/security/test_memory_safety.pas`
  - `tests/security/test_input_validation.pas`
- Require each selected file to contain:
  - context-level `SetServerName(...)`
  - a shared comment marker such as `INTENTIONAL_API_SURFACE: context-level SNI setter coverage`

**Step 2: Run RED**

Run:
`bash tests/scripts/test_api_surface_context_level_sni_labels_contract.sh`

Expected:
- FAIL because the selected files do not yet include the shared marker

### Task 2: GREEN - Label selected API-surface tests

**Files:**
- Modify: `tests/examples/test_lib_core_functionality.pas`
- Modify: `tests/diagnostic/test_error_handling.pas`
- Modify: `tests/diagnostic/test_error_handling_comprehensive.pas`
- Modify: `tests/security/test_memory_safety.pas`
- Modify: `tests/security/test_input_validation.pas`

**Step 1: Add explicit API-surface labels**

- Add one short comment near the retained legacy SNI call.
- Make the purpose explicit:
  - this is API-surface / validation coverage for `ISSLContext.SetServerName(...)`
  - this is not recommended connection-flow guidance

**Step 2: Re-run the contract**

Run:
`bash tests/scripts/test_api_surface_context_level_sni_labels_contract.sh`

Expected:
- PASS

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_api_surface_context_level_sni_labels_contract.sh`
- Test: `tests/examples/test_lib_core_functionality.pas`
- Test: `tests/diagnostic/test_error_handling.pas`
- Test: `tests/diagnostic/test_error_handling_comprehensive.pas`
- Test: `tests/security/test_memory_safety.pas`
- Test: `tests/security/test_input_validation.pas`

**Step 1: Run focused checks**

Run:
- `bash tests/scripts/test_api_surface_context_level_sni_labels_contract.sh`
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_lib_core_functionality tests/examples/test_lib_core_functionality.pas && ./tmp/test_lib_core_functionality`
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_error_handling tests/diagnostic/test_error_handling.pas && ./tmp/test_error_handling`
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_error_handling_comprehensive tests/diagnostic/test_error_handling_comprehensive.pas && ./tmp/test_error_handling_comprehensive`
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_memory_safety tests/security/test_memory_safety.pas && ./tmp/test_memory_safety`
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_input_validation tests/security/test_input_validation.pas && ./tmp/test_input_validation`

Expected:
- contract passes
- selected tests still compile and run

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record the classification**

- Note that these files are not live connection examples.
- Note that they intentionally cover setter acceptance, validation, or no-error behavior on the context API itself.

**Step 2: Roll the next queue**

- Revisit the remaining ambiguous files, especially:
  - `tests/openssl/test_openssl_ca_autoload.pas`
  - any leftover online/backend tests still mixing context-level SNI into non-contract flows
