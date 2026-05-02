# Compatibility Tests Context-Level SNI Labeling Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Explicitly label selected tests that intentionally retain context-level `SetServerName(...)` so future cleanups do not confuse compatibility coverage with stale guidance.

**Architecture:** Treat this as a semantics-preserving test-annotation batch. Add a focused shell contract requiring a shared compatibility marker in the selected files, then add short comments near the intentional legacy SNI usage sites. Do not change any runtime behavior, assertions, or test flow.

**Tech Stack:** Pascal test comments, shell contract test, focused compile verification

---

### Task 1: Add focused RED contract

**Files:**
- Add: `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`

**Step 1: Write the contract**

- Limit scope to tests that intentionally exercise legacy context-level SNI compatibility:
  - `tests/test_connection_builder_hostname_precedence.pas`
  - `tests/test_tls_connector_hostname_override_precedence.pas`
  - `tests/test_freepascal_context_server_name_inheritance.pas`
  - `tests/integration/test_cross_backend_consistency_contract.pas`
  - `tests/integration/test_cross_backend_errors_contract.pas`
- Require each selected file to contain:
  - context-level `SetServerName(...)`
  - a shared comment marker such as `INTENTIONAL_COMPAT: legacy context-level SNI coverage`

**Step 2: Run RED**

Run:
`bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`

Expected:
- FAIL because the selected files do not yet include the shared compatibility marker

### Task 2: GREEN - Label selected compatibility tests

**Files:**
- Modify: `tests/test_connection_builder_hostname_precedence.pas`
- Modify: `tests/test_tls_connector_hostname_override_precedence.pas`
- Modify: `tests/test_freepascal_context_server_name_inheritance.pas`
- Modify: `tests/integration/test_cross_backend_consistency_contract.pas`
- Modify: `tests/integration/test_cross_backend_errors_contract.pas`

**Step 1: Add explicit compatibility labels**

- Add one short comment near the retained legacy SNI call or surrounding case setup.
- Make the purpose explicit:
  - this is intentional compatibility/fallback coverage
  - this is not recommended connection-flow guidance

**Step 2: Re-run the contract**

Run:
`bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`

Expected:
- PASS

### Task 3: Focused verification

**Files:**
- Test: `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
- Test: `tests/test_connection_builder_hostname_precedence.pas`
- Test: `tests/test_tls_connector_hostname_override_precedence.pas`
- Test: `tests/test_freepascal_context_server_name_inheritance.pas`
- Test: `tests/integration/test_cross_backend_consistency_contract.pas`
- Test: `tests/integration/test_cross_backend_errors_contract.pas`

**Step 1: Run focused checks**

Run:
- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
- `fpc -Fu./src -otmp/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence`
- `fpc -Fu./src -otmp/test_tls_connector_hostname_override_precedence tests/test_tls_connector_hostname_override_precedence.pas && ./tmp/test_tls_connector_hostname_override_precedence`
- `fpc -Fu./src -otmp/test_freepascal_context_server_name_inheritance tests/test_freepascal_context_server_name_inheritance.pas && ./tmp/test_freepascal_context_server_name_inheritance`
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_cross_backend_consistency_contract tests/integration/test_cross_backend_consistency_contract.pas`
- `fpc -Fu./src -Fu./tests -Fu./tests/framework -otmp/test_cross_backend_errors_contract tests/integration/test_cross_backend_errors_contract.pas`

Expected:
- contract passes
- selected tests still compile
- the three local contract tests still pass

### Task 4: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record the classification rule**

- Note that active context-level SNI hits should no longer be judged only by presence.
- Distinguish intentional compatibility coverage from stale user-facing guidance.

**Step 2: Roll the next queue**

- Continue classifying the remaining active hits with the same rule.
- Prioritize any leftover example/demo/API-smoke files that still teach context-level SNI without an explicit compatibility reason.
