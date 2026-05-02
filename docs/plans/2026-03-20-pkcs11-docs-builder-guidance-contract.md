# PKCS#11 Docs Builder Guidance Contract Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a lightweight shell contract test that locks the PKCS#11 docs to the current builder/runtime guidance. The test should catch both builder PIN-source drift and stale backend API names in the architecture reference before they silently reappear.

**Architecture:** Keep this batch narrow. Add a single `tests/scripts` contract that checks `docs/guides/PKCS11_USER_GUIDE.md` for supported builder PIN-source examples and lower-level callback guidance, and checks `docs/reference/PKCS11_ARCHITECTURE.md` for the current builder boundary plus current backend interface/class names. Verify with `bash -n`, a real test run, and diff hygiene.

**Tech Stack:** Bash, `rg`, Markdown docs, shell contract tests

---

### Task 1: Add PKCS#11 docs contract test

**Files:**
- Add: `tests/scripts/test_pkcs11_docs_builder_guidance_contract.sh`
- Reference: `docs/guides/PKCS11_USER_GUIDE.md`
- Reference: `docs/reference/PKCS11_ARCHITECTURE.md`

**Step 1: Guard the guide contract**

- Assert the guide still contains:
  - env/file builder examples
  - explicit callback/interactive unsupported-at-builder guidance
  - lower-level callback example using `TPKCS11ConfigDefault`, backend factory, and object-bound callback
- Assert the guide does not regress to stale builder API names:
  - `.WithPKCS11Key(...)`
  - `.ForServer`
  - `.Build;`
  - free-function callback example

**Step 2: Guard the architecture contract**

- Assert the architecture doc still contains:
  - builder runtime contract note
  - env/file builder support markers
  - current `IPKCS11Backend` members
  - `TProviderBackend` / `TEngineBackend`
- Assert the old backend API names do not reappear:
  - `GetBackendType`
  - `GetLastError`
  - `TPKCS11ProviderBackend`
  - `TPKCS11EngineBackend`

### Task 2: Verify the contract test

**Files:**
- Test: `tests/scripts/test_pkcs11_docs_builder_guidance_contract.sh`

**Step 1: Syntax-check**

Run:
`bash -n tests/scripts/test_pkcs11_docs_builder_guidance_contract.sh`

Expected:
- PASS

**Step 2: Run the contract**

Run:
`bash tests/scripts/test_pkcs11_docs_builder_guidance_contract.sh`

Expected:
- PASS

**Step 3: Run whitespace / patch hygiene**

Run:
`git diff --check -- docs/guides/PKCS11_USER_GUIDE.md docs/reference/PKCS11_ARCHITECTURE.md tests/scripts/test_pkcs11_docs_builder_guidance_contract.sh docs/plans/2026-03-20-pkcs11-docs-builder-guidance-contract.md task_plan.md findings.md progress.md`

Expected:
- PASS

### Task 3: Planning writeback

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record why the test exists**

- Note that docs drift already happened once across builder guidance and backend API naming
- Record that the contract test is intentionally pattern-based, not a Markdown linter rewrite

**Step 2: Roll next queue**

- Continue the builder/state audit on `pkcs11_pin_method` export/import/merge behavior
- Keep future callback/interactive builder support as a separate design decision
