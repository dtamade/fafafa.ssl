# Milestone 1 doc guardrail contracts（2026-03-25）

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Freeze Milestone 1 by adding focused shell contracts that lock the already-closed documentation families to the current Wave C navigation and build/test guidance.

**Architecture:** Keep this batch docs-only. Add three `tests/scripts` guardrail contracts for the newly closed 2026-03-25 docs families, rerun the existing top-entrypoint docs contracts, and only touch Markdown if the new contracts expose a fresh drift. Preserve historical references, but require them to stay explicitly labeled as historical/compatibility context.

**Tech Stack:** Bash, `rg`, Markdown docs, shell contract tests

---

## Task 1: Add support-docs guidance contract

**Files:**
- Add: `tests/scripts/test_support_docs_legacy_script_guidance_contract.sh`
- Reference: `docs/FCL_DEPENDENCIES.md`
- Reference: `docs/testing/TEST_COVERAGE_ASSESSMENT.md`

**Checks:**
- require the canonical command chain in both docs
- reject legacy Linux script guidance as an active path
- allow `docs/FCL_DEPENDENCIES.md` to mention historical `build_linux.sh` / `run_tests_linux.sh` only as historical context

## Task 2: Add platform-support guidance contract

**Files:**
- Add: `tests/scripts/test_platform_support_guidance_convergence_contract.sh`
- Reference: `docs/PLATFORM_SUPPORT.md`

**Checks:**
- reject `build_linux.sh`, `run_core_tests.sh`, and `build_macos.sh` as default platform guidance
- require Wave C closeout/current-chain entrypoints
- require Linux canonical commands
- require the macOS focused smoke path using `tests/openssl/test_openssl_simple.pas`

## Task 3: Add historical-reference labeling contract

**Files:**
- Add: `tests/scripts/test_active_docs_historical_reference_labels_contract.sh`
- Reference: `README.md`
- Reference: `docs/README.md`
- Reference: `docs/DOCUMENTATION_INDEX.md`
- Reference: `docs/AGENTS.md`
- Reference: `docs/guides/LINUX_QUICKSTART.md`
- Reference: `docs/guides/QUICKSTART_30SEC.md`
- Reference: `docs/FCL_DEPENDENCIES.md`

**Checks:**
- require existing historical/compatibility labels for B121/B127 and `build_linux.sh`
- reject `B127` as a first-stop troubleshooting entry in `docs/guides/QUICKSTART_30SEC.md`
- keep the historical references reachable, but only in explicitly historical/compatibility wording

## Task 4: Verify and write back

**Run:**
- `bash -n tests/scripts/test_support_docs_legacy_script_guidance_contract.sh`
- `bash -n tests/scripts/test_platform_support_guidance_convergence_contract.sh`
- `bash -n tests/scripts/test_active_docs_historical_reference_labels_contract.sh`
- `bash tests/scripts/test_support_docs_legacy_script_guidance_contract.sh`
- `bash tests/scripts/test_platform_support_guidance_convergence_contract.sh`
- `bash tests/scripts/test_active_docs_historical_reference_labels_contract.sh`
- `bash tests/scripts/test_canonical_wave_c_entrypoint_convergence_contract.sh`
- `bash tests/scripts/test_active_docs_no_ci_pipeline_contract.sh`
- `git diff --check -- docs/plans/2026-03-25-milestone1-doc-guardrail-contracts.md tests/scripts/test_support_docs_legacy_script_guidance_contract.sh tests/scripts/test_platform_support_guidance_convergence_contract.sh tests/scripts/test_active_docs_historical_reference_labels_contract.sh README.md docs/README.md docs/DOCUMENTATION_INDEX.md docs/AGENTS.md docs/guides/LINUX_QUICKSTART.md docs/guides/QUICKSTART_30SEC.md docs/FCL_DEPENDENCIES.md docs/testing/TEST_COVERAGE_ASSESSMENT.md docs/PLATFORM_SUPPORT.md task_plan.md findings.md progress.md`

**Expected:**
- all five docs contracts pass
- diff hygiene passes
- no runtime/source code edits are needed
