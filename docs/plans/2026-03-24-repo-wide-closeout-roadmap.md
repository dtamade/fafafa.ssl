# Repo-wide Closeout Roadmap

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this roadmap in family-sized batches.

**Goal:** Close out the current repo state by converging default entrypoints, finishing remaining coherent hardening families, and treating historical Wave material as reference-only rather than active navigation.

**Architecture:** Keep execution milestone-based rather than symbol-by-symbol. Use existing green minimal CI as the baseline, then close one coherent family or one documentation/navigation milestone at a time. Preserve historical evidence, but move all default navigation to the current canonical Wave C status and manifests.

**Tech Stack:** Free Pascal, shell scripts, Markdown docs, focused contract tests

---

## Milestone 1: Freeze the canonical closeout entrypoints

**Files:**
- Modify: `README.md`
- Modify: `docs/README.md`
- Modify: `docs/DOCUMENTATION_INDEX.md`
- Modify: `docs/guides/GETTING_STARTED.md`
- Modify: `docs/guides/QUICKSTART.md`

**Intent:**
- default documentation navigation points to the current Wave C closeout pages:
  - `docs/test_reports/WAVE_C_CLOSEOUT_STATUS_2026-03-18.md`
  - `docs/test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md`
- historical B121/B127 pages remain reachable only as historical references, not as primary entrypoints
- active build/test guidance uses:
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`

**Acceptance:**
- top-level docs no longer promote B121/B127 as default Wave C entrypoints
- documentation index clearly distinguishes canonical current-chain pages from historical pages

## Milestone 2: Close remaining cert-utils hardening families

**Primary file:**
- Modify: `src/fafafa.ssl.cert.utils.pas`

**Intent:**
- continue only from fresh discovery
- do not replay already-green families
- prefer cohesive successful-path families:
  - remaining `GenerateSigned(...)` or `GenerateSelfSigned(...)` direct/delayed-loss helper gaps
  - post-success cleanup families where materialized output should survive helper loss

**Rules:**
- TDD only: focused RED, then minimal GREEN
- one family per batch, not one symbol per report
- preserve direct/Try-wrapper public contracts

**Acceptance:**
- each family ends with focused contract coverage, adjacent regression rerun, and full compile evidence

## Milestone 3: Close context/SNI compatibility drift

**Primary files:**
- Modify: `src/fafafa.ssl.context.builder.pas`
- Modify nearby tests covering builder validation, hostname precedence, connector/factory/backend consistency

**Intent:**
- keep runtime compatibility intact
- make deprecated context-level SNI guidance consistent with the already-established connection-level SNI policy
- preserve hostname precedence and backend parity contracts

**Acceptance:**
- warnings/validation/tests all align on connection-level SNI as the canonical path
- no runtime behavior regression for compatibility callers

## Milestone 4: Working memory and evidence closeout

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Intent:**
- replace ad hoc “next queue” fragmentation with milestone-level closeout tracking
- record when a line is already green so it is not re-opened without fresh evidence

**Acceptance:**
- every finished batch records scope, evidence, and next milestone
- no closed family is reopened without a fresh failing contract or baseline regression

## Batch Verification Standard

- focused RED command must be observed before production changes for behavior batches
- focused family regression must pass after the fix
- `python3 scripts/compile_all_modules.py`
- `bash scripts/run_minimal_ci_gate.sh --fast-local` at milestone checkpoints
- `git diff --check -- <changed files>`

## Defaults and Assumptions

- “彻底清仓” is implemented as **主入口收敛**, not aggressive deletion
- historical Wave pages remain in-repo as evidence/reference
- minimal CI green status is the current engineering baseline, not something to re-litigate before each batch
