# Dirty Worktree Batched Closeout Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Drain the current mixed dirty worktree into reviewable, verified, topic-scoped commits without reverting user work or mixing unrelated families.

**Architecture:** Treat the current tree as an already-populated backlog. First freeze the state and split it by coherent ownership, then verify and commit one family at a time: OpenSSL/cert-utils hardening, TLS 1.3 primitives, builder/config/SNI/backend-selection, Wave C governance, and final docs truth. FreePascal early-data mainline remains closed unless fresh RED evidence appears.

**Tech Stack:** Free Pascal, shell contracts, Markdown docs, git scoped staging, file-based working memory.

---

## Current State

- The worktree contains hundreds of mixed tracked and untracked changes across `src/`, `tests/`, `scripts/`, `docs/`, and `docs/plans/`.
- Recent commits already absorbed the FreePascal TLS 1.3 completeness mainline and docs-history batch.
- Current canonical verification entrypoints remain:
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local`

## Batch Order

### Batch 0: Freeze and classify

**Files:**
- Modify: `docs/plans/2026-04-29-dirty-worktree-batched-closeout.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Steps:**
1. Record current `git status --short`, `git diff --stat`, and untracked-file inventory.
2. Classify dirty files into commit families.
3. Run only non-invasive baseline checks needed to know whether the tree is already broken.
4. Commit the planning/working-memory boundary if it is independently clean.

### Batch 1: OpenSSL and cert-utils fail-closed hardening

**Primary files:**
- `src/fafafa.ssl.cert.utils.pas`
- `src/fafafa.ssl.cert.*.pas`
- `src/fafafa.ssl.openssl.*.pas`
- `tests/test_cert_utils_*`
- `tests/test_openssl_*`
- matching `docs/plans/*cert-utils*`, `docs/plans/*openssl*`, `docs/plans/*bio*`, `docs/plans/*symbol*`

**Rules:**
- Commit by coherent family, not single symbol.
- Missing OpenSSL helpers must fail with controlled exceptions or capability false, not nil dereference.
- Run focused tests first, then compile gate.

### Batch 2: TLS 1.3 primitives and runtime contracts

**Primary files:**
- `src/fafafa.ssl.tls13.*`
- `tests/test_tls13_*.pas`
- any focused OpenSSL feature test that directly proves TLS 1.3 capability truth

**Rules:**
- Keep cryptographic behavior changes tied to focused tests.
- Do not fold docs/capability wording into this batch unless required by tests.

### Batch 3: Builder/config/SNI/backend-selection

**Primary files:**
- `src/fafafa.ssl.backend.selector.pas`
- config tests under `tests/config/`
- SNI/server-name focused tests
- docs guidance contracts that directly lock connection-level SNI policy

**Rules:**
- Preserve compatibility behavior while making canonical guidance explicit.
- Keep backend requirement enforcement separate from docs navigation.

### Batch 4: Wave C scripts and historical governance

**Primary files:**
- `scripts/check_wave_c_*`
- `scripts/run_wave_c_*`
- `scripts/prepare_wave_c_*`
- `scripts/generate_wave_c_*`
- `tests/scripts/*wave_c*`
- `docs/test_reports/WAVE_C_*`
- matching Wave B/C `docs/plans/`

**Rules:**
- Treat Wave C as historical/governance unless current roadmap says otherwise.
- Verify shell syntax and focused shell contracts before commit.

### Batch 5: Final docs/API/capability truth

**Primary files:**
- `README.md`
- `docs/README.md`
- `docs/DOCUMENTATION_INDEX.md`
- `docs/reference/API_REFERENCE.md`
- capability matrix and platform/user guides

**Rules:**
- Update docs only after behavior batches are verified.
- Keep historical links reachable but not promoted as current default entrypoints.

## Definition of Done

- Each batch has a short review conclusion before commit.
- Each commit is scoped to one coherent family.
- Focused tests and relevant gates are recorded in `progress.md`.
- `git diff --check` is clean for staged files before every commit.
- No existing user changes are reverted.
