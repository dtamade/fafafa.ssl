# Working-Memory And Artifact Hygiene Closeout Plan

**Goal:** clean the current workspace artifacts, resync `task_plan.md` / `findings.md` / `progress.md` to the current `HEAD` (`e80100a`), and finish the already-defined WinSSL Windows workflow closeout without reopening `src/` product work.

**Architecture:** no production code changes in this batch. The work is limited to:

- removing the three generated test binaries from `tests/`
- writing down the current true next step
- keeping the working-memory files aligned with the current repo truth
- fixing the current Wave B/B2 Windows workflow contract RED by aligning the Windows lane to the runtime checklist
- verifying diff hygiene before commit

**Files:**

- Add: `docs/plans/2026-05-12-working-memory-and-artifact-hygiene-closeout.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`
- Modify: `.github/workflows/wave-b-b2-manual.yml`
- Modify: `.github/workflows/wave-b-b2-manual.yml.disabled`
- Modify: `.github/README.md`
- Modify: `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md`
- Modify: `tests/windows/VALIDATION_BUNDLE.md`

## Task 1: Remove generated test binaries

Delete the three untracked ELF test artifacts:

- `tests/contract/test_capabilities_contract`
- `tests/wolfssl/test_wolfssl_connection_contract`
- `tests/wolfssl/test_wolfssl_context_contract`

Expected:

- `git status --short` no longer lists those binaries
- no source files are touched

## Task 2: Resync working memory to the current truth

Update the top of the working-memory files so they say:

- current `HEAD` is `e80100a`
- the current batch is workspace hygiene, not new product behavior
- the remaining product-side blocker is still the separate WinSSL Windows runtime proof lane
- the repository already has a valid docs/plans trail for the WinSSL workflow work

## Task 3: Verify and commit

Run:

```bash
bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh
bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh
bash tests/scripts/test_wave_b_windows_gate_pwsh_and_verbose_contract.sh
git diff --check -- docs/plans/2026-05-12-working-memory-and-artifact-hygiene-closeout.md task_plan.md findings.md progress.md
git status --short
```

Expected:

- workflow contract is green after the Windows lane update
- WinSSL bundle contract stays green
- wave_b gate pwsh/verbose contract stays green
- diff hygiene passes
- only intentional text-file changes remain
- the batch can be committed cleanly
