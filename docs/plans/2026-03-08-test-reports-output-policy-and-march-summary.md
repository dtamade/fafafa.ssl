# Test-Reports Output Policy and March Summary Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 把 2–3 个高频脚本的默认输出从 `test-reports/` 收口到忽略目录 `tmp/`，同时新增 2026-03 月度汇总页以降低 `docs/plans` 治理噪音。

**Architecture:** 先用轻量 contract 锁定三个脚本的默认输出策略，再做最小实现，把默认报告/日志落到 `tmp/`，保留显式 `--output` 和现有调用方兼容。文档侧新增月度汇总页，并把它接到当前 plans 入口，避免继续扩大根索引的历史链接面。

**Tech Stack:** bash scripts, shell contract tests, markdown docs.

---

### Task 1: Lock failing contracts for default output policy

**Files:**
- Create: `tests/scripts/test_repo_hygiene_tmp_report_defaults_contract.sh`
- Create: `tests/scripts/test_wave_b_cross_platform_summary_default_output_contract.sh`
- Modify: `tests/scripts/test_run_all_module_tests_parallel_output_isolation_contract.sh`
- Modify: `tests/scripts/test_wave_c_b101_validation_playbook_isolation_passthrough_contract.sh`

**Step 1: Write the failing contracts**
- Assert `scripts/run_all_module_tests.sh` no longer defaults reports to `test-reports/`.
- Assert `scripts/run_wave_c_b101_validation_playbook.sh` defaults report/log outputs under `tmp/`.
- Assert `scripts/generate_wave_b_cross_platform_summary.sh` default output lands under `tmp/` in a fixture run.
- Extend focused regressions so `run_all_module_tests` report paths and Wave C B101 report-dir defaults are pinned.

**Step 2: Run tests to verify they fail**
Run:
```bash
bash tests/scripts/test_repo_hygiene_tmp_report_defaults_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary_default_output_contract.sh
bash tests/scripts/test_wave_c_b101_validation_playbook_isolation_passthrough_contract.sh
bash tests/scripts/test_run_all_module_tests_parallel_output_isolation_contract.sh
```
Expected: FAIL because the three scripts still default to `test-reports/`.

### Task 2: Move default outputs to ignored `tmp/`

**Files:**
- Modify: `scripts/run_all_module_tests.sh`
- Modify: `scripts/run_wave_c_b101_validation_playbook.sh`
- Modify: `scripts/generate_wave_b_cross_platform_summary.sh`

**Step 1: Write minimal implementation**
- `run_all_module_tests.sh`: add `FAFAFA_TEST_REPORTS_DIR` override and default reports under `tmp/run_all_module_tests_reports_<run_id>/`.
- `run_wave_c_b101_validation_playbook.sh`: add a default `REPORT_DIR` under `tmp/wave_c_b101_reports_<run_id>/` and route report/log outputs through it.
- `generate_wave_b_cross_platform_summary.sh`: default `--output` to `tmp/wave_b_cross_platform_summary_<run_id>.md`.

**Step 2: Keep compatibility**
- Preserve explicit `--output` behavior.
- Preserve existing `test-reports/` inputs where they are evidence inputs, not new outputs.
- Create missing directories before writing reports/logs.

**Step 3: Run tests to verify they pass**
Run:
```bash
bash tests/scripts/test_repo_hygiene_tmp_report_defaults_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary_default_output_contract.sh
bash tests/scripts/test_wave_c_b101_validation_playbook_isolation_passthrough_contract.sh
bash tests/scripts/test_run_all_module_tests_parallel_output_isolation_contract.sh
```
Expected: PASS.

### Task 3: Add March monthly summary and expose it from current index

**Files:**
- Create: `docs/plans/2026-03-current-summary.md`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/README.md`
- Modify: `tests/scripts/test_plans_current_index_contract.sh`

**Step 1: Add the monthly summary**
- Summarize current March entrypoints, completed high-signal waves, and active next queue.
- Keep the page short and explicitly position it as a rollup, not the full history.

**Step 2: Link the summary from active docs entrypoints**
- Add the monthly summary to `docs/PLANS_CURRENT_INDEX.md`.
- Mention the monthly rollup from `docs/plans/README.md`.
- Extend the existing plans-current-index contract so the new summary cannot silently disappear.

**Step 3: Run docs contract to verify**
Run:
```bash
bash tests/scripts/test_plans_current_index_contract.sh
bash tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh
```
Expected: PASS.

### Task 4: Wire hygiene contracts and run final regressions

**Files:**
- Modify: `tests/scripts/test_repo_hygiene_contract_batch.sh`
- Modify: `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Add the new hygiene contract to batch coverage**
- Include `tests/scripts/test_repo_hygiene_tmp_report_defaults_contract.sh`.
- Optionally include the Wave B default-output runtime contract if it stays lightweight and deterministic.

**Step 2: Run final regression set**
Run:
```bash
bash tests/scripts/test_repo_hygiene_tmp_report_defaults_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary_default_output_contract.sh
bash tests/scripts/test_wave_c_b101_validation_playbook_isolation_passthrough_contract.sh
bash tests/scripts/test_run_all_module_tests_parallel_output_isolation_contract.sh
bash tests/scripts/test_plans_current_index_contract.sh
bash tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh
bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh
bash tests/scripts/test_repo_hygiene_contract_batch.sh
git diff --check -- scripts/run_all_module_tests.sh scripts/run_wave_c_b101_validation_playbook.sh scripts/generate_wave_b_cross_platform_summary.sh tests/scripts/test_repo_hygiene_tmp_report_defaults_contract.sh tests/scripts/test_wave_b_cross_platform_summary_default_output_contract.sh tests/scripts/test_run_all_module_tests_parallel_output_isolation_contract.sh tests/scripts/test_wave_c_b101_validation_playbook_isolation_passthrough_contract.sh tests/scripts/test_repo_hygiene_contract_batch.sh tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh tests/scripts/test_plans_current_index_contract.sh docs/PLANS_CURRENT_INDEX.md docs/plans/README.md docs/plans/2026-03-current-summary.md docs/plans/2026-03-08-test-reports-output-policy-and-march-summary.md task_plan.md findings.md progress.md
```
Expected: all commands PASS, and the worktree evidence now shows default outputs moving to `tmp/` while docs have a March rollup entrypoint.

## Result
- Updated `scripts/run_all_module_tests.sh` so default reports now go to `tmp/run_all_module_tests_reports_<run_id>/`, with `FAFAFA_TEST_REPORTS_DIR` as an override.
- Updated `scripts/run_wave_c_b101_validation_playbook.sh` so default report/log outputs now go to `tmp/wave_c_b101_reports_<run_id>/`.
- Updated `scripts/generate_wave_b_cross_platform_summary.sh` so default output now goes to `tmp/wave_b_cross_platform_summary_<run_id>.md`.
- Added `docs/plans/2026-03-current-summary.md` and linked it from the current plans entrypoints.

## Verification
- `bash tests/scripts/test_repo_hygiene_tmp_report_defaults_contract.sh`
- `bash tests/scripts/test_wave_b_cross_platform_summary_default_output_contract.sh`
- `bash tests/scripts/test_wave_c_b101_validation_playbook_isolation_passthrough_contract.sh`
- `bash tests/scripts/test_run_all_module_tests_parallel_output_isolation_contract.sh`
- `bash tests/scripts/test_run_all_module_tests_unit_output_isolation_contract.sh`
- `bash tests/scripts/test_run_all_module_tests_fpc_host_override_contract.sh`
- `bash tests/scripts/test_wave_c_b101_validation_playbook_fpc_host_passthrough_contract.sh`
- `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
- `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
- `bash tests/scripts/test_plans_current_index_contract.sh`
- `bash tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh`
- `git diff --check -- ...`

## Review Snapshot
- live report: `tmp/git_status_noise_summary_current.md`
- `git status` entries: `1854`
- `test_reports_drift`: `944`
- `other`: `1`
- `git ls-files -- test-reports`: `294`
- `docs/plans/*.md`: `356`
- note: this wave changes default outputs for future runs; it does not by itself clean the already-tracked / already-generated historical `test-reports/` surface.
