# Wave C Local Guard Report Dir Policy Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 把 Wave C local-guard 链的默认报告输出从 `test-reports/` 收口到忽略目录 `tmp/wave_c_local_guard_reports/`，避免这条高频运维链继续扩大工作区噪音。

**Architecture:** 这波不改脚本行为语义，只改默认输出策略与默认历史扫描目录。对外保留显式 `--output` / `--reports-dir` 兼容，同时用一个共享环境变量 `FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR` 统一默认目录，并补两层 contract：静态策略 contract + 轻量 runtime contract。

**Tech Stack:** bash scripts, shell contract tests, markdown planning docs.

---

### Task 1: Lock failing contracts for local-guard tmp defaults

**Files:**
- Create: `tests/scripts/test_repo_hygiene_wave_c_local_guard_tmp_defaults_contract.sh`
- Create: `tests/scripts/test_wave_c_local_guard_default_reports_runtime_contract.sh`

**Step 1: Write the failing contracts**
- 静态 contract 锁定 B123/B124/B125/B126/B129/B132/B138/B139/B140/B142/B144 默认目录与历史扫描目录。
- runtime contract 运行 B129 only-platform 模式、B139、B140，确认默认产物落到 `tmp/wave_c_local_guard_reports/`，而不是 `test-reports/`。

**Step 2: Run tests to verify they fail**
Run:
```bash
bash tests/scripts/test_repo_hygiene_wave_c_local_guard_tmp_defaults_contract.sh
bash tests/scripts/test_wave_c_local_guard_default_reports_runtime_contract.sh
```
Expected: FAIL，因为当前脚本仍默认输出到 `test-reports/`。

### Task 2: Move the local-guard chain defaults to tmp

**Files:**
- Modify: `scripts/check_wave_c_local_first_continuity.sh`
- Modify: `scripts/check_wave_c_local_drift_watch.sh`
- Modify: `scripts/run_wave_c_local_first_guard_bundle.sh`
- Modify: `scripts/summarize_wave_c_local_guard_history.sh`
- Modify: `scripts/run_wave_c_local_guard_oncall_check.sh`
- Modify: `scripts/generate_wave_c_local_first_status_snapshot.sh`
- Modify: `scripts/run_wave_c_pre_ci_reenable_full_gate.sh`
- Modify: `scripts/cleanup_wave_c_local_guard_reports.sh`
- Modify: `scripts/check_wave_c_local_guard_consistency.sh`
- Modify: `scripts/export_wave_c_local_guard_status_json.sh`
- Modify: `scripts/run_wave_c_local_guard_ops_pack.sh`

**Step 1: Add one shared default dir policy**
- 在相关脚本中使用：
  - `DEFAULT_REPORTS_DIR="tmp/wave_c_local_guard_reports"`
  - `REPORTS_DIR="${FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR:-$DEFAULT_REPORTS_DIR}"`
- 对没有 `--reports-dir` 的脚本，用 `REPORTS_DIR` 生成默认 `OUTPUT_FILE`。
- 对会扫描历史报告的脚本，把 B123/B124/B125/B126/B129/B138/B140 的 glob 改到 `REPORTS_DIR`。

**Step 2: Keep compatibility**
- 保留显式 `--output` / `--reports-dir` 行为。
- 保留对 `wave_c_quick_sprint_bundle_*.md` 这类仍未迁移证据的既有读取路径。
- 继续让现有 explicit-path runtime contracts 通过。

**Step 3: Re-run tests to verify they pass**
Run:
```bash
bash tests/scripts/test_repo_hygiene_wave_c_local_guard_tmp_defaults_contract.sh
bash tests/scripts/test_wave_c_local_guard_default_reports_runtime_contract.sh
```
Expected: PASS.

### Task 3: Wire repo hygiene batch and run focused regressions

**Files:**
- Modify: `tests/scripts/test_repo_hygiene_contract_batch.sh`
- Modify: `tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`
- Modify: `docs/plans/2026-03-current-summary.md`

**Step 1: Wire contracts**
- Add the new static hygiene contract to repo-hygiene batch.
- Add the runtime contract too if it stays deterministic and fast.

**Step 2: Run regression set**
Run:
```bash
bash tests/scripts/test_repo_hygiene_wave_c_local_guard_tmp_defaults_contract.sh
bash tests/scripts/test_wave_c_local_guard_default_reports_runtime_contract.sh
bash tests/scripts/test_wave_c_local_first_guard_bundle_only_platform_path_check_mode.sh
bash tests/scripts/test_wave_c_local_guard_oncall_only_platform_path_check_passthrough.sh
bash tests/scripts/test_wave_c_pre_ci_reenable_full_gate_oncall_only_platform_passthrough.sh
bash tests/scripts/test_wave_c_local_guard_ops_pack_b138_only_platform_passthrough.sh
bash tests/scripts/test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh
bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh
bash tests/scripts/test_repo_hygiene_contract_batch.sh
bash scripts/summarize_git_status_noise_draft.sh --output tmp/git_status_noise_summary_current.md
git diff --check -- scripts/check_wave_c_local_first_continuity.sh scripts/check_wave_c_local_drift_watch.sh scripts/run_wave_c_local_first_guard_bundle.sh scripts/summarize_wave_c_local_guard_history.sh scripts/run_wave_c_local_guard_oncall_check.sh scripts/generate_wave_c_local_first_status_snapshot.sh scripts/run_wave_c_pre_ci_reenable_full_gate.sh scripts/cleanup_wave_c_local_guard_reports.sh scripts/check_wave_c_local_guard_consistency.sh scripts/export_wave_c_local_guard_status_json.sh scripts/run_wave_c_local_guard_ops_pack.sh tests/scripts/test_repo_hygiene_wave_c_local_guard_tmp_defaults_contract.sh tests/scripts/test_wave_c_local_guard_default_reports_runtime_contract.sh tests/scripts/test_repo_hygiene_contract_batch.sh tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh docs/plans/2026-03-08-wave-c-local-guard-report-dir-policy.md docs/plans/2026-03-current-summary.md task_plan.md findings.md progress.md
```
Expected: PASS. The worktree summary should show future local-guard defaults landing under `tmp/` rather than expanding `test-reports/`.

## Result
- Updated the Wave C local-guard chain so default reports now land in `tmp/wave_c_local_guard_reports/` instead of `test-reports/`.
- Added a shared override path via `FAFAFA_WAVE_C_LOCAL_GUARD_REPORTS_DIR` across the affected scripts.
- Kept explicit `--output` / `--reports-dir` compatibility intact, so existing focused runtime contracts continue to pass.

## Verification
- `bash tests/scripts/test_repo_hygiene_wave_c_local_guard_tmp_defaults_contract.sh`
- `bash tests/scripts/test_wave_c_local_guard_default_reports_runtime_contract.sh`
- `bash tests/scripts/test_wave_c_local_first_guard_bundle_only_platform_path_check_mode.sh`
- `bash tests/scripts/test_wave_c_local_guard_oncall_only_platform_path_check_passthrough.sh`
- `bash tests/scripts/test_wave_c_pre_ci_reenable_full_gate_oncall_only_platform_passthrough.sh`
- `bash tests/scripts/test_wave_c_local_guard_ops_pack_b138_only_platform_passthrough.sh`
- `bash tests/scripts/test_wave_c_local_guard_ops_pack_reports_dir_skip_matrix_batch.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh`
- `bash tests/scripts/test_repo_hygiene_contract_batch.sh`
- `bash scripts/summarize_git_status_noise_draft.sh --output tmp/git_status_noise_summary_current.md`
- `git diff --check -- ...`

## Review Snapshot
- live report: `tmp/git_status_noise_summary_current.md`
- `git status` entries: `1864`
- `test_reports_drift`: `944`
- `other`: `1`
- `git ls-files -- test-reports`: `294`
- `docs/plans/*.md`: `357`
- note: this wave stops the Wave C local-guard chain from adding more default `test-reports/` noise, but it does not delete the existing historical surface.
