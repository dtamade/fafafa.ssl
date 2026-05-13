# Wave B/B2 Absolute Output Path Hardening Plan

**Goal:** 修复 `Wave B/B2` 报告脚本链的 absolute output path 语义，让 `--output FILE` 与 `prepare_wave_b_b2_handoff_bundle.sh --output-dir DIR` 在使用绝对路径时真正写到目标位置。

**Architecture:** 这批继续保持纯脚本层静态修复，不碰 Pascal 生产代码，也不重开运行平台话题。顺序是：
- 先用 focused contract 证明 absolute output path 当前会落错位置；
- 再最小修复三份报告脚本的输出路径归一化；
- 最后用同一个 contract 验证 handoff bundle 的 absolute output-dir 也被一并带绿。

**Files:**

- Add: `tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh`
- Update: `scripts/generate_wave_b_cross_platform_summary.sh`
- Update: `scripts/check_wave_b_b2_closure_readiness.sh`
- Update: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED contract for absolute output paths

Run:

```bash
bash -n tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh
bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh
```

Expected before fix:

- `bash -n` passes
- runtime contract fails because:
  - scripts are invoked from `/tmp`
  - inputs stay relative to the repo
  - outputs are absolute paths/directories
  - current scripts still prepend `PROJECT_ROOT` when writing reports

## Task 2: Minimal output write-path normalization

Patch only the output write sites:

- `scripts/generate_wave_b_cross_platform_summary.sh`
  - add local `resolve_path(...)`
  - use `OUTPUT_ABS` for mkdir/write
- `scripts/check_wave_b_b2_closure_readiness.sh`
  - resolve `OUTPUT_FILE` before mkdir/write
- `scripts/check_wave_b_b2_evidence_consistency.sh`
  - resolve `OUTPUT_FILE` before mkdir/write

Do not broaden this batch into general input-path refactors.

## Task 3: Verify full chain

Run:

```bash
bash -n scripts/generate_wave_b_cross_platform_summary.sh
bash -n scripts/check_wave_b_b2_closure_readiness.sh
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash -n tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh
bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh
bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh
git diff --check
```

Expected after fix:

- cross summary / closure / consistency all honor absolute `--output`
- handoff bundle honors absolute `--output-dir` end-to-end
- existing workflow contract remains green

## Execution Result

- RED was captured as expected:
  - `bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh` failed on missing absolute output for cross summary
- Minimal output-path hardening landed in the three report scripts only.
- GREEN verification passed:
  - `bash -n scripts/generate_wave_b_cross_platform_summary.sh`
  - `bash -n scripts/check_wave_b_b2_closure_readiness.sh`
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`
  - `bash -n tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh`
  - `bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh`
  - `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
  - `git diff --check`
