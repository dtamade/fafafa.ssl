# Wave B Cross-Platform Summary Absolute Input Hardening Plan

**Goal:** 修复 `scripts/generate_wave_b_cross_platform_summary.sh` 的 absolute 输入路径解析，让跨目录调用时的 absolute summary/json 参数不再被错误拼接到项目根。

**Architecture:** 这批只收 `generate_wave_b_cross_platform_summary.sh`，不扩到其他脚本。顺序是：
- 先用 focused contract 证明 absolute 输入路径当前会被误判；
- 再最小补齐输入路径归一化；
- 最后用旧 contract + 新 contract 共同回归。

**Files:**

- Add: `tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh`
- Update: `scripts/generate_wave_b_cross_platform_summary.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED contract for absolute input paths

Run:

```bash
bash -n tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh
```

Expected before fix:

- `bash -n` passes
- runtime contract fails because the script still checks absolute inputs via `"$PROJECT_ROOT/$INPUT_PATH"`

## Task 2: Minimal input-path normalization

Patch:

- move `resolve_path(...)` before the Linux summary existence guard
- normalize:
  - `LINUX_SUMMARY`
  - `LINUX_EXAMPLES_JSON`
  - `MACOS_PROBE`
  - `MACOS_SUMMARY`
  - `WINDOWS_SUMMARY`
- switch metadata/step/json reads to the normalized absolute paths

Do not broaden this batch into new output-path changes or unrelated summary refactors.

## Task 3: Regression verification

Run:

```bash
bash -n scripts/generate_wave_b_cross_platform_summary.sh
bash tests/scripts/test_wave_b_cross_platform_summary.sh
bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh
bash tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh
bash -n tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh
bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh
git diff --check
```

Expected after fix:

- absolute input contract turns GREEN
- old relative-input contracts stay GREEN
- previous absolute-output contract stays GREEN

## Execution Result

- RED was captured as expected:
  - `bash tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh` failed on `Linux summary not found`
- Minimal input-path hardening landed only in `scripts/generate_wave_b_cross_platform_summary.sh`.
- GREEN verification passed:
  - `bash -n scripts/generate_wave_b_cross_platform_summary.sh`
  - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
  - `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
  - `bash tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh`
  - `bash -n tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh`
  - `bash tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh`
  - `bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh`
  - `git diff --check`
