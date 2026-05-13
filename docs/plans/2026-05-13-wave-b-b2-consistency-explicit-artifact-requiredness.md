# 2026-05-13 Wave B/B2 Consistency Explicit Artifact Requiredness

## Goal
收口 `scripts/check_wave_b_b2_evidence_consistency.sh` 对显式非 Linux 证据参数的 required 语义漂移，避免调用者明确传入 `--macos-summary` / `--windows-summary` / Windows runtime logs 后，strict 仍把这些缺失 evidence 静默降成 optional。

## Architecture
- 统一原则：
  - 显式传入的 artifact path 就是调用者要求校验的 evidence
  - 因此缺失时必须进入 `required_missing`
- Windows 额外遵守现有 runtime lane 语义：
  - 显式 `--windows-summary` 不仅要求 summary 自身
  - 也应像 cross-summary-active Windows truth 一样激活 sibling `quick log` / `runtime transcript`
- 显式 `--windows-quick-log` / `--windows-runtime-transcript` 则各自独立成为 required evidence。

## Files
- `scripts/check_wave_b_b2_evidence_consistency.sh`
- `tests/scripts/test_wave_b_b2_consistency_explicit_summary_artifacts_required_contract.sh`
- `tests/scripts/test_wave_b_b2_consistency_explicit_windows_runtime_logs_required_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED contracts，复现显式 macOS/Windows summary 与显式 Windows runtime logs 在 strict 下仍被静默放过。
2. 在 `check_wave_b_b2_evidence_consistency.sh` 内统一显式非 Linux evidence 的 required 语义。
3. 跑 focused 合同、run_id 回归、active-path / Windows strict / inactive-probe 邻近回归。
4. 更新 working memory，给出简短 review 结论并提交。

## Commands
- `bash -n tests/scripts/test_wave_b_b2_consistency_explicit_summary_artifacts_required_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_explicit_summary_artifacts_required_contract.sh`
- `bash -n tests/scripts/test_wave_b_b2_consistency_explicit_windows_runtime_logs_required_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_explicit_windows_runtime_logs_required_contract.sh`
- `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_existing_report_run_id_fallback_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_required_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`
- `git diff --check`

## Expected Outputs
- 修复前两条新合同都失败，并显示显式传入的非 Linux evidence 仍被当成 optional。
- 修复后 strict 会把这些显式 evidence 计入 `required_missing`。
- 现有 run_id、active-path、Windows required 与 inactive-probe 合同继续通过。
