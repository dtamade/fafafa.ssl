# 2026-05-13 Wave B/B2 Consistency Existing Report Run ID Fallback

## Goal
收口 `scripts/check_wave_b_b2_evidence_consistency.sh` 在 active Linux summary 已缺失场景下的 `run_id` 次生污染，避免脚本明明拿到了现有 `cross summary` / `closure report`，却仍回退到新的时间戳并把这两份已对齐报告误记成 `run_id mismatch`。

## Architecture
- `RUN_ID` 默认真值继续保持“优先跟现有证据批次走”的原则：
  - 显式 `--run-id`
  - Linux summary（显式或已知 active path）
  - cross-summary-declared active Linux summary
  - 现有 `cross summary` 自身的 `run_id`
  - 现有 `closure report` 自身的 `run_id`
  - 最后才是时间戳
- 这样即使 active Linux summary 已丢失，consistency 也仍应围绕现有 summary-chain 的真实批次出报告。
- 这次仍只改 `check_wave_b_b2_evidence_consistency.sh`。

## Files
- `scripts/check_wave_b_b2_evidence_consistency.sh`
- `tests/scripts/test_wave_b_b2_consistency_existing_report_run_id_fallback_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED contract，复现“active Linux summary 缺失，但 cross summary / closure 已对齐”的假噪音 mismatch。
2. 在 `check_wave_b_b2_evidence_consistency.sh` 内增加从现有 markdown reports 自身回收 `run_id` 的 fallback。
3. 跑 focused 合同、上一批 cross-summary-driven run_id 合同，以及 Linux/macOS/Windows 邻近回归。
4. 更新 working memory，给出简短 review 结论并提交。

## Commands
- `bash -n tests/scripts/test_wave_b_b2_consistency_existing_report_run_id_fallback_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_existing_report_run_id_fallback_contract.sh`
- `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_required_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`
- `git diff --check`

## Expected Outputs
- 新合同在修复前失败，并显示 `required_missing=1` 之外还多出 `cross_summary` / `closure_report` 的假 mismatch。
- 修复后报告仍为 `INCONSISTENT`，但只因缺失 active Linux summary 而失败，`cross summary` / `closure report` 不再被时间戳污染。
- 上一批 cross-summary-driven run_id 合同与邻近 active-path / strictness 合同继续通过。
