# 2026-05-14 Wave B/B2 Explicit Missing Evidence Passthrough

## Goal
收口 `prepare_wave_b_b2_handoff_bundle.sh` 与 `generate_wave_b_cross_platform_summary.sh` 对显式缺失证据路径的吞参/静默降级问题，避免调用者明确传入 `--macos-summary` / `--windows-summary` / `--macos-probe` 后，下游摘要仍写成 `no evidence`，甚至让 consistency 假绿。

## Architecture
- `prepare` 层：
  - 显式 CLI 参数代表调用者要求校验的证据路径
  - 即使文件缺失，也必须继续传给 downstream 脚本，而不是因为 `-f` 失败就静默丢弃
- `generate` 层：
  - 显式缺失 `macos_summary` / `windows_summary` 应显示为 `summary: <path> (missing file)`
  - 显式缺失 `macos_probe` 在无 summary 时应显示为 `probe: <path> (missing file)`
  - 默认未提供且默认路径缺失时，仍保持现有 `no evidence`
- `consistency` 层：
  - 一旦 `prepare` 不再吞掉显式路径，现有 explicit-required 语义应自动把缺失 summary / companion artifacts 记入 `required_missing`

## Files
- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `scripts/generate_wave_b_cross_platform_summary.sh`
- `tests/scripts/test_wave_b_cross_platform_summary_explicit_missing_evidence_contract.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED contracts，复现 direct `generate` 与 `prepare` 入口对显式缺失 evidence 的吞参与静默 `no evidence`。
2. 在 `prepare` 中增加 explicit flags，并让显式缺失路径继续传给 `generate` / `closure` / `consistency`。
3. 在 `generate` 中区分“显式缺失”和“默认无证据”，把前者展示成 missing-file truth。
4. 跑 focused 合同与相邻回归，更新 working memory，review 后提交。

## Commands
- `bash -n tests/scripts/test_wave_b_cross_platform_summary_explicit_missing_evidence_contract.sh`
- `bash tests/scripts/test_wave_b_cross_platform_summary_explicit_missing_evidence_contract.sh`
- `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`
- `bash -n scripts/generate_wave_b_cross_platform_summary.sh`
- `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_explicit_summary_artifacts_required_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_explicit_windows_runtime_logs_required_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_existing_report_run_id_fallback_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh`
- `bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh`
- `git diff --check`

## Expected Outputs
- `generate` 在显式缺失 summary/probe 场景下不再写 `no evidence`，而是显示具体 missing path。
- `prepare` 在显式缺失 summary 场景下不再让 consistency 假绿，handoff state 会正确落到 `NEEDS_EVIDENCE_SYNC`。
- 现有 explicit-required / run_id / inactive-probe 合同继续通过。
