# 2026-03-08 Wave C quick/enablement + Wave B/TLS13 report-dir policy

## Goal

收口两条当前最接近、最成链的默认输出路径残留：

- Wave C quick-sprint / enablement
- Wave B / TLS13 signer gate

目标不是清理历史 `test-reports/` 存量，而是先阻止新默认噪音继续长出来。

## Scope

- Wave C quick-sprint / enablement 脚本：
  - `scripts/run_wave_c_quick_sprint_bundle.sh`
  - `scripts/evaluate_wave_c_b101_thresholds.sh`
  - `scripts/check_wave_c_default_on_readiness.sh`
  - `scripts/prepare_wave_c_b109_canary_rollout.sh`
  - `scripts/run_wave_c_b110_rollback_drill.sh`
  - `scripts/check_wave_c_workflow_enable_prereq.sh`
  - `scripts/prepare_wave_c_b116_enablement_packet.sh`
  - `scripts/check_wave_c_first_run_preflight.sh`
  - `scripts/check_wave_c_post_trigger_observability.sh`
- Wave B / TLS13 signer gate 脚本：
  - `scripts/run_wave_b_ci_gate.sh`
  - `scripts/run_windows_winssl_blocker_batch_draft.sh`
  - `scripts/generate_wave_b_cross_platform_summary.sh`
  - `scripts/check_wave_b_b2_closure_readiness.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - `scripts/run_tls13_signer_gate_ci.sh`
  - `scripts/generate_tls13_signer_gate_snapshot.sh`
  - `scripts/export_tls13_signer_gate_status_json.sh`
  - `scripts/run_tls13_signer_gate_bundle.sh`
  - `scripts/summarize_tls13_signer_bench_history.sh`
- 合同与 working memory：
  - 新增 4 个 focused contracts
  - 修正 2 个既有合同
  - 更新 repo-hygiene batch / coverage contract
  - 更新月度汇总与 `task_plan.md` / `findings.md` / `progress.md`

## Architecture

### Wave C

- quick-sprint 共享目录：`tmp/wave_c_quick_sprint_reports`
- enablement 共享目录：`tmp/wave_c_enablement_reports`
- B101 validation 默认输入：`tmp/wave_c_b101_reports_*/wave_c_b101_validation_*.md`
- 环境变量：
  - `FAFAFA_WAVE_C_QUICK_SPRINT_REPORTS_DIR`
  - `FAFAFA_WAVE_C_ENABLEMENT_REPORTS_DIR`
  - `FAFAFA_WAVE_C_B101_VALIDATION_GLOB`

### Wave B / TLS13

- Wave B 共享目录：`tmp/wave_b_reports`
- TLS13 signer gate 共享目录：`tmp/tls13_signer_gate_reports`
- 环境变量：
  - `FAFAFA_WAVE_B_REPORTS_DIR`
  - `FAFAFA_TLS13_SIGNER_GATE_REPORTS_DIR`
  - `FAFAFA_TLS13_SIGN_BENCH_HISTORY_GLOB`

### Policy

- 同一链路内部默认读写稳定共享目录，而不是 `test-reports/`
- wrapper / downstream reader 的“最新报告”扫描必须和新的共享目录一致
- 仍保留显式 `--output` / `--reports-dir` / `--output-dir` 覆盖能力
- 对 shell 直写输出的脚本，默认目录也要能自动创建，避免换到 `tmp/` 后首次运行失败

## RED -> GREEN Plan

1. 新增 `tests/scripts/test_repo_hygiene_wave_c_quick_enablement_tmp_defaults_contract.sh`
2. 新增 `tests/scripts/test_wave_c_quick_enablement_default_reports_runtime_contract.sh`
3. 新增 `tests/scripts/test_repo_hygiene_wave_b_tls13_tmp_defaults_contract.sh`
4. 新增 `tests/scripts/test_wave_b_tls13_default_reports_runtime_contract.sh`
5. 运行新合同 + 受影响旧合同，确认 RED
6. 修补 Wave C 默认目录自动创建与遗漏断言
7. 修正旧合同默认路径断言并接入 repo-hygiene batch
8. 运行 focused contracts + repo-hygiene batch，确认 GREEN
9. 更新月度汇总与 working memory

## Planned Commands

Run:

```bash
bash tests/scripts/test_repo_hygiene_wave_c_quick_enablement_tmp_defaults_contract.sh
bash tests/scripts/test_wave_c_quick_enablement_default_reports_runtime_contract.sh
bash tests/scripts/test_repo_hygiene_wave_b_tls13_tmp_defaults_contract.sh
bash tests/scripts/test_wave_b_tls13_default_reports_runtime_contract.sh
bash tests/scripts/test_repo_hygiene_tmp_report_defaults_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary_default_output_contract.sh
```

Then:

```bash
bash tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh
bash tests/scripts/test_repo_hygiene_contract_batch.sh
git diff --check --   scripts/check_wave_c_workflow_enable_prereq.sh   scripts/prepare_wave_c_b116_enablement_packet.sh   scripts/check_wave_c_first_run_preflight.sh   scripts/check_wave_c_post_trigger_observability.sh   scripts/check_wave_c_default_on_readiness.sh   scripts/prepare_wave_c_b109_canary_rollout.sh   scripts/run_wave_c_b110_rollback_drill.sh   tests/scripts/test_repo_hygiene_tmp_report_defaults_contract.sh   tests/scripts/test_wave_b_cross_platform_summary_default_output_contract.sh   tests/scripts/test_repo_hygiene_wave_c_quick_enablement_tmp_defaults_contract.sh   tests/scripts/test_wave_c_quick_enablement_default_reports_runtime_contract.sh   tests/scripts/test_repo_hygiene_wave_b_tls13_tmp_defaults_contract.sh   tests/scripts/test_wave_b_tls13_default_reports_runtime_contract.sh   tests/scripts/test_repo_hygiene_contract_batch.sh   tests/scripts/test_repo_hygiene_contract_batch_coverage_contract.sh   docs/plans/2026-03-current-summary.md   docs/plans/2026-03-08-wave-c-quick-enablement-and-wave-b-tls13-report-dir-policy.md   docs/plans/2026-03-08-test-reports-historical-surface-cleanup-plan.md   task_plan.md findings.md progress.md
```

## Expected Outputs

- 新默认输出全部落在 `tmp/` 共享目录，而不是 `test-reports/`
- Wave C quick/enablement runtime 合同覆盖默认输出 + 最新报告回填路径
- Wave B/TLS13 runtime 合同覆盖默认输出 + handoff/snapshot/status 路径
- repo-hygiene super-batch 接入这四个新合同并保持 green
