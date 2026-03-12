# Wave C Quick Sprint Spaced-Path Safety Plan

**Goal**
- 让 Wave C quick-sprint bundle 在 `--reports-dir` 含空格时仍能整链通过。
- 保持 quick-sprint reports 与 enablement reports 的目录边界不变，不把两个输出面重新混成一处。

**Architecture**
- `scripts/run_wave_c_quick_sprint_bundle.sh` 不再用字符串 `eval` 执行下游步骤，改为参数式调用，避免路径拆词。
- `scripts/check_wave_c_default_on_readiness.sh`、`scripts/prepare_wave_c_b109_canary_rollout.sh`、`scripts/run_wave_c_b110_rollback_drill.sh` 统一通过安全 helper 解析最新 B101 validation report，不再裸用 `ls -1t $VALIDATION_GLOB`。
- quick-sprint 默认目录仍是 `tmp/wave_c_quick_sprint_reports`，enablement 默认目录仍是 `tmp/wave_c_enablement_reports`；这波只做路径安全加固，不改默认输出策略。
- 验证面由新 spaces contract + 既有 default reports runtime contract 双重兜底。

**Files**
- Add: `docs/plans/2026-03-09-wave-c-quick-sprint-spaced-path-safety.md`
- Add: `tests/scripts/test_wave_c_quick_sprint_bundle_reports_dir_spaces_contract.sh`
- Modify: `scripts/run_wave_c_quick_sprint_bundle.sh`
- Modify: `scripts/check_wave_c_default_on_readiness.sh`
- Modify: `scripts/prepare_wave_c_b109_canary_rollout.sh`
- Modify: `scripts/run_wave_c_b110_rollback_drill.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加含空格 reports-dir 合同，确认 RED。
2. 去掉 bundle 的 `eval`，修第一层拆词问题。
3. 继续修 B108/B109/B110 的 validation glob 解析路径。
4. 跑新合同、既有 default reports 合同、`bash -n` 验证。
5. 回写 working memory 与当前汇总。

**Expected Outputs**
- Wave C quick-sprint bundle 在含空格路径下也能稳定生成 B107/B108/B109/B110 与 bundle 汇总报告。
- 既有 quick reports / enablement reports 默认目录契约保持绿色。
- 下一波可以复用同一合同模式处理 `run_tls13_signer_gate_bundle.sh` 的同类 `eval` 风险。
