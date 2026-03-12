# Wave C Enablement CLI Reports-Dir Passthrough Plan

**Goal**
- 让 Wave C enablement 链在不依赖环境变量的情况下，通过 CLI 参数稳定切换 quick reports / enablement reports 目录。
- 保持现有默认目录策略与判定语义不变，只补齐 orchestration surface。

**Architecture**
- `scripts/run_wave_c_quick_sprint_bundle.sh` 已经支持 `--reports-dir`，因此这波只补下游 enablement 三支脚本的 CLI 面。
- `scripts/check_wave_c_workflow_enable_prereq.sh` 与 `scripts/prepare_wave_c_b116_enablement_packet.sh` 新增 `--reports-dir`，统一控制 enablement 输出目录与 B115 latest lookup。
- `scripts/check_wave_c_first_run_preflight.sh` 新增 `--reports-dir` 与 `--quick-reports-dir`，分别控制 preflight 输出目录与 quick bundle 默认发现目录。
- 这波不改默认值、不改 strict 规则、不改报告格式主体；只让 caller 不必再混用 env + CLI。

**Files**
- Add: `docs/plans/2026-03-09-wave-c-enablement-cli-reports-dir-passthrough.md`
- Add: `tests/scripts/test_wave_c_enablement_cli_reports_dir_passthrough_contract.sh`
- Modify: `scripts/check_wave_c_workflow_enable_prereq.sh`
- Modify: `scripts/prepare_wave_c_b116_enablement_packet.sh`
- Modify: `scripts/check_wave_c_first_run_preflight.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 CLI reports-dir passthrough 合同并确认 RED。
2. 给 B115 / B116 增加 `--reports-dir`。
3. 给 B119 增加 `--reports-dir` 与 `--quick-reports-dir`。
4. 跑 focused 合同、既有 default/runtime 回归与 `bash -n`。
5. 回写 working memory 与当前汇总。

**Expected Outputs**
- caller 可以只靠 CLI 参数串起 quick-sprint / enablement 链，不再强依赖 `FAFAFA_WAVE_C_*_REPORTS_DIR`。
- B116 默认 latest B115 lookup 会跟随 CLI reports-dir。
- B119 默认 latest bundle lookup 会跟随 CLI quick-reports-dir。
