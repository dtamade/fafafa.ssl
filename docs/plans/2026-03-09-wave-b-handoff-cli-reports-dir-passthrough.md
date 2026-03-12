# Wave B Handoff CLI Reports-Dir Passthrough Plan

**Goal**
- 给 `prepare_wave_b_b2_handoff_bundle.sh` 补齐 caller-facing `--reports-dir`，让默认输入发现与默认输出目录重新对齐。
- 保持现有 `--output-dir` 显式覆盖语义不变。

**Architecture**
- handoff 入口内部同时有两类目录：`REPORTS_DIR` 负责默认输入发现，`OUTPUT_DIR` 负责产物输出。
- 之前 `OUTPUT_DIR` 在参数解析前就绑定到初始默认值，导致新增 `--reports-dir` 后，如果 caller 不显式传 `--output-dir`，输出仍会落到旧默认目录。
- 最小正确修复是：把 `OUTPUT_DIR` 改成 parse-after-default，并在 wrapper fan-out 时把 `REPORTS_DIR` 通过 env 透传给 child consumer。
- 这波不改 child checker 的判定规则，只修 wrapper orchestration surface。

**Files**
- Add: `docs/plans/2026-03-09-wave-b-handoff-cli-reports-dir-passthrough.md`
- Add: `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_cli_reports_dir_passthrough_contract.sh`
- Modify: `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 handoff CLI passthrough 合同并确认 RED。
2. 给 handoff 增加 `--reports-dir`，并让默认 `OUTPUT_DIR` 跟随它。
3. 在 fan-out 到 cross/closure/evidence 时透传 `FAFAFA_WAVE_B_REPORTS_DIR`。
4. 跑 focused 合同与既有 handoff / runtime 回归。
5. 回写 working memory 与当前汇总。

**Expected Outputs**
- caller 只给 `--reports-dir` 时，handoff 会从该目录发现输入，并默认把 cross/closure/evidence/bundle 全部写回同一目录。
- handoff wrapper 不再把 child consumer 拉回旧默认 reports dir。
- 显式 `--output-dir` 仍可独立覆盖输出位置。
