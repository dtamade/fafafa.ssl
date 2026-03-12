# Wave B CI Gate Quoted-Path Safety Plan

**Goal**
- 让 `scripts/run_wave_b_ci_gate.sh` 在 `FAFAFA_WAVE_B_REPORTS_DIR` 含单引号时仍能稳定执行。
- 保持 dry-run 输出、FPC host passthrough、isolation passthrough 这三条既有 contract 语义不变。

**Architecture**
- `run_wave_b_ci_gate.sh` 不再依赖 `zsh -lc` / `bash -lc` 执行字符串命令，改为在 subshell 中 `cd "$PROJECT_ROOT"` 后直接执行参数数组。
- 为了不打破既有 dry-run / summary contract，展示层继续保留 shell 风格 display string；执行层与展示层分离。
- 这波只处理 Wave B 主入口，不扩写 `generate_tls13_signer_gate_snapshot.sh` / `export_tls13_signer_gate_status_json.sh` 的发现逻辑。
- quoted-path 合同只覆盖 TLS13 purity + bench 快路径，避免把 compile/modules/examples 的耗时带进合同。

**Files**
- Add: `docs/plans/2026-03-09-wave-b-ci-gate-quoted-path-safety.md`
- Add: `tests/scripts/test_wave_b_ci_gate_reports_dir_quote_contract.sh`
- Modify: `scripts/run_wave_b_ci_gate.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 quoted-path 合同，确认 `run_step` shell-string RED。
2. 把主入口切到参数式执行，同时保留旧展示字符串语义。
3. 跑新合同与两条既有 passthrough 合同。
4. 跑 `bash -n` 与 `git diff --check`。
5. 回写 working memory 与当前汇总。

**Expected Outputs**
- Wave B 主入口在 quoted reports-dir 下不再因 shell 解析失真而失败。
- dry-run / passthrough contract 继续保持绿色。
- 下一波可以继续处理 snapshot/status/closure/evidence 的发现与 passthrough 一致性。
