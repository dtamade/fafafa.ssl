# TLS13 Signer Gate Bundle Quoted-Path Safety Plan

**Goal**
- 让 `scripts/run_tls13_signer_gate_bundle.sh` 在 `--reports-dir` 含单引号时仍能稳定通过。
- 保持 TLS13 signer gate 的默认 reports-dir 和现有 snapshot/status 语义不变。

**Architecture**
- `scripts/run_tls13_signer_gate_bundle.sh` 不再通过 `eval` 执行带引号拼接的 shell 字符串，改为参数式调用与 `env` 前缀。
- `run_tls13_signer_gate_ci.sh`、`generate_tls13_signer_gate_snapshot.sh`、`export_tls13_signer_gate_status_json.sh` 本身保持不变；bundle 只负责安全地把参数与环境传下去。
- 验证层使用 fake `bash` stub 隔离重型 CI 步骤，让合同聚焦在 bundle 的路径安全而不是实际编译耗时。
- 既有 `tests/scripts/test_wave_b_tls13_default_reports_runtime_contract.sh` 继续兜底默认 reports-dir 语义不回退。

**Files**
- Add: `docs/plans/2026-03-09-tls13-signer-gate-bundle-quoted-path-safety.md`
- Add: `tests/scripts/test_tls13_signer_gate_bundle_reports_dir_quote_contract.sh`
- Modify: `scripts/run_tls13_signer_gate_bundle.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 quoted-path 合同，确认 `eval` 路径失败 RED。
2. 把 bundle 改成参数式 `run_step`，并用 `env` 传递环境变量。
3. 跑新合同与既有 TLS13 default reports 合同。
4. 跑 `bash -n` 与 `git diff --check`。
5. 回写 working memory 与当前汇总。

**Expected Outputs**
- TLS13 signer gate bundle 在 quoted reports-dir 下不再因为 shell 拼接失真而失败。
- 默认 reports-dir contract 继续保持绿色。
- 下一波可以把同类 shell-string 风险继续下沉到 `scripts/run_wave_b_ci_gate.sh`。
