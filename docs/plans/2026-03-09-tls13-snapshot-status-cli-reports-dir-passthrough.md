# TLS13 Snapshot Status CLI Reports-Dir Passthrough Plan

**Goal**
- 让 `generate_tls13_signer_gate_snapshot.sh` / `export_tls13_signer_gate_status_json.sh` 支持 CLI `--reports-dir`，不再要求 caller 必须通过 env 改默认目录。
- 保持现有 run-scoped default discovery、stale-fallback 收口和默认目录策略不变。

**Architecture**
- `run_tls13_signer_gate_bundle.sh` 已经有 `--reports-dir`，但 snapshot/status 直连脚本仍只有 env surface，导致 direct caller 与 bundle caller 的接口不一致。
- 给两个直连脚本补 `--reports-dir` 即可；其余 `--summary` / `--bench-json` / `--history` / `--snapshot` 明确覆盖语义保持不变。
- 这波不改默认发现优先级，只让 `REPORTS_DIR` 的来源从“env only”扩成“CLI > env > default”。

**Files**
- Add: `docs/plans/2026-03-09-tls13-snapshot-status-cli-reports-dir-passthrough.md`
- Add: `tests/scripts/test_tls13_signer_gate_cli_reports_dir_passthrough_contract.sh`
- Modify: `scripts/generate_tls13_signer_gate_snapshot.sh`
- Modify: `scripts/export_tls13_signer_gate_status_json.sh`
- Modify: `docs/PLANS_CURRENT_INDEX.md`
- Modify: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 CLI reports-dir passthrough 合同并确认 RED。
2. 给 snapshot/status 两个脚本补 `--reports-dir`。
3. 跑新合同 + stale-fallback / bundle / default-runtime 回归。
4. 回写 working memory 与当前汇总。

**Expected Outputs**
- direct caller 可以只靠 CLI 切换 TLS13 reports dir。
- bundle caller 与直连 caller 的目录控制接口一致。
- 已有 run-id scoped default selection / stale-fallback 行为继续保持绿色。
