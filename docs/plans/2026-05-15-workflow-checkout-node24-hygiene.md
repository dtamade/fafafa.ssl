# 2026-05-15 Workflow Node24 Hygiene

## Goal

清理仓库中 GitHub Actions workflow / template 里残留的 Node20-era JavaScript actions，先把 `actions/checkout` 升级到 Node24 兼容线，再把 `actions/upload-artifact` 升级到 Node24 默认线，并保证现有活跃工作流与同步模板不回退。

## Architecture

- GitHub Actions 当前远端 annotation 先后指向：
  - `actions/checkout@v4`
  - `actions/upload-artifact@v4`
- 活跃 workflow 绿了以后，最高价值的问题已不是运行时逻辑，而是 workflow runtime hygiene
- 只修活跃 workflow 不够：
  - `release.yml.disabled`
  - `wave-b-b2-manual.yml.disabled`
  - 以及其他 dormant templates
  未来一旦重新启用，仍会把旧的 Node20 告警带回来

## Files

- `.github/workflows/*.yml`
- `.github/workflows/*.yml.disabled`
- `tests/scripts/test_workflow_checkout_node24_contract.sh`
- `tests/scripts/test_workflow_upload_artifact_node24_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 用 focused contract 先锁定 `.github/workflows` 中残留的 `actions/checkout@v3/v4`。
2. 将所有 workflow / template 的 checkout action 升级到 `actions/checkout@v5`。
3. 观察远端 runs；若 annotation 前移到 `actions/upload-artifact@v4`，再补第二条 focused contract。
4. 将所有 workflow / template 的 artifact upload action 升级到 `actions/upload-artifact@v6`。
5. 复跑关键 workflow contracts，确认 release / signer / completeness 没有回退。
6. 检查同步模板仍保持一致。
7. 更新 working-memory，review，commit 并 push。
8. 观察新的远端 `CI` / `TLS13 Signer Gate` runs，确认升级后继续通过。

## Commands

```bash
bash tests/scripts/test_workflow_checkout_node24_contract.sh
bash tests/scripts/test_workflow_upload_artifact_node24_contract.sh
bash tests/scripts/test_release_workflow_v1_5_0_contract.sh
bash tests/scripts/test_tls13_signer_gate_workflow_contract.sh
bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh
cmp -s .github/workflows/release.yml .github/workflows/release.yml.disabled
cmp -s .github/workflows/wave-b-b2-manual.yml .github/workflows/wave-b-b2-manual.yml.disabled
git diff --check
```

## Expected Outputs

- `.github/workflows` 下不再残留 `actions/checkout@v3/v4` 或 `actions/upload-artifact@v3/v4/v5`
- 活跃 workflow 和同步模板显式使用 `actions/checkout@v5` 与 `actions/upload-artifact@v6`
- release / signer / completeness 合同继续通过
- 远端 CI / signer 不再出现由 checkout 或 upload-artifact 旧 runtime 引发的 Node20 deprecation annotation
