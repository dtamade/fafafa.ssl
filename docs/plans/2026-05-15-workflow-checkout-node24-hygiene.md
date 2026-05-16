# 2026-05-15 Workflow Node24 Hygiene

## Goal

清理仓库中 GitHub Actions workflow / template 里残留的 Node20-era JavaScript actions，分五波收口：先修 `actions/checkout`，再修 `actions/upload-artifact`，第三波修 `actions/download-artifact`，第四波修 `softprops/action-gh-release`、`actions/setup-python`、`actions/cache`，第五波去掉不可替代性不足的 `gcarreno/setup-lazarus`，并保证现有活跃 workflow 与同步模板不回退。

## Architecture

- GitHub Actions 当前远端 annotation 先后指向：
  - `actions/checkout@v4`
  - `actions/upload-artifact@v4`
- 当前仓库静态扫描还暴露出：
  - `actions/download-artifact@v4`
  - `softprops/action-gh-release@v2`
  - `actions/setup-python@v5`
  - `actions/cache@v4`
  - `gcarreno/setup-lazarus@v3`
- 活跃 workflow 绿了以后，最高价值的问题已不是运行时逻辑，而是 workflow runtime hygiene
- 只修活跃 workflow 不够：
  - `release.yml.disabled`
  - `wave-b-b2-manual.yml.disabled`
  - 以及其他 dormant templates
  未来一旦重新启用，仍会把旧的 Node20 告警带回来
- 版本边界必须跟随官方 release 真相，而不是沿用过时计划：
  - `actions/checkout@v5` 是当前仓库选择的最小 Node24 默认线
  - `actions/upload-artifact@v6` 是当前仓库选择的最小 Node24 默认线
  - `actions/download-artifact@v7` 是当前仓库选择的最小 Node24 默认线
  - `softprops/action-gh-release@v3` 是当前仓库选择的最小 Node24 默认线
  - `actions/setup-python@v6` 是当前仓库选择的最小 Node24 默认线
  - `actions/cache@v5` 是当前仓库选择的最小 Node24 默认线
- `download-artifact` 这一波的验证边界与前两波不同：
  - 活跃命中点在 `wave-b-b2-manual.yml`
  - 它是 `workflow_dispatch`
  - 因此 `CI` / `TLS13 Signer Gate` 不能作为这波变更的 runtime 验证代理
  - 当前批次按用户约束保持 `static-only`
- `gcarreno/setup-lazarus@v3.4.1` 当前仍是 `node20`，但上游暂未观察到 Node24 继任 major：
  - 最终并未保留为阻塞
  - 因为仓库内已有可复用的手工安装模式，可以直接去依赖化

## Files

- `.github/workflows/*.yml`
- `.github/workflows/*.yml.disabled`
- `tests/scripts/test_workflow_checkout_node24_contract.sh`
- `tests/scripts/test_workflow_upload_artifact_node24_contract.sh`
- `tests/scripts/test_workflow_download_artifact_node24_contract.sh`
- `tests/scripts/test_workflow_setup_python_node24_contract.sh`
- `tests/scripts/test_workflow_cache_node24_contract.sh`
- `tests/scripts/test_workflow_lazarus_setup_node24_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 用 focused contract 先锁定 `.github/workflows` 中残留的 `actions/checkout@v3/v4`。
2. 将所有 workflow / template 的 checkout action 升级到 `actions/checkout@v5`。
3. 观察远端 runs；若 annotation 前移到 `actions/upload-artifact@v4`，再补第二条 focused contract。
4. 将所有 workflow / template 的 artifact upload action 升级到 `actions/upload-artifact@v6`。
5. 重新核对官方 release truth，修正过时的 `download-artifact` 目标版本。
6. 用 focused contract 锁定 `.github/workflows` 中残留的 `actions/download-artifact@v3` 到 `@v6`。
7. 将所有 workflow / template 的 artifact download action 升级到 `actions/download-artifact@v7`。
8. 强化 release 合同，并新增 setup-python / cache focused contracts。
9. 将 `softprops/action-gh-release`、`actions/setup-python`、`actions/cache` 升级到各自 Node24 默认线。
10. 为 `gcarreno/setup-lazarus` 添加 focused contract，并验证它是否真的不可替代。
11. 将 `test-all-platforms.yml.disabled` 中的 `setup-lazarus` 改成仓库内已有的 Windows 手工安装模式。
12. 复跑关键 workflow contracts，确认 release / signer / completeness 没有回退。
13. 检查同步模板仍保持一致。
14. 更新 working-memory，review，commit 并 push。
15. `download-artifact` 这波只做静态闭环；如未来要补 runtime 证据，单独 dispatch `wave-b-b2-manual.yml`。

## Commands

```bash
bash tests/scripts/test_workflow_checkout_node24_contract.sh
bash tests/scripts/test_workflow_upload_artifact_node24_contract.sh
bash tests/scripts/test_workflow_download_artifact_node24_contract.sh
bash tests/scripts/test_workflow_setup_python_node24_contract.sh
bash tests/scripts/test_workflow_cache_node24_contract.sh
bash tests/scripts/test_workflow_lazarus_setup_node24_contract.sh
bash tests/scripts/test_release_workflow_v1_5_0_contract.sh
bash tests/scripts/test_tls13_signer_gate_workflow_contract.sh
bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh
cmp -s .github/workflows/release.yml .github/workflows/release.yml.disabled
cmp -s .github/workflows/wave-b-b2-manual.yml .github/workflows/wave-b-b2-manual.yml.disabled
git diff --check
```

## Expected Outputs

- `.github/workflows` 下不再残留 `actions/checkout@v3/v4`、`actions/upload-artifact@v3/v4/v5`、`actions/download-artifact@v3/v4/v5/v6`、`actions/setup-python@v1-v5`、`actions/cache@v1-v4`，不再在 release workflow 里保留 `softprops/action-gh-release@v2`，也不再保留 `gcarreno/setup-lazarus`
- 活跃 workflow 和同步模板显式使用 `actions/checkout@v5`、`actions/upload-artifact@v6`、`actions/download-artifact@v7`、`softprops/action-gh-release@v3`、`actions/setup-python@v6`、`actions/cache@v5`
- release / signer / completeness 合同继续通过
- 不再把无关自动 workflow 的绿灯误判成 `download-artifact` 的 runtime 证明
