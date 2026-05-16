# 2026-05-15 Workflow Node24 Hygiene

## Goal

清理仓库中 GitHub Actions workflow / template 里残留的 Node20-era JavaScript actions，并在此基础上继续做供应链硬化：先修 `actions/checkout`，再修 `actions/upload-artifact`，第三波修 `actions/download-artifact`，第四波修 `softprops/action-gh-release`、`actions/setup-python`、`actions/cache`，第五波去掉不可替代性不足的 `gcarreno/setup-lazarus`，第六波把所有外部 action 从浮动 major tag 收紧到 full commit SHA，并保证现有活跃 workflow 与同步模板不回退。

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
- 在版本线清理完成后，又暴露出下一层供应链风险：
  - 所有 action 仍是 `@v5` / `@v6` / `@v7` / `@v3` 这种浮动 major tag
  - 这需要进一步 pin 到 full commit SHA
- 在 action 版本与 SHA pinning 收口后，又暴露出下一层 workflow 权限风险：
  - 除 release 以外的 workflow 仍未显式声明 `permissions:`
  - 这会让它们继承仓库默认 `GITHUB_TOKEN` 权限，而不是把权限边界固定在仓库代码里
- 在 permissions 收口后，又暴露出下一层 checkout hygiene 与 dormant correctness 风险：
  - checkout step 仍在默认持久化凭据
  - `pr-checks.yml.disabled` 直接使用 `git diff HEAD~1 HEAD`，但没有保证 checkout 拿到父提交
- 在 checkout 与历史深度收口后，又暴露出下一层 mixed-trigger 上下文风险：
  - `pr-checks.yml.disabled` 同时支持 `pull_request` 与 `workflow_dispatch`
  - 但多个 shell 步骤默认自己一定拿到了 PR 上下文
- 在 mixed-trigger 上下文风险收口后，又暴露出下一层 dormant workflow truth 风险：
  - `performance.yml.disabled` 声称 `ubuntu-latest` / `windows-latest` / `macos-latest` 三平台 matrix
  - 但 benchmark project file 目标仍锁在 `linux`，run / report 步骤又直接写成 PowerShell 语法
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
- `tests/scripts/test_workflow_action_sha_pinning_contract.sh`
- `tests/scripts/test_workflow_permissions_contract.sh`
- `tests/scripts/test_workflow_checkout_credentials_contract.sh`
- `tests/scripts/test_workflow_pr_checks_history_contract.sh`
- `tests/scripts/test_workflow_pr_checks_dispatch_context_contract.sh`
- `tests/scripts/test_workflow_performance_linux_truth_contract.sh`
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
12. 收集当前 major tags 对应的真实 commit SHA，并新增 SHA pinning contract。
13. 将所有 workflow / template 的外部 action 收紧到 full commit SHA，并保留版本注释。
14. 复跑关键 workflow contracts，确认 release / signer / completeness 没有回退。
15. 检查同步模板仍保持一致。
16. 更新 working-memory，review，commit 并 push。
17. `download-artifact` 这波只做静态闭环；如未来要补 runtime 证据，单独 dispatch `wave-b-b2-manual.yml`。
18. 审查 workflow `permissions:` 显式声明情况，确认是否仍依赖仓库默认 token 权限。
19. 新增 permissions contract，并将非 release workflow / template 统一收紧到 `contents: read`。
20. 复跑本地 workflow contracts，并用自动 `CI` / `TLS13 Signer Gate` 远端 run 复核权限收紧没有误伤活跃链路。
21. 审查 checkout credential persistence，确认哪些 workflow 可以显式加 `persist-credentials: false`。
22. 新增 checkout credential contract，并把所有 checkout step 收紧到显式不保留凭据。
23. 审查 dormant `pr-checks` 模板的 checkout 历史深度与 `HEAD~1` 使用是否匹配。
24. 新增 PR history contract，并只给真正依赖父提交的 job 补 `fetch-depth: 2`。
25. 审查 mixed-trigger `pr-checks` 模板里 `workflow_dispatch` 与 PR-only 上下文的兼容性。
26. 新增 dispatch-context contract，并为手动触发路径补上显式 fallback 元数据。
27. 审查 dormant `performance` 模板的 runner 声明、toolchain、project target 与 shell 语义是否一致。
28. 新增 performance linux-truth contract，并把模板声明范围收紧到真实可支持的 Linux-only benchmark lane。

## Commands

```bash
bash tests/scripts/test_workflow_checkout_node24_contract.sh
bash tests/scripts/test_workflow_upload_artifact_node24_contract.sh
bash tests/scripts/test_workflow_download_artifact_node24_contract.sh
bash tests/scripts/test_workflow_setup_python_node24_contract.sh
bash tests/scripts/test_workflow_cache_node24_contract.sh
bash tests/scripts/test_workflow_lazarus_setup_node24_contract.sh
bash tests/scripts/test_workflow_action_sha_pinning_contract.sh
bash tests/scripts/test_workflow_permissions_contract.sh
bash tests/scripts/test_workflow_checkout_credentials_contract.sh
bash tests/scripts/test_workflow_pr_checks_history_contract.sh
bash tests/scripts/test_workflow_pr_checks_dispatch_context_contract.sh
bash tests/scripts/test_workflow_performance_linux_truth_contract.sh
bash tests/scripts/test_release_workflow_v1_5_0_contract.sh
bash tests/scripts/test_tls13_signer_gate_workflow_contract.sh
bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh
cmp -s .github/workflows/release.yml .github/workflows/release.yml.disabled
cmp -s .github/workflows/wave-b-b2-manual.yml .github/workflows/wave-b-b2-manual.yml.disabled
git diff --check
```

## Expected Outputs

- `.github/workflows` 下不再残留 `actions/checkout@v3/v4`、`actions/upload-artifact@v3/v4/v5`、`actions/download-artifact@v3/v4/v5/v6`、`actions/setup-python@v1-v5`、`actions/cache@v1-v4`，不再在 release workflow 里保留 `softprops/action-gh-release@v2`，也不再保留 `gcarreno/setup-lazarus`
- 活跃 workflow 和同步模板显式使用 SHA pinned 的 `actions/checkout`、`actions/upload-artifact`、`actions/download-artifact`、`softprops/action-gh-release`、`actions/setup-python`、`actions/cache`
- 非 release workflow / template 显式使用 `permissions: contents: read`，release 保持 `contents: write`
- 所有 checkout step 显式使用 `persist-credentials: false`
- `pr-checks.yml.disabled` 中真正依赖 `HEAD~1` 的 job 显式使用 `fetch-depth: 2`
- `pr-checks.yml.disabled` 中 mixed-trigger 手动路径不再直接依赖 PR-only 上下文
- `performance.yml.disabled` 不再硬写虚假的全平台 benchmark 覆盖面，改为 Linux-only truth
- release / signer / completeness 合同继续通过
- 不再把无关自动 workflow 的绿灯误判成 `download-artifact` 的 runtime 证明

## Closeout

- 第十一次批次已通过 `5a03f1c` 推送到 `master`
- 推送后的远端复核结果：
  - `TLS13 Signer Gate` run `25967316650` SUCCESS
  - `CI` run `25967316614` SUCCESS
  - `Code Quality (Light)` / `Minimal Gate (Linux)` / `FreePascal TLS 1.3 Completeness` 全部 SUCCESS
- 结论：
  - workflow action 家族已经同时满足 Node24 默认线与 full commit SHA pinning
  - 自动 Linux 主线未被这轮供应链收紧误伤
  - `wave-b-b2-manual.yml`、Windows/WinSSL、release 等未自动触发路径仍按用户约束保持 `static-only`
  - 后续又补上了显式 `permissions:` 收紧；`a24b983` 对应的 `TLS13 Signer Gate` run `25967632738` 与 `CI` run `25967632737` 继续 SUCCESS
  - 再后续又补上 checkout credential hardening 与 dormant `pr-checks` 历史深度修复；`6421420` 对应的 `TLS13 Signer Gate` run `25969736945`、`CI` run `25969736933`，以及 `3d4c322` 对应的 `CI` run `25969897201` 继续 SUCCESS
  - 最新又补上 dormant `pr-checks` 的 dispatch-context fallback；`cbd86d0` 对应的 `CI` run `25970607766` 继续 SUCCESS
  - 随后 docs closeout `083c057` 也通过了 `CI` run `25970738320`，说明 working-memory truth sync 没有带偏自动主线
  - 最新又补上 dormant `performance` workflow 的 Linux-only truth 收紧；`1d4f346` 对应的 `CI` run `25970919173` 继续 SUCCESS
