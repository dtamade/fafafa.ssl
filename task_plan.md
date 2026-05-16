# Task Plan - CI Runtime Gate Repair

## Goal

修复 GitHub Actions 恢复执行后暴露出来的真实远端阻塞，并继续把 completeness lane 收口到最新的退出期崩溃：先解决 WolfSSL / MbedTLS runtime 依赖与 signer workflow 问题，再修复 `FreePascal TLS 1.3 Completeness` 在所有测试完成后触发的 shutdown-time `EAccessViolation`，随后完成本地验证、提交并推送到 `master` 复核远端状态。

## Current Status

- [completed] 复核远端失败证据（CI run `25893971783` / signer run `25901035350`）
- [completed] 把 3 个真实问题写成 focused contract tests，并先观测到红灯
- [completed] 修复 `.github/workflows/ci.yml` / `release.yml` / `release.yml.disabled` 的 WolfSSL 依赖缺口
- [completed] 修复 `.github/workflows/tls13-signer-gate.yml` 的 here-doc terminator 缩进
- [completed] 修复 `scripts/run_freepascal_tls13_servercertverify_bench.sh` 的 `-Criot` 与编译诊断吞没问题
- [completed] 本地 focused contracts、bench、`run_tls13_signer_gate_ci.sh`、bundle `--strict` 已通过
- [completed] 首次提交 `d3ebeee` 并推送到 `master`，创建远端 runs `25901775672` / `25901775676`
- [completed] 二次远端取证：
  - signer workflow 的 bundle 主步骤已经转绿
  - signer workflow 仍因 summary Python body 缩进触发 `IndentationError`
  - completeness workflow 仍然报 `Failed to load WolfSSL library: libwolfssl.so`
- [completed] 新增可执行 signer summary contract 与 WolfSSL loader fallback contract
- [completed] 修复 `.github/workflows/tls13-signer-gate.yml` 的 Python heredoc 正文缩进
- [completed] 修复 `src/fafafa.ssl.wolfssl.api.pas`，让 loader 在 Linux 上回退扫描常见库路径与版本化 `libwolfssl.so*`
- [completed] 复核第二次 push 远端 runs：signer run `25902255923` 已全绿，但 CI run `25902255941` 仍在 completeness job 报 `Failed to load WolfSSL library: libwolfssl.so`
- [completed] 发现 `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh` 的盲区：它只对整份 `ci.yml` 做 `libwolfssl-dev` 粗粒度 grep，误把 `Minimal Gate (Linux)` 的安装步骤当成了 completeness job 的保障
- [completed] 把 completeness contract 升级为 job-local install-step 断言，并先在当前 `ci.yml` 上观测到红灯
- [completed] 修复 `.github/workflows/ci.yml` 的 `freepascal-tls13-completeness` job 安装步骤，补回 `libwolfssl-dev`
- [completed] 第三次提交 `8d052dd` 推送后，远端 CI run `25902644127` 继续收敛：
  - `Minimal Gate (Linux)` PASS
  - `Code Quality (Light)` PASS
  - `FreePascal TLS 1.3 Completeness` 失败点已经从 WolfSSL 前移到 `MbedTLS KnownIssues 运行时对齐测试`
- [completed] 把 completeness/release contracts 升级到显式要求 `libmbedtls-dev`，并先观测到红灯
- [completed] 修复 `.github/workflows/ci.yml` 的 completeness job 与 `release.yml` / `release.yml.disabled` 的安装步骤，补上 `libmbedtls-dev`
- [completed] 第四次提交 `30467e4` 推送后，远端 CI run `25902932655` 继续收敛：
  - `FreePascal KnownIssues` PASS
  - `WolfSSL KnownIssues` PASS
  - `MbedTLS KnownIssues` PASS
  - 在打印 `所有测试完成！` 后进程退出期抛出两次 `EAccessViolation`
- [completed] 新增 shutdown-safe focused contract，并先在当前源码上观测到红灯
- [completed] 在 `TSSLFactory` / `TMbedTLSLibrary` / `TWolfSSLLibrary` 上实现 process-shutdown 安全注销路径
- [completed] 本地 shutdown contract、compile-all 与 `run_freepascal_tls13_completeness_gate.sh --fast-local` 继续通过
- [completed] 第五次提交 `45dabb4` 推送后，远端 CI run `25903921296` SUCCESS：
  - `FreePascal TLS 1.3 Completeness` SUCCESS
  - `Minimal Gate (Linux)` SUCCESS
  - 退出期 `EAccessViolation` 未再复现
- [completed] 新增 workflow checkout Node24 contract，并先在当前模板上观测到红灯
- [completed] 仓库内全部 workflow / workflow template 的 `actions/checkout@v3/v4` 已升级到 `actions/checkout@v5`
- [completed] 第六次提交 `d56637f` 推送后，远端 runs 继续通过：
  - `TLS13 Signer Gate` run `25904745243` SUCCESS
  - `CI` run `25904745247` SUCCESS
  - `actions/checkout@v4` 相关 Node20 annotation 已消失
- [completed] 新增 workflow upload-artifact Node24 contract，并先在当前模板上观测到红灯
- [completed] 仓库内全部 workflow / workflow template 的 `actions/upload-artifact@v4` 已升级到 `actions/upload-artifact@v6`
- [completed] 第七次提交 `863dca2` 已完成，workflow artifact hygiene 第二波已经收口到 `master`
- [completed] 重新审查 workflow hygiene 路线后确认旧计划存在版本真相偏差：
  - 截至 `2026-05-16`，官方 first-party actions 的 Node24 默认线分别是 `actions/checkout@v5`、`actions/upload-artifact@v6`、`actions/download-artifact@v7`
  - 旧的 “download-artifact 升到 `v5`” 目标已经过时，不能继续执行
- [completed] 新增 workflow download-artifact Node24 contract，并先在当前模板上观测到红灯
- [completed] 仓库内全部 workflow / workflow template 的 `actions/download-artifact@v4` 已升级到 `actions/download-artifact@v7`
- [completed] workflow hygiene 第三波本地复核通过：
  - `download-artifact` / `upload-artifact` / `checkout` contracts PASS
  - `release` / `signer` / `completeness` contracts PASS
  - `wave-b-b2-manual.yml` 与 `wave-b-b2-manual.yml.disabled` 继续保持同步
- [completed] 第八次提交 `5c200bf` 已完成，`download-artifact` hygiene 第三波已经推送到 `master`
- [completed] 远端自动 `CI` run `25952317087` SUCCESS：
  - `Code Quality (Light)` SUCCESS
  - `Minimal Gate (Linux)` SUCCESS
  - `FreePascal TLS 1.3 Completeness` SUCCESS
  - 这只证明自动主线未被误伤，不作为 `wave-b-b2-manual.yml` 中 `download-artifact` 路径的 runtime 证明
- [completed] 重新核对剩余 action 家族后确认当前真正仍有升级路径的 Node20 风险点：
  - `softprops/action-gh-release@v2`（活跃 `release.yml` / `release.yml.disabled`）
  - `actions/setup-python@v5`（`code-quality.yml.disabled`）
  - `actions/cache@v4`（`test-all-platforms.yml.disabled` / `winssl-tests.yml.disabled`）
- [completed] 新增/强化本地合同并先观测到红灯：
  - `tests/scripts/test_release_workflow_v1_5_0_contract.sh` 现在显式要求 `softprops/action-gh-release@v3`
  - 新增 `tests/scripts/test_workflow_setup_python_node24_contract.sh`
  - 新增 `tests/scripts/test_workflow_cache_node24_contract.sh`
- [completed] 仓库内全部可升级的剩余 action 已收口到 Node24 基线：
  - `softprops/action-gh-release@v3`
  - `actions/setup-python@v6`
  - `actions/cache@v5`
- [completed] 第九波 workflow hygiene 本地复核通过：
  - release / setup-python / cache contracts PASS
  - checkout / upload-artifact / download-artifact contracts PASS
  - signer / completeness contracts PASS
  - `release.yml` 与 `release.yml.disabled` 继续保持同步
- [completed] 第九次提交 `7485034` 已完成，release/tooling/cache Node24 hygiene 第四波已经推送到 `master`
- [completed] 远端自动 `CI` run `25962132252` SUCCESS：
  - `Code Quality (Light)` SUCCESS
  - `Minimal Gate (Linux)` SUCCESS
  - `FreePascal TLS 1.3 Completeness` SUCCESS
  - 这证明第四波 hygiene 没有误伤自动主线，但不代表 dormant Windows workflows 已被远端实际执行
- [completed] 重新审查 `gcarreno/setup-lazarus@v3.4.1` 后确认存在可行静态替代路径：
  - `test-all-platforms.yml.disabled` 不依赖该 action 的独有功能
  - 仓库内 `wave-b-b2-manual.yml` 已经有可复用的 Windows 手工安装 FPC/Lazarus 模式
- [completed] 新增 workflow Lazarus setup Node24 contract，并先在当前模板上观测到红灯
- [completed] `test-all-platforms.yml.disabled` 已移除 `gcarreno/setup-lazarus@v3`，改为手工安装 `freepascal` / `lazarus` 并显式校验 `fpc` / `lazbuild` / `lazarus`
- [completed] 第十波 workflow hygiene 本地复核通过：
  - `lazarus setup` / `cache` / `setup-python` / `release` contracts PASS
  - 现有 checkout / upload-artifact / download-artifact / signer / completeness contracts继续 PASS
- [in_progress] 准备第十次 commit/push，收口最后一条 Node20 action 替代

## Current Blocker

- 当前没有新的本地语法/contract blocker。
- 运行时主阻塞已经解除。
- 当前没有已知仍停留在 Node20 默认线且可在仓库内继续直接清理的 GitHub Action 残留。
- 当前剩余边界只在验证层：
  - `release.yml`、`code-quality.yml.disabled`、`test-all-platforms.yml.disabled`、`winssl-tests.yml.disabled` 这几条被改到的路径没有在本轮远端自动 push run 中被实际执行
  - 其中 Windows / dormant 路径继续保持 `static-only`，符合用户当前约束

## Current Queue

1. 更新 root working-memory 与 workflow hygiene plan doc，写入第五波真相并撤销 `setup-lazarus` 阻塞结论。
2. 给出第十批简短 review 结论后 commit。
3. `git push origin master`。
4. 轻量复核 push 后的自动远端 run，确认自动主线未被第五波 hygiene 误伤。
5. 若未来用户要把 dormant Windows workflows 变回活跃路线，再单独补 Windows runtime 证据；当前继续保持静态审查边界。

## Decision Locks

- 不创建 `v1.5.0` tag，不发 GitHub Release。
- Windows/WinSSL 继续保持 `static-only / deferred runtime proof`，不混入本批。
- 本批只修真实已复现的 CI/runtime blocker 与 workflow runtime hygiene，不扩展到新功能或重新开 PR 流。
- 发布主线仍以当前 `master` 为准。

## Stop Condition

- 根 working-memory 与新 plan doc 已同步当前真相
- release / setup-python / cache contracts 与既有 workflow contracts 继续通过
- 第十批 Lazarus setup 替代 batch commit / push 完成
- `.github/workflows` 下不再残留 `gcarreno/setup-lazarus`
- `.github/workflows` 下不再残留可直接升级但仍停在 Node20 默认线的 GitHub Action 引用
