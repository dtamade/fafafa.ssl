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
- [in_progress] 准备第八次 commit/push，收口 `download-artifact` hygiene 第三波

## Current Blocker

- 当前没有新的本地语法/contract blocker。
- 运行时主阻塞已经解除，`download-artifact` 残留也已静态清理完毕。
- 由于本次改动命中的 action 只存在于 `wave-b-b2-manual.yml`（`workflow_dispatch`）与多个 `.disabled` 模板，`CI` / `TLS13 Signer Gate` 这类自动 workflow 不能作为这一波的 runtime 复核手段。
- 当前剩余动作是更新 working-memory 并完成第八次 commit/push；`download-artifact` 的 runtime 证据继续保持 `static-only / deferred manual dispatch`。

## Current Queue

1. 更新 root working-memory 与 workflow hygiene plan doc，写入第三波真相、版本边界与验证边界。
2. 给出第八批简短 review 结论后 commit。
3. `git push origin master`。
4. 若未来要补 `download-artifact` runtime 证据，只能单独 dispatch `wave-b-b2-manual.yml`；不再误把 `CI` / `TLS13 Signer Gate` 当成验证代理。
5. 若后续再出现新的 GitHub Actions runtime annotation，再单独复核 `softprops/action-gh-release` 等剩余第三方 actions。

## Decision Locks

- 不创建 `v1.5.0` tag，不发 GitHub Release。
- Windows/WinSSL 继续保持 `static-only / deferred runtime proof`，不混入本批。
- 本批只修真实已复现的 CI/runtime blocker 与 workflow runtime hygiene，不扩展到新功能或重新开 PR 流。
- 发布主线仍以当前 `master` 为准。

## Stop Condition

- 根 working-memory 与新 plan doc 已同步当前真相
- runtime-fix contracts、workflow checkout contract、workflow upload-artifact contract、workflow download-artifact contract 继续通过
- 第八批 `download-artifact` hygiene commit / push 完成
- `.github/workflows` 下不再残留 pre-Node24-default 的 `actions/download-artifact`
- 不再把与改动无关的自动 workflow 绿灯误记为 `download-artifact` 的 runtime 证明
