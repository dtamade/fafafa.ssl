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
- [in_progress] 准备第六次 commit/push，并验证 workflow 升级后远端 CI / signer 继续通过

## Current Blocker

- 当前没有新的本地语法/contract blocker。
- 运行时主阻塞已经解除，当前批次只剩 workflow hygiene 收口与远端复核。
- 需要确认升级到 `actions/checkout@v5` 后，活跃 workflow 继续绿且 Node20 弃用 annotation 不再出现。

## Current Queue

1. 更新 root working-memory 与新的 workflow hygiene plan doc，记录 checkout Node24 合同与升级理由。
2. 给出第六批简短 review 结论后 commit。
3. `git push origin master`。
4. 盯住新的 `CI` / `TLS13 Signer Gate` runs，确认 workflow 升级后继续成功。
5. 若仍出现新的 GitHub Actions runtime annotation，再单独复核其他第三方 actions。

## Decision Locks

- 不创建 `v1.5.0` tag，不发 GitHub Release。
- Windows/WinSSL 继续保持 `static-only / deferred runtime proof`，不混入本批。
- 本批只修真实已复现的 CI/runtime blocker，不扩展到新功能或重新开 PR 流。
- 发布主线仍以当前 `master` 为准。

## Stop Condition

- 根 working-memory 与新 plan doc 已同步当前真相
- runtime-fix contracts 与 workflow checkout contract 继续通过
- 第五批 runtime 修复和第六批 workflow hygiene commit / push 完成
- 新远端 `CI` / `TLS13 Signer Gate` runs 继续通过
- checkout Node20 弃用告警不再由仓库内 `actions/checkout@v3/v4` 引起
