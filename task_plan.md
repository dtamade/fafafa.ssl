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
- [in_progress] 准备第五次 commit/push，并验证远端 completeness job 的退出期异常是否消失

## Current Blocker

- 当前没有新的本地语法/contract blocker。
- 最新远端失败已收敛到 `25902932655` 的退出期 `EAccessViolation`，而不是测试主体逻辑或缺库问题。
- 本地 long-run gate 已继续通过，但远端 GitHub runner 上是否完全消除 shutdown-time 崩溃仍需第五次 push 复核。

## Current Queue

1. 更新 root working-memory 与 plan doc，写入 `25902932655` 的 shutdown-time `EAccessViolation` 真相与本地修复证据。
2. 给出第五批简短 review 结论后 commit。
3. `git push origin master`。
4. 盯住新的 `CI` run，确认 completeness job 不再在打印 `所有测试完成！` 后崩溃。
5. 若远端仍复现退出期异常，再回头深挖 `TMbedTLSLibrary.InitializeRNG/FinalizeRNG` 与裸内存上下文方案。

## Decision Locks

- 不创建 `v1.5.0` tag，不发 GitHub Release。
- Windows/WinSSL 继续保持 `static-only / deferred runtime proof`，不混入本批。
- 本批只修真实已复现的 CI/runtime blocker，不扩展到新功能或重新开 PR 流。
- 发布主线仍以当前 `master` 为准。

## Stop Condition

- 根 working-memory 与新 plan doc 已同步当前真相
- focused contract tests 与 shutdown contract 继续通过
- `python3 scripts/compile_all_modules.py` 与 `run_freepascal_tls13_completeness_gate.sh --fast-local` 通过
- 第五批 commit / push 完成
- 新远端 `CI` run 已创建并验证退出期 `EAccessViolation` 不再复现
