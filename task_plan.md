# Task Plan - CI Runtime Gate Repair

## Goal

修复 GitHub Actions 恢复执行后暴露出来的真实远端阻塞：FreePascal completeness 缺 WolfSSL runtime 依赖、TLS13 signer workflow 的 here-doc summary 语法损坏，以及 signer bench 编译旗标/诊断可见性问题；随后完成本地验证、提交并推送到 `master` 复核远端状态。

## Current Status

- [completed] 复核远端失败证据（CI run `25893971783` / signer run `25901035350`）
- [completed] 把 3 个真实问题写成 focused contract tests，并先观测到红灯
- [completed] 修复 `.github/workflows/ci.yml` / `release.yml` / `release.yml.disabled` 的 WolfSSL 依赖缺口
- [completed] 修复 `.github/workflows/tls13-signer-gate.yml` 的 here-doc terminator 缩进
- [completed] 修复 `scripts/run_freepascal_tls13_servercertverify_bench.sh` 的 `-Criot` 与编译诊断吞没问题
- [completed] 本地 focused contracts、bench、`run_tls13_signer_gate_ci.sh`、bundle `--strict` 已通过
- [in_progress] 更新 working-memory、清理临时产物、提交并推送，然后观察新的远端 runs

## Current Blocker

- 当前没有本地代码 blocker。
- 剩余风险只在远端重新执行是否与本地验证一致，需要 push 后核对 GitHub Actions 新 run。

## Current Queue

1. 写入 `docs/plans/2026-05-15-ci-runtime-gate-repair.md` 与根 working-memory。
2. 清理本批生成的临时 `test-reports` 产物，保持提交面干净。
3. 复跑 `git diff --check` 与 focused contracts（如文档更新影响）。
4. 给出简短 review 结论后 commit。
5. `git push origin master`，确认新的 `CI` 与 `TLS13 Signer Gate` run 已创建并核对首轮结果。

## Decision Locks

- 不创建 `v1.5.0` tag，不发 GitHub Release。
- Windows/WinSSL 继续保持 `static-only / deferred runtime proof`，不混入本批。
- 本批只修真实已复现的 CI/runtime blocker，不扩展到新功能或重新开 PR 流。
- 发布主线仍以当前 `master` 为准。

## Stop Condition

- 根 working-memory 与新 plan doc 已同步当前真相
- focused contract tests 继续通过
- `run_tls13_signer_gate_ci.sh` 与 `run_tls13_signer_gate_bundle.sh --strict` 通过
- commit / push 完成
- 新远端 workflow run 已创建并完成首轮状态核对
