# 2026-05-15 CI Runtime Gate Repair

## Goal

修复 GitHub Actions 恢复执行后暴露出的真实 CI/runtime 阻塞，并把验证链路收口到可重复的本地 contracts + 远端 rerun。

## Architecture

- `CI -> FreePascal TLS 1.3 Completeness`
  - 实际会跑到 WolfSSL runtime 对齐测试
  - 所以 workflow 不能只装 `fpc + libssl-dev + python3`
  - 必须同时装 `libwolfssl-dev`

- `Release v1.5.0`
  - 同样调用 `run_freepascal_tls13_completeness_gate.sh`
  - 必须与 active CI 维持同一套 runtime 依赖，不然 release lane 会复现 completeness 红灯

- `TLS13 Signer Gate`
  - 远端失败分两层：
    - bundle 内层 bench 红灯
    - workflow summary shell 语法损坏
  - bench 红灯不是 signer 算法逻辑回归，而是 bench 编译脚本：
    - `-Criot` 触发 compile-time range-check error
    - 编译输出被吞掉，难以定位

## Files

- `.github/workflows/ci.yml`
- `.github/workflows/release.yml`
- `.github/workflows/release.yml.disabled`
- `.github/workflows/tls13-signer-gate.yml`
- `scripts/run_freepascal_tls13_servercertverify_bench.sh`
- `src/fafafa.ssl.wolfssl.api.pas`
- `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
- `tests/scripts/test_release_workflow_v1_5_0_contract.sh`
- `tests/scripts/test_tls13_signer_gate_workflow_contract.sh`
- `tests/scripts/test_tls13_servercertverify_bench_contract.sh`
- `tests/scripts/test_wolfssl_loader_fallback_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 用当前远端 run 真相补 focused contracts，让问题先红。
2. 最小修补 workflow 依赖、signer summary here-doc、bench compile flags / diagnostics。
3. 首次 push 后继续读新远端 run，识别二阶真实失败。
4. 若仍有 blocker：
   - 提升 signer summary contract 到“真实执行 fake payload”
   - 给 WolfSSL loader 加 Linux fallback 路径 / versioned soname 扫描
5. 若第二次 push 后 completeness 仍红：
   - 先重新核对当前 `ci.yml` 的 job-local install step，而不是直接假设 loader fallback 失败
   - 确保 contract 断言落在 `freepascal-tls13-completeness` job 自己的 install step，而不是整份 workflow 粗粒度 grep
6. 若第三次 push 后 completeness 继续前移到新的 backend：
   - 把新的缺库错误当作真实 blocker 继续收敛，不回退到已经转绿的旧问题
   - 同步检查 `release.yml` / `release.yml.disabled` 是否缺同一运行时依赖
   - 对重复结构的 workflow step，优先用 job-local contract 防止补丁误命中相邻 job
7. 更新 working-memory，做简短 review，commit 并 push。
8. 观察新的 `CI` / `TLS13 Signer Gate` 远端 run，确认四阶 blocker 已消除。

## Commands

```bash
bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh
bash tests/scripts/test_release_workflow_v1_5_0_contract.sh
bash tests/scripts/test_tls13_signer_gate_workflow_contract.sh
bash tests/scripts/test_tls13_servercertverify_bench_contract.sh
bash tests/scripts/test_wolfssl_loader_fallback_contract.sh
bash scripts/run_freepascal_tls13_servercertverify_bench.sh
bash scripts/run_tls13_signer_gate_ci.sh
bash scripts/run_tls13_signer_gate_bundle.sh --run-id local_bundle_repair_20260515 --reports-dir test-reports --strict
git diff --check
```

## Expected Outputs

- completeness / release workflow contracts 通过，并显式要求 `libwolfssl-dev`
- signer workflow contract 通过，提取出的 append-step-summary shell 可 `bash -n`
- signer workflow contract 通过，并能真实执行 fake status payload
- wolfssl loader fallback contract 通过，source 明确包含 Linux fallback 搜索逻辑
- bench contract 通过，并在 fake compiler 失败时能看到真实编译诊断
- 本地 signer gate CI 与 bundle `--strict` 都恢复 PASS
- 若第二次 push 后 completeness 仍红，contract 能进一步抓出 job-local 依赖缺口，而不是被其他 job 的安装行误导
- 若第三次 push 后 completeness 前移到 MbedTLS，contract 与 workflow 会同步补上 `libmbedtls-dev`
- 第四次 push 后新的远端 runs 在真实执行层面不再复现旧的 blocker
