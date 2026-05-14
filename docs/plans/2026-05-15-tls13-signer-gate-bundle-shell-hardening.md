# 2026-05-15 TLS13 Signer Gate Bundle Shell Hardening

## Goal
收口 `scripts/run_tls13_signer_gate_bundle.sh` 的 `eval` / 字符串执行风险，避免 `run-id` 等动态值被当成 shell 语法执行。

## Architecture
- 当前 bundle 脚本的 4 个 step 都通过：
  - `run_step() -> eval "$cmd"`
  - `ci` step 还把 `RUN_ID` / `REPORTS_DIR` 拼进 env 前缀字符串
- 这和刚修完的 Wave B/macOS 执行底座是同型问题，只是这里的边界更外层：
  - 内层 `scripts/run_tls13_signer_gate_ci.sh` 已经直接用 argv 调 `run_wave_b_ci_gate.sh`
  - 但 bundle 自己仍把 `RUN_ID` / `REPORTS_DIR` / `ARCHIVE_PROFILE` / `ARCHIVE_ROOT` 重新暴露给 shell 解释层
- 这批最小修法不需要改 bundle 语义，只需要：
  - 保留 operator-facing 命令文本
  - 真正执行改成 argv / `env "KEY=value"` 数据传递

## Files
- `scripts/run_tls13_signer_gate_bundle.sh`
- `tests/scripts/test_tls13_signer_gate_bundle_run_id_injection_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，证明 bundle 当前仍会执行来自 `--run-id` 的 shell payload。
2. 最小修改 bundle，把 `ci/snapshot/status/archive` step 全部切到 argv 执行。
3. 复跑新合同和脚本语法检查。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_tls13_signer_gate_bundle_run_id_injection_contract.sh
bash tests/scripts/test_tls13_signer_gate_bundle_run_id_injection_contract.sh
bash -n scripts/run_tls13_signer_gate_bundle.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - `--run-id` payload 可从 `eval` 命令串逃逸
  - fake nested CI runner 可能拿不到完整原始值
- 修复后：
  - `run-id` 只作为 env / argv 数据透传
  - bundle 仍能产出 PASS 汇总
  - fake nested CI runner 观察到完整 `run-id` 原始值
