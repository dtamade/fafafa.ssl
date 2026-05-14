# 2026-05-15 FreePascal TLS 1.3 Completeness Gate Shell Hardening

## Goal
收口 `scripts/run_freepascal_tls13_completeness_gate.sh` 的 `bash -c` / 字符串执行风险，避免当前 focused gate 继续把 `run-id` 等动态值暴露给 shell 解释层。

## Architecture
- 当前 focused gate 仍通过：
  - `run_cmd() -> bash -c "$cmd"`
  - 每个 test lane 都把 `RUN_ID` / `WORK_ROOT` / `FPC_EXE` / `TEST_FILE` 拼进字符串命令
- 这意味着即使 `REPORTS_DIR` 已经有限制，当前 pure Pascal 主线的核心 gate 仍会在真正执行前重新进入 shell 解释层。
- 这批最小修法继续保持一致：
  - display 命令保留
  - 真正执行切成 direct argv
  - `mkdir -p` 前置到 shell 外，不再依赖 `&&` 串联

## Files
- `scripts/run_freepascal_tls13_completeness_gate.sh`
- `tests/scripts/test_freepascal_tls13_completeness_gate_run_id_injection_contract.sh`
- `tests/scripts/test_freepascal_tls13_completeness_gate_no_shell_execution_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，证明 `run-id` payload 当前可从 focused gate 的 `bash -c` 命令串逃逸。
2. 写 focused contract，明确脚本不得再依赖 `bash -c` / `eval` 执行模型。
3. 最小修改 gate，把 test lane 执行切到 direct argv，并保留 dry-run/operator-facing 文本。
4. 复跑新合同、既有 `test_freepascal_tls13_completeness_gate_contract.sh` 和脚本语法检查。
5. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_freepascal_tls13_completeness_gate_run_id_injection_contract.sh
bash tests/scripts/test_freepascal_tls13_completeness_gate_run_id_injection_contract.sh
bash tests/scripts/test_freepascal_tls13_completeness_gate_no_shell_execution_contract.sh
bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh
bash -n scripts/run_freepascal_tls13_completeness_gate.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - `run-id` payload 可作为 shell 语法执行
  - 脚本仍显式依赖 `bash -c`
- 修复后：
  - `run-id` 只作为 argv 数据进入 tmp 路径和 fake fpc 参数
  - `bash -c / eval` 不再参与执行
  - focused gate 现有 dry-run、PATH 解析、summary 契约保持 green
