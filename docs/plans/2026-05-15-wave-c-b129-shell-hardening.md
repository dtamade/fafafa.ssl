# 2026-05-15 Wave C B129 Oncall Shell Hardening

## Goal
收口 `scripts/run_wave_c_local_guard_oncall_check.sh` 的 `eval` / 字符串执行风险，避免 `run-id` 被当成 shell 语法执行。

## Architecture
- 当前 B129 oncall 仍通过：
  - `run_step() -> eval "$cmd"`
  - `bundle_exit` / `history_exit` 都把 `RUN_ID` 拼进字符串命令
- 这意味着即使 B125 已经安全，B129 仍会在更外层重新引入同型执行面：
  - `RUN_ID`
  - 派生的 `bundle_report/history_report`
- 这批最小修法继续保持同一方向：
  - display 命令保留
  - 真正执行切成 direct argv

## Files
- `scripts/run_wave_c_local_guard_oncall_check.sh`
- `tests/scripts/test_run_wave_c_local_guard_oncall_check_run_id_injection_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，证明 `--run-id` payload 当前可从 `eval` 命令串逃逸。
2. 最小修改 B129 oncall，把 B125/B126 step 切到 argv 执行。
3. 复跑新合同、`test_export_wave_c_local_guard_status_json_tmp_lookup_contract.sh` 和脚本语法检查。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_run_wave_c_local_guard_oncall_check_run_id_injection_contract.sh
bash tests/scripts/test_run_wave_c_local_guard_oncall_check_run_id_injection_contract.sh
bash tests/scripts/test_export_wave_c_local_guard_status_json_tmp_lookup_contract.sh
bash -n scripts/run_wave_c_local_guard_oncall_check.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - `run-id` payload 可作为 shell 语法执行
  - fake nested B125/B126 runner 不会稳定收到完整原始值
- 修复后：
  - `run-id` 只作为 argv 数据透传
  - B129 仍生成 PASS oncall report
  - 下游 B142 tmp lookup 契约保持 green
