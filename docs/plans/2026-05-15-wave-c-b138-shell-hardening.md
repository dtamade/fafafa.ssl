# 2026-05-15 Wave C B138 Full Gate Shell Hardening

## Goal
收口 `scripts/run_wave_c_pre_ci_reenable_full_gate.sh` 的 `eval` / 字符串执行风险，避免 `run-id` 被当成 shell 语法执行。

## Architecture
- 当前 B138 full gate 仍通过：
  - `run_step() -> eval "$cmd"`
  - `oncall/snapshot/packet` 三个 step 都把 `RUN_ID` 拼进字符串命令
- 这意味着即使下游 `B129` 已经安全，B138 仍会在自己的 orchestration 层重新引入 shell 执行面。
- 这批最小修法继续保持同一方向：
  - display 命令保留
  - 真正执行切成 direct argv

## Files
- `scripts/run_wave_c_pre_ci_reenable_full_gate.sh`
- `tests/scripts/test_run_wave_c_b138_full_gate_run_id_injection_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，证明 `--run-id` payload 当前可从 `eval` 命令串逃逸。
2. 最小修改 B138 full gate，把 B129/B132/B137 step 切到 argv 执行。
3. 复跑新合同、`test_wave_c_b138_tmp_structure_contract.sh` 和脚本语法检查。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_run_wave_c_b138_full_gate_run_id_injection_contract.sh
bash tests/scripts/test_run_wave_c_b138_full_gate_run_id_injection_contract.sh
bash tests/scripts/test_wave_c_b138_tmp_structure_contract.sh
bash -n scripts/run_wave_c_pre_ci_reenable_full_gate.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - `run-id` payload 可作为 shell 语法执行
  - fake nested B129/B132/B137 runner 不会稳定收到完整原始值
- 修复后：
  - `run-id` 只作为 argv 数据透传
  - B138 仍生成 PASS full-gate report
  - 既有 tmp structure 契约保持 green
