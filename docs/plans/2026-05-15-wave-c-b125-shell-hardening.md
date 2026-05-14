# 2026-05-15 Wave C B125 Local-First Guard Bundle Shell Hardening

## Goal
收口 `scripts/run_wave_c_local_first_guard_bundle.sh` 的 `eval` / 字符串执行风险，避免 `run-id` 被当成 shell 语法执行。

## Architecture
- 当前 B125 bundle 仍通过：
  - `run_step() -> eval "$cmd"`
  - `continuity_exit` / `drift_exit` 都把 `RUN_ID` 拼进字符串命令
- 这条风险不只是脚本单体问题：
  - B125 是 `Wave C local guard` 链条里的底层 bundle
  - 上层 `run_wave_c_local_guard_oncall_check.sh` 等脚本会消费它的产物
- 这批最小修法与前几轮的 shell hardening 一致：
  - operator-facing 命令文本继续保留
  - 真正执行改成 direct argv

## Files
- `scripts/run_wave_c_local_first_guard_bundle.sh`
- `tests/scripts/test_run_wave_c_local_first_guard_bundle_run_id_injection_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，证明 `--run-id` payload 当前可从 `eval` 命令串逃逸。
2. 最小修改 B125 bundle，把 `B123/B124` step 切到 argv 执行。
3. 复跑新合同、既有 tmp-default 合同和脚本语法检查。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_run_wave_c_local_first_guard_bundle_run_id_injection_contract.sh
bash tests/scripts/test_run_wave_c_local_first_guard_bundle_run_id_injection_contract.sh
bash tests/scripts/test_run_wave_c_local_first_guard_bundle_tmp_default_contract.sh
bash -n scripts/run_wave_c_local_first_guard_bundle.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - `run-id` payload 可作为 shell 语法执行
  - fake nested B123/B124 runner 不会稳定收到完整原始值
- 修复后：
  - `run-id` 只作为 argv 数据透传
  - B125 仍生成 PASS bundle summary
  - 既有 tmp 默认目录契约保持 green
