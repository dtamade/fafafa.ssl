# 2026-05-15 Wave C Quick Sprint Bundle Shell Hardening

## Goal
收口 `scripts/run_wave_c_quick_sprint_bundle.sh` 的 `eval` / 字符串执行风险，避免 `run-id` 等动态值被当成 shell 语法执行。

## Architecture
- 当前 quick sprint bundle 仍通过：
  - `run_step() -> eval "$cmd"`
  - `B107/B108/B109/B110` 四个 step 都把 `RUN_ID`、目录和报告路径拼进字符串命令
  - B107 还额外通过 `$( [[ ... ]] && printf ... )` 把 `--require-full-gate` 注入命令串
- 这意味着即使下游 `threshold/readiness/canary/rollback` 子脚本已经足够稳定，bundle 仍会在最外层重新引入 shell 执行面。
- 这批最小修法继续保持一致：
  - display 命令保留
  - 真正执行切成 direct argv
  - 可选开关通过条件追加 argv，而不是内联 shell 条件

## Files
- `scripts/run_wave_c_quick_sprint_bundle.sh`
- `tests/scripts/test_run_wave_c_quick_sprint_bundle_run_id_injection_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，证明 `--run-id` payload 当前可从 `eval` 命令串逃逸，并锁住 `--require-full-gate` 透传语义。
2. 最小修改 quick sprint bundle，把 B107/B108/B109/B110 step 切到 argv 执行。
3. 用条件 argv 追加替换 B107 step 内联的 `$( [[ ... ]] && printf ... )` shell 拼接。
4. 复跑新合同、`test_run_wave_c_quick_sprint_bundle_unified_inputs_contract.sh`、`test_check_wave_c_post_trigger_observability_tmp_reports_contract.sh` 和脚本语法检查。
5. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_run_wave_c_quick_sprint_bundle_run_id_injection_contract.sh
bash tests/scripts/test_run_wave_c_quick_sprint_bundle_run_id_injection_contract.sh
bash tests/scripts/test_run_wave_c_quick_sprint_bundle_unified_inputs_contract.sh
bash tests/scripts/test_check_wave_c_post_trigger_observability_tmp_reports_contract.sh
bash -n scripts/run_wave_c_quick_sprint_bundle.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - `run-id` payload 可作为 shell 语法执行
  - fake nested B107/B108/B109/B110 runner 不能稳定收到完整原始值
- 修复后：
  - `run-id` 只作为 argv 数据透传
  - `--require-full-gate` 仍会明确透传给 B107
  - quick sprint bundle 仍生成 PASS 汇总报告
  - 邻近 B120 tmp artifact contract 保持 green
