# 2026-05-15 Wave C B149 Submission Bundle Shell Hardening

## Goal
收口 `scripts/run_wave_c_ci_reenable_submission_bundle.sh` 的 `eval` / 字符串执行风险，避免 `run-id` 被当成 shell 语法执行。

## Architecture
- 当前 B149 submission bundle 仍通过：
  - `run_step() -> eval "$cmd"`
  - `B146/B147/B148` 三个 step 都把 `RUN_ID` 和输入路径拼进字符串命令
- 这意味着即使下游 `prepare/check/brief` 脚本已经足够稳定，B149 仍会在最外层重新引入 shell 执行面。
- 这批最小修法继续保持一致：
  - display 命令保留
  - 真正执行切成 direct argv

## Files
- `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
- `tests/scripts/test_run_wave_c_b149_submission_bundle_run_id_injection_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，证明 `--run-id` payload 当前可从 `eval` 命令串逃逸。
2. 最小修改 B149 submission bundle，把 B146/B147/B148 step 切到 argv 执行。
3. 复跑新合同、`test_run_wave_c_ci_reenable_submission_bundle_unified_contract.sh` 和脚本语法检查。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_run_wave_c_b149_submission_bundle_run_id_injection_contract.sh
bash tests/scripts/test_run_wave_c_b149_submission_bundle_run_id_injection_contract.sh
bash tests/scripts/test_run_wave_c_ci_reenable_submission_bundle_unified_contract.sh
bash -n scripts/run_wave_c_ci_reenable_submission_bundle.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - `run-id` payload 可作为 shell 语法执行
  - fake nested B146/B147/B148 runner 不会稳定收到完整原始值
- 修复后：
  - `run-id` 只作为 argv 数据透传
  - B149 仍生成 PASS submission bundle report
  - 既有 unified contract 保持 green
