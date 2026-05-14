# 2026-05-14 Verify Examples Pass-Rate BC Independence

## Goal
收口 `scripts/verify_examples_compile.sh` 对 `bc` 的脆弱依赖，避免 `bc` 不可用时脚本继续返回成功、却产出坏 JSON。

## Architecture
- 当前 `verify_examples_compile.sh` 用下面这段计算通过率：
  - `PASS_RATE=$(echo "scale=1; $PASSED * 100 / $TESTED" | bc)`
- 脚本本身没有 `set -e`，所以当 `bc` 缺失或异常退出时：
  - stderr 会报错
  - `PASS_RATE` 为空
  - JSON 会生成成 `"pass_rate":` 空值
  - 只要 `FAILED=0`，脚本最终仍会 `exit 0`
- 这是一条直接的 producer-side 假成功链：
  - examples 编译表面通过
  - 但 artifact 已经不是合法 JSON
  - 调用者只有在后续 `json.load(...)` 时才会晚一点爆炸
- 这批最小正确修法不是给 `bc` 补检查，而是移除这个脆弱依赖：
  - 改用 shell 已有环境更稳定的计算方式产出一位小数 pass_rate

## Files
- `scripts/verify_examples_compile.sh`
- `tests/scripts/test_verify_examples_compile_pass_rate_without_bc_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，用 fake `bc` 复现“exit 0 + 坏 JSON”的当前行为。
2. 最小修改 `verify_examples_compile.sh`，移除 `bc` 依赖并保持 pass_rate 格式不变。
3. 复跑 focused 合同与 verify_examples 邻近合同。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_verify_examples_compile_pass_rate_without_bc_contract.sh
bash tests/scripts/test_verify_examples_compile_pass_rate_without_bc_contract.sh
bash -n scripts/verify_examples_compile.sh
bash tests/scripts/test_verify_examples_compile_json_stdout_contract.sh
bash tests/scripts/test_verify_examples_compile_stop_on_error_summary_contract.sh
bash tests/scripts/test_verify_examples_compile_report_write_contract.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - stderr 报 `bc` 错误
  - JSON 中 `pass_rate` 为空
  - exit code 仍是 `0`
- 修复后：
  - 不再依赖 `bc`
  - JSON 仍可解析
  - `pass_rate` 保持一位小数。
