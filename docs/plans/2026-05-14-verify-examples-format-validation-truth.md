# 2026-05-14 Verify Examples Format Validation Truth

## Goal
收口 `scripts/verify_examples_compile.sh -f/--format` 的格式校验真值，避免未知格式被静默降级成 text 输出。

## Architecture
- 帮助文本当前明确写了只支持三种输出格式：
  - `text`
  - `json`
  - `markdown`
- 但实际实现里 `output_summary()` 用 `case` 的默认分支兜底成 text。
- 这意味着：
  - `-f yaml`
  - `-f whatever`
  - 甚至参数拼错
  都会静默输出 text 并返回成功。
- 这是一条典型的 CLI 契约漂移：
  - 帮助面和真实行为不一致
  - 调用者不会得到明确错误
  - 自动化脚本可能以为拿到了请求格式

## Files
- `scripts/verify_examples_compile.sh`
- `tests/scripts/test_verify_examples_compile_invalid_format_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，证明 `-f yaml` 现在仍静默输出 text 并返回成功。
2. 最小修改参数校验逻辑，对未知格式直接报错退出。
3. 复跑 focused 合同与 verify_examples 邻近合同。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_verify_examples_compile_invalid_format_contract.sh
bash tests/scripts/test_verify_examples_compile_invalid_format_contract.sh
bash -n scripts/verify_examples_compile.sh
bash tests/scripts/test_verify_examples_compile_pass_rate_without_bc_contract.sh
bash tests/scripts/test_verify_examples_compile_json_stdout_contract.sh
bash tests/scripts/test_verify_examples_compile_stop_on_error_summary_contract.sh
bash tests/scripts/test_verify_examples_compile_report_write_contract.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - `-f yaml` 返回 `0`
  - stdout 是 text 摘要
- 修复后：
  - `-f yaml` 直接非零退出
  - stderr 明确说明格式非法。
