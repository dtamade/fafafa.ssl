# 2026-05-14 Verify Examples Report Write Truth

## Goal
收口 `scripts/verify_examples_compile.sh -o FILE` 的报告落盘真值，避免报告写入失败时脚本仍然回显“报告已保存到”并返回成功。

## Architecture
- `verify_examples_compile.sh` 当前在 `REPORT_FILE` 非空时会执行：
  - `output_summary > "$REPORT_FILE"`
  - 然后无条件 `echo "报告已保存到: ..."`
- 脚本本身没有 `set -e`，所以当 shell 重定向失败时：
  - stderr 会出现 `No such file or directory`
  - 但脚本仍继续执行
  - 最终退出码仍只由 `FAILED > 0` 决定
- 这会制造一个典型 producer-side 假成功：
  - 编译本身全绿
  - 报告文件其实没写出来
  - CLI 却继续宣称已经保存成功

## Files
- `scripts/verify_examples_compile.sh`
- `tests/scripts/test_verify_examples_compile_report_write_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，用 fake `fpc` + 不存在的报告目录复现“回显成功但没写出来”的假成功。
2. 最小修改 `verify_examples_compile.sh`，在报告写入失败时明确返回非零并停止。
3. 复跑 focused 合同与已有 verify_examples 邻近合同。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_verify_examples_compile_report_write_contract.sh
bash tests/scripts/test_verify_examples_compile_report_write_contract.sh
bash -n scripts/verify_examples_compile.sh
bash tests/scripts/test_verify_examples_compile_json_stdout_contract.sh
bash tests/scripts/test_verify_examples_compile_stop_on_error_summary_contract.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - stderr: `No such file or directory`
  - stdout: `报告已保存到: ...`
  - exit code: `0`
- 修复后：
  - exit code: 非 0
  - stdout 不再谎报保存成功
  - stderr 清楚说明报告文件写入失败。
