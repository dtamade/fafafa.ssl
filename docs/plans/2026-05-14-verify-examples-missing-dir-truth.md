# 2026-05-14 Verify Examples Missing Directory Truth

## Goal
收口 `scripts/verify_examples_compile.sh` 在 `examples/` 目录缺失时的假成功，避免 `find` 已经报错但脚本仍返回成功并产出 `total=0` 的 JSON。

## Architecture
- 当前脚本通过：
  - `mapfile -t EXAMPLE_FILES < <(find "$EXAMPLES_DIR" -name "*.pas" -type f | sort)`
  来预扫描 examples。
- 但脚本本身没有 `set -e`，所以当 `find` 因目录缺失失败时：
  - stderr 会出现 `No such file or directory`
  - `EXAMPLE_FILES` 仍然为空
  - `total/tested/passed/failed/skipped/remaining` 全变成 `0`
  - 最终仍可能 `exit 0`
- 这是一条很危险的 producer-side 假成功：
  - 证据链上游已经断了
  - artifact 却伪装成“0 个示例，且一切正常”

## Files
- `scripts/verify_examples_compile.sh`
- `tests/scripts/test_verify_examples_compile_missing_examples_dir_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，证明缺失 `examples/` 目录时脚本当前仍会成功返回并产出 `total=0` JSON。
2. 最小修改 `verify_examples_compile.sh`，在 `examples/` 缺失或不可扫描时直接 fail loud。
3. 复跑 focused 合同与 verify_examples 邻近合同。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_verify_examples_compile_missing_examples_dir_contract.sh
bash tests/scripts/test_verify_examples_compile_missing_examples_dir_contract.sh
bash -n scripts/verify_examples_compile.sh
bash tests/scripts/test_verify_examples_compile_invalid_format_contract.sh
bash tests/scripts/test_verify_examples_compile_pass_rate_without_bc_contract.sh
bash tests/scripts/test_verify_examples_compile_json_stdout_contract.sh
bash tests/scripts/test_verify_examples_compile_stop_on_error_summary_contract.sh
bash tests/scripts/test_verify_examples_compile_report_write_contract.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - stderr 有 `find: ... No such file or directory`
  - stdout JSON 仍然是 `total=0`
  - exit code 仍是 `0`
- 修复后：
  - 直接非零退出
  - stderr 清楚说明 `examples/` 目录缺失或不可扫描
  - 不再继续产出伪装成成功的 summary。
