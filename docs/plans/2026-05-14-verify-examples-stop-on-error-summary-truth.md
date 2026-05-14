# 2026-05-14 Verify Examples Stop-On-Error Summary Truth

## Goal
收口 `scripts/verify_examples_compile.sh --stop-on-error` 的 summary 真值，避免脚本在首个编译失败后提前停止时，仍产出一份容易被误读成“全量 examples 统计”的半程报告。

## Architecture
- `verify_examples_compile.sh` 的 `-s/--stop-on-error` 会在首个失败后 `break`。
- 修复前 summary 仍沿用“当前已走到的循环计数”：
  - `summary.total` 实际是“已遇到的文件数”
  - `pass_rate` 分母也是这段半程结果
  - 报告里没有 `tested` / `remaining` / `stopped_early`
- 这会制造 producer-side 真值漂移：
  - 用户只看到 `total/passed/failed/skipped/pass_rate`
  - 很容易把它当成整个 `examples/` 目录的全量统计
  - 实际上后面还有未处理文件
- 这批要把“提前终止”显式编码进 summary：
  - `total` = 全量 examples 数
  - `tested` = 实际尝试编译的数量
  - `remaining` = 未处理数量
  - `stopped_early` = 是否因 stop-on-error 提前终止
  - `pass_rate` 明确按 `tested` 口径计算

## Files
- `scripts/verify_examples_compile.sh`
- `tests/scripts/test_verify_examples_compile_stop_on_error_summary_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，用 fake `fpc` 构造“第 2 个示例失败，第 3 个示例未处理”的 stop-on-error 场景。
2. 证明当前 JSON summary 仍把半程计数伪装成全量 `total`。
3. 最小修改 `verify_examples_compile.sh`，把 stop-on-error 的 partial truth 显式写进 summary。
4. 复跑 focused 合同与语法检查。
5. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_verify_examples_compile_stop_on_error_summary_contract.sh
bash tests/scripts/test_verify_examples_compile_stop_on_error_summary_contract.sh
bash -n scripts/verify_examples_compile.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - summary 看起来像只有 2 个文件
  - 没有 `tested` / `remaining` / `stopped_early`
- 修复后：
  - `total=3`
  - `tested=2`
  - `remaining=1`
  - `stopped_early=true`
  - `pass_rate` 继续按已测试样本计算。
