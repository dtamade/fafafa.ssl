# 2026-05-14 Wave B Invalid Examples Threshold Truth

## Goal
收口 `scripts/run_wave_b_ci_gate.sh --examples-threshold` 的输入校验真值，避免非法阈值被伪装成一次普通的 examples gate 失败。

## Architecture
- 当前脚本接受 `--examples-threshold FLOAT`，但没有在参数阶段做校验。
- 直到 examples JSON 解析完成后，才会在这里做：
  - `rate = float("$examples_rate")`
  - `threshold = float("$EXAMPLES_THRESHOLD")`
- 当阈值是非法字符串时：
  - compile/modules/examples 仍会先执行
  - Python 在中途抛 `ValueError`
  - 最终 summary 会把 `verify_examples_compile` 标成 `FAIL`
  - `Overall Status` 也变成 `FAIL`
- 这是一条很典型的 CLI 假分类：
  - 真实问题是“用户输入非法”
  - 报告表面却像是“examples gate 没过”

## Files
- `scripts/run_wave_b_ci_gate.sh`
- `tests/scripts/test_wave_b_ci_gate_invalid_examples_threshold_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，证明非法 `--examples-threshold` 当前仍会真正跑门禁并生成一份伪装成 gate FAIL 的 summary。
2. 最小修改 `run_wave_b_ci_gate.sh`，把阈值解析失败前移到参数阶段。
3. 复跑 focused 合同与 Wave B gate 邻近合同。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_wave_b_ci_gate_invalid_examples_threshold_contract.sh
bash tests/scripts/test_wave_b_ci_gate_invalid_examples_threshold_contract.sh
bash -n scripts/run_wave_b_ci_gate.sh
bash tests/scripts/test_wave_b_ci_gate_examples_threshold_contract.sh
bash tests/scripts/test_wave_b_ci_gate_dry_run_truth_contract.sh
bash tests/scripts/test_wave_b_ci_gate_run_id_passthrough_contract.sh
bash tests/scripts/test_wave_b_ci_gate_fast_local_clean_worktree_contract.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - 非法阈值仍会触发 compile/modules/examples
  - stderr 出现 Python `ValueError`
  - summary 把 examples step 误写成 `FAIL`
- 修复后：
  - 参数阶段直接非零退出
  - stderr 明确说明阈值非法
  - 不再执行任何 gate step，也不再生成伪装 summary。
