# 2026-05-14 Wave B Linux Examples Threshold Truth

## Goal
收口 `scripts/run_wave_b_ci_gate.sh` 对 examples gate 的判定语义漂移，避免 Linux Wave B gate 在 `examples pass_rate` 已达阈值时，仅因为 `verify_examples_compile.sh` 报告了少量 failed files 就继续把 gate 判成 FAIL。

## Architecture
- `scripts/verify_examples_compile.sh` 负责产出 `examples_compile_ci_gate.json`：
  - `summary.total`
  - `summary.passed`
  - `summary.failed`
  - `summary.skipped`
  - `summary.pass_rate`
- 这个 helper 的退出码语义是：
  - `failed > 0` 就 `exit 1`
  - 不表达“是否达到上层阈值”
- `scripts/run_wave_b_ci_gate.sh` 的用户契约却写得更清楚：
  - “示例编译门禁（按通过率阈值判定）”
  - 默认阈值 `80.0`
- 修复前 Linux gate 同时要求：
  - JSON `pass_rate >= threshold`
  - `verify_examples_compile.sh` 退出码必须是 0
- 这让 examples gate 实际变成“阈值 + 零失败”双重门禁，与现有文档和 Wave A/Wave B 路线图里的阈值语义不一致。

## Files
- `scripts/run_wave_b_ci_gate.sh`
- `tests/scripts/test_wave_b_ci_gate_examples_threshold_contract.sh`
- `tests/scripts/test_wave_b_ci_gate_dry_run_truth_contract.sh`
- `tests/scripts/test_wave_b_ci_gate_run_id_passthrough_contract.sh`
- `tests/scripts/test_wave_b_ci_gate_fast_local_clean_worktree_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED 合同，用 fake `verify_examples_compile.sh` 产出：
   - 有效 JSON
   - `pass_rate=94.7`
   - helper `exit 1`
2. 证明当前 Linux gate 仍会把 `verify_examples_compile` step 和 `Overall Status` 判成 FAIL。
3. 最小修改 `scripts/run_wave_b_ci_gate.sh`：
   - examples step 改为“JSON 可解析且 `pass_rate >= threshold` 即 PASS”
   - 保留底层 helper exit code 作为 evidence，不再把它当最终 gate 真值
4. 复跑 Linux gate 邻近合同。
5. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_wave_b_ci_gate_examples_threshold_contract.sh
bash tests/scripts/test_wave_b_ci_gate_examples_threshold_contract.sh
bash -n scripts/run_wave_b_ci_gate.sh
bash tests/scripts/test_wave_b_ci_gate_dry_run_truth_contract.sh
bash tests/scripts/test_wave_b_ci_gate_run_id_passthrough_contract.sh
bash tests/scripts/test_wave_b_ci_gate_fast_local_clean_worktree_contract.sh
git diff --check
```

## Expected Outputs
- 新合同在修复前 FAIL：
  - `pass_rate=94.7 >= 80.0`
  - helper `exit 1`
  - Linux gate 仍错误返回非 0，并把 examples step / overall 判成 FAIL
- 修复后：
  - examples step 按阈值真相判成 `PASS`
  - overall 保持 `PASS`
  - summary 继续保留底层 helper `exit 1` 作为可审计 evidence。
