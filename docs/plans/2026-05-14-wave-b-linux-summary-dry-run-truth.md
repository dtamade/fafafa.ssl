# 2026-05-14 Wave B Linux Summary Dry-Run Truth

## Goal
收口 `scripts/run_wave_b_ci_gate.sh --dry-run` 的假绿 summary，避免 dry-run 产物继续把 Linux Wave B gate 写成 `PASS`，并误导上层 `cross_summary / closure / consistency / handoff` 把一份未执行的门禁当成真实绿证据。

## Architecture
- `run_wave_b_ci_gate.sh` 是 Linux `Wave B CI Gate Summary` 的直接 producer。
- 上层会消费这个 summary 的关键 truth：
  - `Run ID`
  - `Overall Status`
  - `Gate Steps`
  - `verify_examples_compile` 这类 step status
- 当前 repo 里其他平台的 dry-run 已经有稳定语义：
  - `run_wave_b_macos_gate.sh` 写 `overall=DRY_RUN`
  - `run_wave_b_windows_gate.ps1` 写 `overall=DRY_RUN`
- 但 Linux gate 此前在 `--dry-run` 下仍会：
  - 为启用的 step 写 `PASS`
  - 为 overall 写 `PASS`
  - 同时 examples metrics 仍是 `n/a`
- 这不是 harmless placeholder，而是一份会伪装成真实绿 summary 的 producer-side 假证据。

## Files
- `scripts/run_wave_b_ci_gate.sh`
- `tests/scripts/test_wave_b_ci_gate_dry_run_truth_contract.sh`
- `tests/scripts/test_wave_b_ci_gate_run_id_passthrough_contract.sh`
- `tests/scripts/test_wave_b_ci_gate_fast_local_clean_worktree_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED 合同，证明 `--dry-run` 时 summary 仍错误写成 `PASS`。
2. 最小修改 `scripts/run_wave_b_ci_gate.sh`：
   - dry-run 下启用的 step 状态改成 `DRY_RUN`
   - overall 改成 `DRY_RUN`
   - 增加显式 `Mode: dry-run/live`
   - dry-run 返回码继续保持 0
3. 复跑 Linux gate 邻近合同。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_wave_b_ci_gate_dry_run_truth_contract.sh
bash tests/scripts/test_wave_b_ci_gate_dry_run_truth_contract.sh
bash -n scripts/run_wave_b_ci_gate.sh
bash tests/scripts/test_wave_b_ci_gate_run_id_passthrough_contract.sh
bash tests/scripts/test_wave_b_ci_gate_fast_local_clean_worktree_contract.sh
git diff --check
```

## Expected Outputs
- 新合同在修复前 FAIL：
  - dry-run summary 不应再写 `Overall Status: **PASS**`
  - dry-run summary 不应再把已启用 step 写成 `PASS`
- 修复后：
  - `Overall Status: **DRY_RUN**`
  - summary 显式记录 `Mode: dry-run`
  - 启用的 step 都写成 `DRY_RUN`
  - dry-run 仍返回 0，继续支持 cheap contract / fast-local 工作流。
