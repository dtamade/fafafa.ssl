# 2026-05-14 Wave B/B2 Consistency Cross Summary Platform Matrix Truth

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 对 `cross_summary` 平台状态矩阵的盲信，避免 `linux/macos/windows` 行缺失或 `linux` state 非法时，strict consistency 仍错误产出 `CONSISTENT`。

## Architecture
- `cross_summary` 不只是 Linux metadata 容器，它还承载三平台当前 evidence truth：
  - `linux` 必须给出合法 state
  - `linux` evidence 不能为空
  - `macos/windows` 平台行也必须存在，哪怕当前是 `PENDING`
- 之前 consistency checker 只校验了 `cross_summary` 的：
  - `run_id`
  - `linux_summary`
  - `linux_examples_json`
  - active macOS / Windows evidence metadata
- 这意味着平台矩阵本身缺行或 Linux state 非法时，strict consistency 仍可能继续给绿灯。

## Files
- `scripts/check_wave_b_b2_evidence_consistency.sh`
- `tests/scripts/test_wave_b_b2_consistency_cross_summary_platform_matrix_contract.sh`
- `tests/scripts/test_wave_b_b2_consistency_closure_status_parse_contract.sh`
- `tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh`
- `tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED 合同，构造两个 `cross_summary` 假绿灯：
   - 缺失 `windows` 平台行
   - `linux` 行 state=`BROKEN`
2. 最小修改 `check_wave_b_b2_evidence_consistency.sh`：
   - 强制 `linux/macos/windows` state 行都可解析
   - 校验 `linux` state 属于允许集合
   - 强制 `linux` evidence 非空
   - 缺失/非法时计入 `runid_mismatch_or_parse_issue`
3. 复跑主线与邻近回归。
4. 若旧合同因为共享了过时极简 `cross_summary` 夹具而失败，只升级测试基线，不回退新的平台矩阵 guard。
5. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_platform_matrix_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_platform_matrix_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_metadata_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_platform_evidence_metadata_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_probe_missing_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_closure_platform_matrix_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_closure_status_parse_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_next_actions_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_report_chain_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh
bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh
git diff --check
```

## Expected Outputs
- 新合同在修复前 FAIL：
  - `cross_summary` 缺失 `windows` 行时 strict consistency 不能再返回 `CONSISTENT`
  - `linux` state 非法时必须落到 `INCONSISTENT`
- 修复后：
  - `cross_summary` 行显式暴露 `cross_summary platform state missing: windows`
  - `cross_summary` 行显式暴露 `invalid linux state: BROKEN`
  - 旧合同若只是复用了过时夹具，应通过升级夹具恢复单故障隔离，而不是放松新的 strict guard。
