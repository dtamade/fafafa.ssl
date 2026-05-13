# 2026-05-14 Wave B/B2 Consistency Closure Platform Matrix Truth

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 对 `closure_report` 平台状态矩阵的盲信，避免 `closure_status` 虽然存在，但 `linux/macos/windows` 平台行缺失或非法时，strict consistency 仍错误产出 `CONSISTENT`。

## Architecture
- `check_wave_b_b2_evidence_consistency.sh` 之前只把 `closure_report` 当成：
  - run_id
  - closure_status
  这两个字段的组合。
- 但 `closure_report` 还承载了平台级 closure truth：
  - linux
  - macos
  - windows
- 如果平台矩阵已经坏了，只看 `closure_status` 会把坏 closure 链继续标成绿色一致性。

## Files
- `scripts/check_wave_b_b2_evidence_consistency.sh`
- `tests/scripts/test_wave_b_b2_consistency_closure_platform_matrix_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED 合同，构造 `closure_status=CLOSED` 但 closure 平台矩阵缺失 `windows` 行的场景。
2. 证明当前 strict consistency 仍继续返回 `CONSISTENT`。
3. 最小修改 `check_wave_b_b2_evidence_consistency.sh`：
   - 解析 closure 平台状态
   - 校验三平台行是否齐全且合法
   - 缺失/非法时计入 `runid_mismatch_or_parse_issue`
4. 复跑 consistency 与 handoff 邻近回归。
5. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash -n tests/scripts/test_wave_b_b2_consistency_closure_platform_matrix_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_closure_platform_matrix_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_closure_status_parse_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_next_actions_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_closure_platform_matrix_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_report_chain_contract.sh
git diff --check
```

## Expected Outputs
- 新合同在修复前 FAIL：`closure_report` 平台矩阵不完整时，strict consistency 仍错误给出 `CONSISTENT`。
- 修复后：
  - closure 平台矩阵缺失/非法会落到 `INCONSISTENT`
  - `closure_status_note` 与 `closure_report` 行都会显式暴露平台矩阵 parse issue
  - handoff 上层对同类 closure matrix 问题的防御继续保持兼容。
