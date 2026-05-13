# 2026-05-14 Wave B/B2 Consistency Cross Summary Metadata Truth

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 对 `cross summary` 元数据的盲信，避免 `cross_summary` 缺失 `linux_summary` / `linux_examples_json` 等关键字段时，只要真实 evidence 还在，consistency 仍错误产出 `CONSISTENT`。

## Architecture
- `check_wave_b_b2_evidence_consistency.sh` 当前会消费：
  - `cross_summary`
  - `closure_report`
  - active linux/macOS/windows evidence
- 它此前只把 `cross_summary` 当成一个“带 run_id 的 markdown artifact”：
  - 会继承路径
  - 但不会校验 `cross summary` 自己的关键 metadata 是否完整
- 这会让“真实 evidence 仍在，但 cross summary 已损坏”的场景被伪装成绿色一致性。

## Files
- `scripts/check_wave_b_b2_evidence_consistency.sh`
- `tests/scripts/test_wave_b_b2_consistency_cross_summary_metadata_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED 合同，构造一个缺失 `linux_examples_json` 的 cross summary。
2. 证明当前 consistency 即使在 strict 模式下也仍会给出 `CONSISTENT`。
3. 最小修改 `check_wave_b_b2_evidence_consistency.sh`：
   - 为 `cross_summary` 单独加 artifact 校验
   - 校验 `linux_summary` / `linux_examples_json` metadata
   - 缺失时计入 `runid_mismatch_or_parse_issue`
4. 复跑 consistency 与 prepare/handoff 邻近回归。
5. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_metadata_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_metadata_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_closure_status_parse_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_next_actions_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_report_chain_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh
bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_strict_metadata_contract.sh
git diff --check
```

## Expected Outputs
- 新合同在修复前 FAIL：malformed `cross summary` 仍被 consistency 当作正常 artifact，strict 模式不失败。
- 修复后：
  - `cross_summary` 缺失关键 metadata 时落到 `INCONSISTENT`
  - `cross_summary` 行显式写出 metadata parse issue
  - 现有 linux/macOS/windows path 继承与 handoff 邻近语义保持不变。
