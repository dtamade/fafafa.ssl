# 2026-05-14 Wave B Cross Summary Linux Overall State Truth

## Goal
收口 `generate_wave_b_cross_platform_summary.sh` 对 Linux `Overall Status` 的原样透传，避免生成器在 Linux overall 缺失或非法时，把 `UNKNOWN/BROKEN` 这类不在 repo 合法平台状态集合内的值直接写进 `cross_summary`。

## Architecture
- `cross_summary` 的 `Platform Evidence Status` 和 `Cross-Platform Checklist` 都会暴露 Linux platform state。
- consumer 侧现在已经要求 `cross_summary` 的 Linux state 必须属于允许集合：
  - `PASS`
  - `FAIL`
  - `DRY_RUN`
  - `READY`
  - `PENDING`
  - `PROBE_ONLY`
  - `PROBE_OK`
- 但生成器此前对 Linux overall 没做正规化：
  - 缺失 `Overall Status` 时输出 `UNKNOWN`
  - 非法值如 `BROKEN` 会被原样透传
- `closure` 脚本已经把同类 Linux overall unknown 收口成 `READY`，所以这次是 `cross_summary` 独有的 producer-side 漂移。

## Files
- `scripts/generate_wave_b_cross_platform_summary.sh`
- `tests/scripts/test_wave_b_cross_platform_summary_linux_overall_state_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused RED 合同，构造 Linux overall 缺失和非法两种场景。
2. 证明当前生成器会把 `UNKNOWN/BROKEN` 直接写进 Linux 平台行和 checklist overall 行。
3. 最小修改 `generate_wave_b_cross_platform_summary.sh`：
   - 将 Linux overall 正规化到合法 platform state
   - 保持现有 step-check fallback 逻辑
   - 收掉过时的 `UNKNOWN` Next Actions 文案
4. 复跑 generator 邻近合同和依赖它的 consumer/prepare 回归。
5. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_wave_b_cross_platform_summary_linux_overall_state_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary_linux_overall_state_contract.sh
bash -n scripts/generate_wave_b_cross_platform_summary.sh
bash tests/scripts/test_wave_b_cross_platform_summary_next_actions_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary_explicit_missing_evidence_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh
bash tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_run_id_inference_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_existing_report_run_id_fallback_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh
bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh
git diff --check
```

## Expected Outputs
- 新合同在修复前 FAIL：
  - Linux `Overall Status` 缺失时不应再产出 `UNKNOWN`
  - Linux `Overall Status` 非法时不应再产出 `BROKEN`
- 修复后：
  - `cross_summary` 的 Linux platform row 与 checklist overall row 统一落到合法的 `READY`
  - 旧的 `UNKNOWN` Linux guidance 从 `Next Actions` 中消失
  - generator 与 closure/consistency 的状态语义重新对齐。
