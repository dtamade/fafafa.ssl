# Wave B cross-platform summary stable states（2026-03-15）

## Goal
- 消除 `scripts/generate_wave_b_cross_platform_summary.sh` 中残留的字面量 `TODO` 占位状态。
- 让“未回填 / probe-only / pass / fail / dry-run”都落到稳定、可审查、可机读的状态集合。

## Architecture / Approach
1. 去掉内部 `TODO` sentinel
2. 将 step 级状态分成两层：
   - `parse_check_state`：只解析已知状态，未知返回空
   - `stable_check_state`：把空值稳定收敛到 `PENDING`
3. 规则：
   - Linux step 缺失时回退到 Linux overall
   - macOS/Windows step 缺失时默认 `PENDING`
   - macOS/Windows overall 直接映射平台 state：`PASS/FAIL/DRY_RUN/PROBE_ONLY/PROBE_OK/PENDING/READY`

## Files
- `scripts/generate_wave_b_cross_platform_summary.sh`
- `tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh`
- Existing regressions:
  - `tests/scripts/test_wave_b_cross_platform_summary.sh`
  - `tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`

## Step-by-step Commands
1. Syntax:
   - `bash -n scripts/generate_wave_b_cross_platform_summary.sh`
2. Contracts:
   - `bash tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
   - `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`

## Expected Outputs
- 当 macOS/Windows 证据缺失时，checklist 使用 `PENDING`，不再出现 `TODO`
- 当平台 summary 已提供时，原有 PASS/FAIL/SKIP 映射保持不变
