# 2026-05-14 Wave B/B2 Consistency Partial Linux Examples Truth

## Goal
收口 `check_wave_b_b2_evidence_consistency.sh` 对 partial `linux_examples_json` 的盲信，避免 `stopped_early=true` / `remaining>0` 的半程 examples 报告继续被 strict consistency 认成绿色完整证据。

## Architecture
- `verify_examples_compile.sh` 现在已经显式输出：
  - `summary.tested`
  - `summary.remaining`
  - `summary.stopped_early`
- 但 `check_wave_b_b2_evidence_consistency.sh` 当前仍只对 `linux_examples_json` 做：
  - 文件存在
  - JSON 可解析
- 这意味着：
  - 一个 parse-valid 的 partial examples 报告
  - 只要路径、run_id、cross_summary 其他结构都正常
  - strict consistency 仍可能继续给 `CONSISTENT`
- 这是一条新的 consumer-side 假绿灯：
  - producer 已经暴露了 partial truth
  - consumer 却没有把它接进来

## Files
- `scripts/check_wave_b_b2_evidence_consistency.sh`
- `tests/scripts/test_wave_b_b2_consistency_partial_linux_examples_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused contract，证明 active custom `linux_examples_json` 即使已经显式 `stopped_early=true`、`remaining>0`，strict consistency 仍会错误保持绿色。
2. 最小修改 `check_wave_b_b2_evidence_consistency.sh`，仅对 `linux_examples_json` 增加 partial-truth 校验。
3. 复跑 focused 合同与 linux-examples 邻近 consistency 合同。
4. 更新 working-memory，然后 review 并提交。

## Commands
```bash
bash -n tests/scripts/test_wave_b_b2_consistency_partial_linux_examples_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_partial_linux_examples_contract.sh
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_generic_linux_examples_fallback_contract.sh
git diff --check
```

## Expected Outputs
- 修复前：
  - partial custom `linux_examples_json` 仍会保持 `CONSISTENT`
  - `linux_examples_json` 行只写 `json_valid=YES`
- 修复后：
  - strict consistency 变成 `INCONSISTENT`
  - `linux_examples_json` 行显式暴露 `partial_examples_report=YES`
  - 旧的 full-run JSON 路径保持现有绿行为不变。
