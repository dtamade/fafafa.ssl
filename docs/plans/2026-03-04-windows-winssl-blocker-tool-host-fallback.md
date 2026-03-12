# windows winssl blocker tool host fallback (2026-03-04)

## Goal
提升 `run_windows_winssl_blocker_batch_draft.sh` 在不同 runner 上的工具兼容性，避免对隐式 PATH 前提的脆弱依赖。

## Architecture / Scope
- `scripts/run_windows_winssl_blocker_batch_draft.sh`
  - 新增 `LAZBUILD_EXE` 与 `FPC_EXE` 解析（env 覆盖 + command probe）。
  - 在 `--dry-run` 下允许缺失工具时继续输出命令预览并告警。
  - 在 live 模式缺失工具时快速失败。
  - P1-33~P1-35 命令改为使用 `$LAZBUILD_EXE`，P1-36 使用 `$FPC_EXE`。

## Files
- Modify: `scripts/run_windows_winssl_blocker_batch_draft.sh`
- Add: `tests/scripts/test_windows_winssl_blocker_batch_tool_host_fallback_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：
   - 新增静态合同，要求脚本存在 `LAZBUILD_EXE` / `FPC_EXE` 探测与 dry-run 容错逻辑，并通过变量执行命令。
2. GREEN：
   - 最小改造 blocker 批次脚本，补齐工具解析与命令变量化。
3. Regression：
   - 新合同。
   - 既有 blocker 合同：
     - `test_windows_winssl_blocker_batch_draft_dryrun_contract.sh`
     - `test_windows_winssl_blocker_batch_draft_failure_contract.sh`
   - Wave B 集成合同：
     - `test_wave_b_windows_gate_winssl_blocker_batch_integration_contract.sh`
   - `bash -n` 语法检查。

## Expected Outputs
- 脚本对 `lazbuild` / `fpc` 的依赖更显式、可覆盖、可审计。
- dry-run 在缺工具场景仍能生成完整命令证据。
