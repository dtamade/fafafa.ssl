# windows powershell host fallback hardening (2026-03-04)

## Goal
提升 Windows 相关 gate/path-check 脚本在不同 PowerShell 宿主环境下的兼容性：
- 优先 `pwsh`，回退到 `powershell`。
- 在 dry-run 场景保持命令可预览，不因宿主缺失直接中断。

## Architecture / Scope
- `scripts/run_wave_b_windows_gate.ps1`
  - 增加 `pwsh -> powershell` 选择逻辑。
  - `Invoke-WaveStep` 执行器改为使用已解析宿主变量。
- `scripts/run_windows_winssl_path_check_draft.sh`
  - 增加 `POWERSHELL_EXE` 回退逻辑。
  - Windows test/build/version 三个命令统一走该变量。

## Files
- Modify: `scripts/run_wave_b_windows_gate.ps1`
- Modify: `scripts/run_windows_winssl_path_check_draft.sh`
- Add: `tests/scripts/test_wave_b_windows_gate_powershell_host_fallback_contract.sh`
- Add: `tests/scripts/test_windows_winssl_path_check_powershell_host_fallback_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：新增两条静态合同，验证当前实现缺少 host fallback。
2. GREEN：最小改造两个脚本。
3. Regression：
   - 两条新合同
   - `test_wave_b_windows_gate_winssl_blocker_batch_integration_contract.sh`
   - `test_wave_b_windows_gate_validate_modules_passthrough_contract.sh`
   - `test_windows_winssl_path_check_draft_dryrun_contract.sh`
   - `test_windows_winssl_path_check_isolation_passthrough_contract.sh`
   - `bash -n scripts/run_windows_winssl_path_check_draft.sh` + 新 bash 合同语法检查

## Expected Outputs
- 脚本在有 `pwsh` 或仅有 `powershell` 的环境都可执行。
- dry-run 场景继续输出可审计命令。
- 既有 windows 合同不回归。
