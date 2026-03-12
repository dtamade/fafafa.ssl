# 2026-03-10 Wave B Windows gate dry-run observability

## Goal
- 补齐 `scripts/run_wave_b_windows_gate.ps1 -DryRun` 的结构化 metadata 输出。
- 让 Windows gate dry-run 也具备稳定的 `run_id` / `output_dir` / summary / step-log 观察面。

## Architecture
- 这波只做 observability 收口，不改 Windows gate 的 step 执行语义、host fallback、或 blocker batch 逻辑。
- active Linux 环境没有 `pwsh` / `powershell`，所以合同采用源码级静态约束，冻结 `Write-Host "[DRY-RUN] key=value"` 输出面。
- 输出字段风格与已完成的 `run_wave_b_macos_gate.sh` dry-run metadata 对齐，统一成稳定的 `[DRY-RUN]` key-value 行。

## Files
- `scripts/run_wave_b_windows_gate.ps1`
- `tests/scripts/test_wave_b_windows_gate_dryrun_observability_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Commands
1. `bash tests/scripts/test_wave_b_windows_gate_dryrun_observability_contract.sh`
2. `bash -n tests/scripts/test_wave_b_windows_gate_dryrun_observability_contract.sh tests/scripts/test_wave_b_windows_gate_winssl_blocker_batch_integration_contract.sh tests/scripts/test_wave_b_windows_gate_validate_modules_passthrough_contract.sh tests/scripts/test_wave_b_windows_gate_powershell_host_fallback_contract.sh`
3. `bash tests/scripts/test_wave_b_windows_gate_dryrun_observability_contract.sh`
4. `bash tests/scripts/test_wave_b_windows_gate_winssl_blocker_batch_integration_contract.sh`
5. `bash tests/scripts/test_wave_b_windows_gate_validate_modules_passthrough_contract.sh`
6. `bash tests/scripts/test_wave_b_windows_gate_powershell_host_fallback_contract.sh`

## Expected Outputs
- RED：第 1 步先失败，指出 dry-run 缺少结构化 `run_id` / `output_dir` / `summary` / per-step log fields。
- GREEN：PowerShell 脚本源码显式包含这些 `[DRY-RUN]` metadata 输出。
- Verification：focused 合同与既有 Windows gate 邻近合同都通过。

## Verification
- `bash tests/scripts/test_wave_b_windows_gate_dryrun_observability_contract.sh` => PASS
- `bash -n tests/scripts/test_wave_b_windows_gate_dryrun_observability_contract.sh tests/scripts/test_wave_b_windows_gate_winssl_blocker_batch_integration_contract.sh tests/scripts/test_wave_b_windows_gate_validate_modules_passthrough_contract.sh tests/scripts/test_wave_b_windows_gate_powershell_host_fallback_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_windows_gate_winssl_blocker_batch_integration_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_windows_gate_validate_modules_passthrough_contract.sh` => PASS
- `bash tests/scripts/test_wave_b_windows_gate_powershell_host_fallback_contract.sh` => PASS

## Result
- Windows gate dry-run 现在会显式输出：
  - `run_id`
  - `output_dir`
  - `summary`
  - `winssl_blocker_log`
  - `winssl_log`
  - `openssl_log`
  - `modules_log`
- 这样 Windows platform gate 的 dry-run 也进入了结构化观测面。
