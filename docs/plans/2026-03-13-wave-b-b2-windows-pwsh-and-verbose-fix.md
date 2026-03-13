# Wave B / B2: Windows gate 修复（Verbose 冲突 + pwsh 执行 + UTF-8 日志）

## Goal
- 修复 Windows Wave B gate 在 runner 上的全量失败，使 B2 闭环具备可复现、可审查的证据链。

## Root Cause (From 2026-03-13 runner logs)
1) `run_openssl_tests.ps1` / `run_winssl_tests.ps1` 使用 `[CmdletBinding()]`，但又在 `param(...)` 中声明了 `[switch]$Verbose`：  
   - 与 PowerShell 的 common parameter `-Verbose` 冲突，导致 **parameter defined multiple times**。
2) `scripts/run_wave_b_windows_gate.ps1` 用 `powershell.exe` 执行子脚本，且 `*>` 默认写 UTF-16：  
   - 影响 UTF-8 脚本/Unicode 输出兼容与产物可读性（CI artifacts 解析困难）。

## Fix
- `run_openssl_tests.ps1` / `run_winssl_tests.ps1`：
  - 移除自定义 `[switch]$Verbose`（保留 common `-Verbose`）
  - 用 `$PSBoundParameters.ContainsKey('Verbose')` 控制编译输出回显
- `scripts/run_wave_b_windows_gate.ps1`：
  - 优先使用 `pwsh`（PowerShell 7）执行子步骤；无 `pwsh` 时回退 `powershell`
  - 用 `Out-File -Encoding utf8` 统一日志编码，提升 artifact 可读性

## Files
- Modify: `run_openssl_tests.ps1`
- Modify: `run_winssl_tests.ps1`
- Modify: `scripts/run_wave_b_windows_gate.ps1`
- Add: `tests/scripts/test_wave_b_windows_gate_pwsh_and_verbose_contract.sh`

## Verification
- `bash tests/scripts/test_wave_b_windows_gate_pwsh_and_verbose_contract.sh`
- （CI）触发 `.github/workflows/wave-b-b2-manual.yml`，确认 Windows gate summary `overall=PASS`

## Expected Outputs / Acceptance
- contract test => PASS
- Windows gate 不再出现 `Verbose was defined multiple times` / PowerShell parse errors
- `test-reports/wave_b_windows_*.log` 为 UTF-8（可直接在 Linux/macOS 上查看）
