param(
  [string]$RunId = "",
  [string]$OutputDir = "test-reports",
  [switch]$DryRun
)

$ErrorActionPreference = "Continue"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = Split-Path -Parent $ScriptDir

if ([string]::IsNullOrWhiteSpace($RunId)) {
  $RunId = Get-Date -Format "yyyyMMdd_HHmmss"
}

$OutDirAbs = Join-Path $ProjectRoot $OutputDir
if (-not (Test-Path $OutDirAbs)) {
  New-Item -Path $OutDirAbs -ItemType Directory -Force | Out-Null
}

$WinsslLog = Join-Path $OutDirAbs "wave_b_windows_winssl_${RunId}.log"
$OpenSSLLog = Join-Path $OutDirAbs "wave_b_windows_openssl_${RunId}.log"
$ModulesLog = Join-Path $OutDirAbs "wave_b_windows_modules_${RunId}.log"
$SummaryFile = Join-Path $OutDirAbs "wave_b_windows_gate_summary_${RunId}.md"

function Invoke-WaveStep {
  param(
    [string]$Name,
    [string]$Command,
    [string]$LogPath
  )

  Write-Host "[WAVE-B-WINDOWS] [$Name] $Command" -ForegroundColor Cyan

  if ($DryRun) {
    "[DRY-RUN] $Command" | Out-File -FilePath $LogPath -Encoding utf8
    return 0
  }

  # Prefer PowerShell 7 (pwsh) for UTF-8 script compatibility; fallback to Windows PowerShell.
  $psExe = "powershell"
  if (Get-Command pwsh -ErrorAction SilentlyContinue) {
    $psExe = "pwsh"
  }

  # Ensure logs are UTF-8 for artifact readability (avoid UTF-16 redirection by default).
  & $psExe -NoProfile -ExecutionPolicy Bypass -Command $Command *>&1 | Out-File -FilePath $LogPath -Encoding utf8
  return $LASTEXITCODE
}

$winsslCmd = "Set-Location '$ProjectRoot'; ./run_winssl_tests.ps1 -ProjectRoot '$ProjectRoot' -RunId '$RunId' -OutputDir '$OutputDir'"
$opensslCmd = "Set-Location '$ProjectRoot'; ./run_openssl_tests.ps1 -ProjectRoot '$ProjectRoot' -RunId '$RunId' -OutputDir '$OutputDir'"
$modulesCmd = "Set-Location '$ProjectRoot'; ./scripts/validate_all_modules.ps1 -ProjectRoot '$ProjectRoot' -RunId '$RunId' -OutputDir '$OutputDir'"

$winsslExit = Invoke-WaveStep -Name "winssl" -Command $winsslCmd -LogPath $WinsslLog
$opensslExit = Invoke-WaveStep -Name "openssl" -Command $opensslCmd -LogPath $OpenSSLLog
$modulesExit = Invoke-WaveStep -Name "modules" -Command $modulesCmd -LogPath $ModulesLog

$winsslStatus = if ($winsslExit -eq 0) { "PASS" } else { "FAIL" }
$opensslStatus = if ($opensslExit -eq 0) { "PASS" } else { "FAIL" }
$modulesStatus = if ($modulesExit -eq 0) { "PASS" } else { "FAIL" }

$overall = "FAIL"
if ($winsslStatus -eq "PASS" -and $opensslStatus -eq "PASS" -and $modulesStatus -eq "PASS") {
  $overall = "PASS"
}

$mode = "live"
if ($DryRun) {
  $mode = "dry-run"
  $winsslStatus = "DRY_RUN"
  $opensslStatus = "DRY_RUN"
  $modulesStatus = "DRY_RUN"
  $overall = "DRY_RUN"
}

$generatedAt = Get-Date -Format "yyyy-MM-dd HH:mm:ss zzz"
$summary = @"
# Wave B Windows Gate Summary

- run_id: $RunId
- generated_at: $generatedAt
- mode: $mode
- overall: **$overall**

## Steps

| step | exit | status | evidence |
|------|------|--------|----------|
| winssl | $winsslExit | $winsslStatus | $($WinsslLog.Replace($ProjectRoot + [IO.Path]::DirectorySeparatorChar, '')) |
| openssl | $opensslExit | $opensslStatus | $($OpenSSLLog.Replace($ProjectRoot + [IO.Path]::DirectorySeparatorChar, '')) |
| modules | $modulesExit | $modulesStatus | $($ModulesLog.Replace($ProjectRoot + [IO.Path]::DirectorySeparatorChar, '')) |
"@

$summary | Out-File -FilePath $SummaryFile -Encoding utf8
Write-Host "[WAVE-B-WINDOWS] summary: $($SummaryFile.Replace($ProjectRoot + [IO.Path]::DirectorySeparatorChar, ''))" -ForegroundColor Green

if ($overall -eq "PASS" -or $overall -eq "DRY_RUN") {
  exit 0
}
exit 1
