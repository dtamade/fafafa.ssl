param(
  [string]$RunId = "",
  [string]$OutputDir = "",
  [switch]$DryRun,
  [switch]$SkipWinsslBlockerBatch
)

$ErrorActionPreference = "Continue"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = Split-Path -Parent $ScriptDir

if ([string]::IsNullOrWhiteSpace($RunId)) {
  $RunId = Get-Date -Format "yyyyMMdd_HHmmss"
}

if ([string]::IsNullOrWhiteSpace($OutputDir)) {
  if (-not [string]::IsNullOrWhiteSpace($env:FAFAFA_WAVE_B_REPORTS_DIR)) {
    $OutputDir = $env:FAFAFA_WAVE_B_REPORTS_DIR
  } else {
    $OutputDir = "tmp/wave_b_reports"
  }
}

$OutDirAbs = Join-Path $ProjectRoot $OutputDir
if (-not (Test-Path $OutDirAbs)) {
  New-Item -Path $OutDirAbs -ItemType Directory -Force | Out-Null
}

$PowerShellExe = "pwsh"
if (-not (Get-Command $PowerShellExe -ErrorAction SilentlyContinue)) {
  if (Get-Command "powershell" -ErrorAction SilentlyContinue) {
    $PowerShellExe = "powershell"
  } else {
    Write-Host "[WAVE-B-WINDOWS] [FAIL] no PowerShell host found (pwsh/powershell)" -ForegroundColor Red
    exit 1
  }
}

$WinsslBlockerLog = Join-Path $OutDirAbs "wave_b_windows_winssl_blocker_batch_${RunId}.log"
$WinsslLog = Join-Path $OutDirAbs "wave_b_windows_winssl_${RunId}.log"
$OpenSSLLog = Join-Path $OutDirAbs "wave_b_windows_openssl_${RunId}.log"
$ModulesLog = Join-Path $OutDirAbs "wave_b_windows_modules_${RunId}.log"
$SummaryFile = Join-Path $OutDirAbs "wave_b_windows_gate_summary_${RunId}.md"
$WinsslBlockerLogRel = $WinsslBlockerLog.Replace($ProjectRoot + [IO.Path]::DirectorySeparatorChar, '')
$WinsslLogRel = $WinsslLog.Replace($ProjectRoot + [IO.Path]::DirectorySeparatorChar, '')
$OpenSSLLogRel = $OpenSSLLog.Replace($ProjectRoot + [IO.Path]::DirectorySeparatorChar, '')
$ModulesLogRel = $ModulesLog.Replace($ProjectRoot + [IO.Path]::DirectorySeparatorChar, '')
$SummaryRel = $SummaryFile.Replace($ProjectRoot + [IO.Path]::DirectorySeparatorChar, '')

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

  & $PowerShellExe -ExecutionPolicy Bypass -Command $Command *> $LogPath
  return $LASTEXITCODE
}

$winsslBlockerCmd = "Set-Location '$ProjectRoot'; bash scripts/run_windows_winssl_blocker_batch_draft.sh --run-id $RunId --reports-dir $OutputDir --output $OutputDir/winssl_blocker_batch_${RunId}.md --strict"
$winsslCmd = "Set-Location '$ProjectRoot'; ./run_winssl_tests.ps1"
$opensslCmd = "Set-Location '$ProjectRoot'; ./run_openssl_tests.ps1"
$modulesCmd = "Set-Location '$ProjectRoot'; ./scripts/validate_all_modules.ps1 -ProjectRoot '$ProjectRoot' -UnitOutputDir '$OutputDir/wave_b_windows_validate_units_${RunId}'"

$winsslBlockerExit = "SKIP"
$winsslBlockerStatus = "SKIPPED"
$winsslBlockerEvidence = "<none>"
if (-not $SkipWinsslBlockerBatch) {
  $winsslBlockerExit = Invoke-WaveStep -Name "winssl_blocker_batch" -Command $winsslBlockerCmd -LogPath $WinsslBlockerLog
  $winsslBlockerStatus = if ($winsslBlockerExit -eq 0) { "PASS" } else { "FAIL" }
  $winsslBlockerEvidence = $WinsslBlockerLog.Replace($ProjectRoot + [IO.Path]::DirectorySeparatorChar, '')
}

$winsslExit = Invoke-WaveStep -Name "winssl" -Command $winsslCmd -LogPath $WinsslLog
$opensslExit = Invoke-WaveStep -Name "openssl" -Command $opensslCmd -LogPath $OpenSSLLog
$modulesExit = Invoke-WaveStep -Name "modules" -Command $modulesCmd -LogPath $ModulesLog

$winsslStatus = if ($winsslExit -eq 0) { "PASS" } else { "FAIL" }
$opensslStatus = if ($opensslExit -eq 0) { "PASS" } else { "FAIL" }
$modulesStatus = if ($modulesExit -eq 0) { "PASS" } else { "FAIL" }

$overall = "FAIL"
if (
  $winsslStatus -eq "PASS" -and
  ($winsslBlockerStatus -eq "PASS" -or $winsslBlockerStatus -eq "SKIPPED") -and
  $opensslStatus -eq "PASS" -and
  $modulesStatus -eq "PASS"
) {
  $overall = "PASS"
}

$mode = "live"
if ($DryRun) {
  $mode = "dry-run"
  Write-Host "[DRY-RUN] run_id=$RunId"
  Write-Host "[DRY-RUN] output_dir=$OutputDir"
  Write-Host "[DRY-RUN] summary=$SummaryRel"
  Write-Host "[DRY-RUN] winssl_blocker_log=$WinsslBlockerLogRel"
  Write-Host "[DRY-RUN] winssl_log=$WinsslLogRel"
  Write-Host "[DRY-RUN] openssl_log=$OpenSSLLogRel"
  Write-Host "[DRY-RUN] modules_log=$ModulesLogRel"
  if ($winsslBlockerStatus -ne "SKIPPED") {
    $winsslBlockerStatus = "DRY_RUN"
  }
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
| winssl_blocker_batch | $winsslBlockerExit | $winsslBlockerStatus | $winsslBlockerEvidence |
| winssl | $winsslExit | $winsslStatus | $WinsslLogRel |
| openssl | $opensslExit | $opensslStatus | $OpenSSLLogRel |
| modules | $modulesExit | $modulesStatus | $ModulesLogRel |
"@

$summary | Out-File -FilePath $SummaryFile -Encoding utf8
Write-Host "[WAVE-B-WINDOWS] summary: $SummaryRel" -ForegroundColor Green

if ($overall -eq "PASS" -or $overall -eq "DRY_RUN") {
  exit 0
}
exit 1
