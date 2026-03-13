# Wave B Windows Gate: WinSSL minimal test runner (non-interactive)
#
# This script is runner-safe:
# - does not assume lazbuild exists
# - does not scan examples/
# - compiles to an isolated tmp/ folder
# - exits non-zero on failures

[CmdletBinding()]
param(
  [string]$ProjectRoot = "",
  [string]$RunId = "",
  [string]$OutputDir = "test-reports",
  [switch]$Verbose
)

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
if ([string]::IsNullOrWhiteSpace($ProjectRoot)) {
  $ProjectRoot = $ScriptDir
}
if ([string]::IsNullOrWhiteSpace($RunId)) {
  $RunId = Get-Date -Format "yyyyMMdd_HHmmss"
}

$ProjectRootAbs = (Resolve-Path $ProjectRoot).Path
$SrcDir = Join-Path $ProjectRootAbs "src"
$OutDirAbs = Join-Path $ProjectRootAbs $OutputDir
if (-not (Test-Path $OutDirAbs)) {
  New-Item -Path $OutDirAbs -ItemType Directory -Force | Out-Null
}

$BuildDir = Join-Path $ProjectRootAbs ("tmp\\wave_b_winssl_" + $RunId)
if (-not (Test-Path $BuildDir)) {
  New-Item -Path $BuildDir -ItemType Directory -Force | Out-Null
}

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Wave B WinSSL Minimal Tests" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ("run_id: " + $RunId) -ForegroundColor Gray
Write-Host ("project_root: " + $ProjectRootAbs) -ForegroundColor Gray
Write-Host ("build_dir: " + $BuildDir) -ForegroundColor Gray
Write-Host ""

# Minimal, non-network, non-interactive tests.
$TestFiles = @(
  "tests\\winssl\\test_winssl_api_basic.pas",
  "tests\\unit\\test_winssl_comprehensive.pas"
)

$UnitPaths = @(
  ("-Fu" + $SrcDir)
)

function Invoke-CompileRun {
  param(
    [string]$PasRelPath
  )

  $pasPath = Join-Path $ProjectRootAbs $PasRelPath
  if (-not (Test-Path $pasPath)) {
    Write-Host ("[FAIL] missing test file: " + $PasRelPath) -ForegroundColor Red
    return 1
  }

  $name = [IO.Path]::GetFileNameWithoutExtension($pasPath)
  $exePath = Join-Path $BuildDir ($name + ".exe")

  Write-Host ("[BUILD] " + $PasRelPath) -ForegroundColor Yellow
  $compileOut = & fpc @UnitPaths ("-FE" + $BuildDir) ("-FU" + $BuildDir) $pasPath 2>&1
  if ($Verbose) {
    $compileOut | ForEach-Object { Write-Host ("  " + $_) -ForegroundColor DarkGray }
  }
  if ($LASTEXITCODE -ne 0 -or -not (Test-Path $exePath)) {
    Write-Host ("[FAIL] compile failed: " + $PasRelPath) -ForegroundColor Red
    return 1
  }

  Write-Host ("[RUN  ] " + $name) -ForegroundColor Yellow
  & $exePath
  if ($LASTEXITCODE -ne 0) {
    Write-Host ("[FAIL] runtime failed: " + $name + " (exit=" + $LASTEXITCODE + ")") -ForegroundColor Red
    return 1
  }
  Write-Host ("[PASS] " + $name) -ForegroundColor Green
  return 0
}

$failures = 0
foreach ($testFile in $TestFiles) {
  $exitCode = Invoke-CompileRun -PasRelPath $testFile
  if ($exitCode -ne 0) { $failures++ }
  Write-Host ""
}

if ($failures -gt 0) {
  Write-Host ("[WAVE-B-WINSSL] FAIL: " + $failures + " test(s) failed") -ForegroundColor Red
  exit 1
}

Write-Host "[WAVE-B-WINSSL] PASS" -ForegroundColor Green
exit 0

