# Wave B Windows Gate: OpenSSL minimal test runner (non-interactive)
#
# Runner-safe goals:
# - only compile/run minimal stable test(s)
# - build outputs isolated under tmp/
# - exit non-zero on failures

[CmdletBinding()]
param(
  [string]$ProjectRoot = "",
  [string]$RunId = "",
  [string]$OutputDir = "test-reports"
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

$BuildDir = Join-Path $ProjectRootAbs ("tmp\\wave_b_openssl_" + $RunId)
if (-not (Test-Path $BuildDir)) {
  New-Item -Path $BuildDir -ItemType Directory -Force | Out-Null
}

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Wave B OpenSSL Minimal Tests" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ("run_id: " + $RunId) -ForegroundColor Gray
Write-Host ("project_root: " + $ProjectRootAbs) -ForegroundColor Gray
Write-Host ("build_dir: " + $BuildDir) -ForegroundColor Gray
Write-Host ""

Write-Host "[INFO] fpc version" -ForegroundColor Gray
try {
  & fpc -iV
} catch {
  Write-Host "[WARN] failed to query fpc version" -ForegroundColor Yellow
}
Write-Host ""

Write-Host "[INFO] openssl.exe detection (best-effort)" -ForegroundColor Gray
$openssl = Get-Command openssl -ErrorAction SilentlyContinue
if ($openssl) {
  Write-Host ("  openssl: " + $openssl.Source) -ForegroundColor Gray
} else {
  Write-Host "  openssl: not found in PATH" -ForegroundColor Yellow
}
Write-Host ""

$TestFileRel = "tests\\openssl\\test_openssl_minimal.pas"
$TestFileAbs = Join-Path $ProjectRootAbs $TestFileRel
if (-not (Test-Path $TestFileAbs)) {
  Write-Host ("[FAIL] missing test file: " + $TestFileRel) -ForegroundColor Red
  exit 1
}

$UnitPaths = @(
  ("-Fu" + $SrcDir)
)

$name = [IO.Path]::GetFileNameWithoutExtension($TestFileAbs)
$exePath = Join-Path $BuildDir ($name + ".exe")

Write-Host ("[BUILD] " + $TestFileRel) -ForegroundColor Yellow
$compileOut = & fpc @UnitPaths ("-FE" + $BuildDir) ("-FU" + $BuildDir) $TestFileAbs 2>&1
if ($PSBoundParameters.ContainsKey('Verbose')) {
  $compileOut | ForEach-Object { Write-Host ("  " + $_) -ForegroundColor DarkGray }
}
if ($LASTEXITCODE -ne 0 -or -not (Test-Path $exePath)) {
  Write-Host ("[FAIL] compile failed: " + $TestFileRel) -ForegroundColor Red
  $compileOut | Select-Object -Last 120 | ForEach-Object { Write-Host ("  " + $_) -ForegroundColor DarkGray }
  exit 1
}

Write-Host ("[RUN  ] " + $name) -ForegroundColor Yellow
& $exePath
if ($LASTEXITCODE -ne 0) {
  Write-Host ("[FAIL] runtime failed: " + $name + " (exit=" + $LASTEXITCODE + ")") -ForegroundColor Red
  exit 1
}

Write-Host ("[PASS] " + $name) -ForegroundColor Green
Write-Host "[WAVE-B-OPENSSL] PASS" -ForegroundColor Green
exit 0
