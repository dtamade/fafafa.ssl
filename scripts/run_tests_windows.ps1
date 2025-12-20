Param(
  [switch]$IncludePerf = $false,
  [switch]$RunNetwork = $false
)

$ErrorActionPreference = "Continue"

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "fafafa.ssl Windows Test Runner" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "IncludePerf: $IncludePerf  RunNetwork: $RunNetwork" -ForegroundColor Gray
Write-Host

# Ensure FPC exists
if (-not (Get-Command fpc -ErrorAction SilentlyContinue)) {
  Write-Host "FreePascal (fpc) not found in PATH" -ForegroundColor Yellow
}

# Project paths
$Root = (Get-Location).Path
$TestsDir = Join-Path $Root "tests"
$BinDir = Join-Path $TestsDir "bin-win"
New-Item -ItemType Directory -Force -Path $BinDir | Out-Null

# Unit paths
$UnitPaths = @(
  "-Fusrc",
  "-Futests",
  "-Fusrc\\openssl",
  "-Fusrc\\winssl"
)

function Compile-And-Run($pasPath) {
  $name = [IO.Path]::GetFileNameWithoutExtension($pasPath)
  $exePath = Join-Path $BinDir ("$name.exe")
  Write-Host "[BUILD] $name" -ForegroundColor Yellow
  & fpc @UnitPaths -FE:$BinDir $pasPath | Out-Null
  if ($LASTEXITCODE -ne 0 -or -not (Test-Path $exePath)) {
    Write-Host "  -> COMPILE ERROR" -ForegroundColor Red
    return $false
  }
  Write-Host "[RUN  ] $name" -ForegroundColor Yellow
  & $exePath
  return $LASTEXITCODE -eq 0
}

$ok = $true

# Optional skip list (basenames)
$skip = @{}
if (Test-Path 'scripts/skip_tests_windows.txt') {
  Get-Content 'scripts/skip_tests_windows.txt' | ForEach-Object {
    $n = $_.Trim()
    if ($n -ne '' -and -not $n.StartsWith('#')) { $skip[$n] = $true }
  }
}
function ShouldSkip($path) {
  $bn = [IO.Path]::GetFileName($path)
  return $skip.ContainsKey($bn)
}

# Always run unit-level WinSSL tests (no network)
$unitTests = @(
  "tests/test_winssl_lib_simple.pas",
  "tests/test_winssl_library_basic.pas",
  "tests/test_winssl_unit_comprehensive.pas",
  "tests/test_winssl_certificate_loading.pas"
)
foreach ($t in $unitTests) {
  if (Test-Path $t) {
    if (ShouldSkip $t) { Write-Host "[SKIP] $t" -ForegroundColor DarkYellow }
    else { $ok = (Compile-And-Run $t) -and $ok }
  }
}

# Network tests (optional)
if ($RunNetwork) {
  $env:FAFAFA_RUN_NETWORK_TESTS = "1"
  $netTests = @(
    "tests/test_winssl_integration_multi.pas",
    "tests/test_winssl_alpn_sni.pas",
    "tests/test_winssl_error_mapping_online.pas",
    "tests/test_winssl_session_resumption.pas",
    "tests/test_winssl_revocation_online.pas",
    "tests/test_winssl_hostname_mismatch_online.pas",
    "tests/test_cross_backend_consistency_contract.pas",
    "tests/test_cross_backend_errors_contract.pas",
    "tests/test_winssl_mtls_skeleton.pas",
    "tests/test_winssl_mtls_e2e_local.pas"
  )
  foreach ($t in $netTests) {
    if (Test-Path $t) {
      if (ShouldSkip $t) { Write-Host "[SKIP] $t" -ForegroundColor DarkYellow }
      else { $ok = (Compile-And-Run $t) -and $ok }
    }
  }
  if ($IncludePerf) {
    if (Test-Path "tests/test_winssl_performance.pas") {
      $ok = (Compile-And-Run "tests/test_winssl_performance.pas") -and $ok
    }
  }
}

if (-not $ok) { exit 1 } else { exit 0 }


