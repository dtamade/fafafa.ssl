# quick_winssl_validation.ps1
# Quick WinSSL Validation Script
# Purpose: Fast smoke test for Windows environment validation
# Duration: ~2 minutes

[CmdletBinding()]
param(
    [switch]$SkipCertSetup = $false
)

$ErrorActionPreference = "Stop"
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8

Write-Host "================================================================================" -ForegroundColor Cyan
Write-Host " WinSSL Quick Validation Script" -ForegroundColor Cyan  
Write-Host " Purpose: Rapid smoke test for certificate loading (2025-10-28 implementation)" -ForegroundColor Cyan
Write-Host "================================================================================" -ForegroundColor Cyan
Write-Host ""

# Step 1: Check prerequisites
Write-Host "[1/4] Checking Prerequisites..." -ForegroundColor Yellow
Write-Host ""

# Check lazbuild
if (-not (Get-Command lazbuild -ErrorAction SilentlyContinue)) {
    Write-Host "  ✗ lazbuild not found!" -ForegroundColor Red
    Write-Host "  → Please install Lazarus IDE" -ForegroundColor Yellow
    exit 1
}
Write-Host "  ✓ lazbuild found" -ForegroundColor Green

# Check Windows version
$osVersion = [System.Environment]::OSVersion.Version
Write-Host "  ✓ Windows $($osVersion.Major).$($osVersion.Minor) Build $($osVersion.Build)" -ForegroundColor Green

# Step 2: Create test certificate (if needed)
Write-Host ""
Write-Host "[2/4] Checking Test Certificate..." -ForegroundColor Yellow  
Write-Host ""

if (-not $SkipCertSetup) {
    # Check if test certificate exists
    $testCert = Get-ChildItem Cert:\CurrentUser\My | Where-Object { $_.Subject -like "*CN=fafafa-ssl-test*" } | Select-Object -First 1
    
    if ($null -eq $testCert) {
        Write-Host "  → Creating test certificate..." -ForegroundColor Yellow
        try {
            $cert = New-SelfSignedCertificate `
                -Subject "CN=fafafa-ssl-test" `
                -CertStoreLocation "Cert:\CurrentUser\My" `
                -KeyExportPolicy Exportable `
                -NotAfter (Get-Date).AddYears(1) `
                -KeyAlgorithm RSA `
                -KeyLength 2048
            
            Write-Host "  ✓ Test certificate created: $($cert.Thumbprint)" -ForegroundColor Green
            Write-Host "    Subject: $($cert.Subject)" -ForegroundColor Gray
            Write-Host "    Issuer:  $($cert.Issuer)" -ForegroundColor Gray
            Write-Host "    Valid:   Self-signed certificate" -ForegroundColor Gray
        }
        catch {
            Write-Host "  ✗ Failed to create certificate: $($_.Exception.Message)" -ForegroundColor Red
            Write-Host "  → Run with -SkipCertSetup to skip certificate setup" -ForegroundColor Yellow
            exit 1
        }
    }
    else {
        Write-Host "  ✓ Test certificate already exists" -ForegroundColor Green
        Write-Host "    Thumbprint: $($testCert.Thumbprint)" -ForegroundColor Gray
    }
}
else {
    Write-Host "  ⚠ Skipping certificate setup (as requested)" -ForegroundColor Yellow
}

# Step 3: Compile certificate loading test
Write-Host ""
Write-Host "[3/4] Compiling Certificate Loading Test..." -ForegroundColor Yellow
Write-Host ""

$testLpi = "test_winssl_certificate_loading.lpi"
$testExe = "bin\test_winssl_certificate_loading.exe"

if (-not (Test-Path $testLpi)) {
    Write-Host "  ✗ Test project not found: $testLpi" -ForegroundColor Red
    exit 1
}

Write-Host "  → Compiling $testLpi..." -NoNewline
try {
    $output = & lazbuild $testLpi 2>&1
    if ($LASTEXITCODE -eq 0) {
        Write-Host " [OK]" -ForegroundColor Green
    }
    else {
        Write-Host " [FAILED]" -ForegroundColor Red
        Write-Host "  Compilation output:" -ForegroundColor Red
        Write-Host $output -ForegroundColor Gray
        exit 1
    }
}
catch {
    Write-Host " [ERROR]" -ForegroundColor Red
    Write-Host "  Exception: $($_.Exception.Message)" -ForegroundColor Red
    exit 1
}

# Step 4: Run test
Write-Host ""
Write-Host "[4/4] Running Certificate Loading Test..." -ForegroundColor Yellow
Write-Host ""

if (-not (Test-Path $testExe)) {
    Write-Host "  ✗ Test executable not found: $testExe" -ForegroundColor Red
    exit 1
}

Write-Host "  → Executing test..." -ForegroundColor Cyan
Write-Host "  " + ("-" * 78) -ForegroundColor Gray
Write-Host ""

$startTime = Get-Date
try {
    # Run test and capture output
    $testOutput = & ".\$testExe" 2>&1
    $exitCode = $LASTEXITCODE
    $duration = (Get-Date) - $startTime
    
    # Display output with colors
    $testOutput | ForEach-Object {
        if ($_ -match "PASS|✅|成功") {
            Write-Host "  $_" -ForegroundColor Green
        }
        elseif ($_ -match "FAIL|✗|失败|ERROR") {
            Write-Host "  $_" -ForegroundColor Red
        }
        elseif ($_ -match "===|---") {
            Write-Host "  $_" -ForegroundColor Cyan
        }
        elseif ($_ -match "NOTE|TIP") {
            Write-Host "  $_" -ForegroundColor Yellow
        }
        else {
            Write-Host "  $_" -ForegroundColor Gray
        }
    }
    
    Write-Host ""
    Write-Host "  " + ("-" * 78) -ForegroundColor Gray
    
    # Show result
    if ($exitCode -eq 0) {
        Write-Host ""
        Write-Host "  ✅ TEST PASSED! (Duration: $($duration.TotalSeconds.ToString("F2"))s)" -ForegroundColor Green
        Write-Host ""
        Write-Host "  Certificate loading feature is working correctly!" -ForegroundColor Green
        Write-Host ""
    }
    else {
        Write-Host ""
        Write-Host "  ✗ TEST FAILED! (Exit code: $exitCode, Duration: $($duration.TotalSeconds.ToString("F2"))s)" -ForegroundColor Red
        Write-Host ""
        Write-Host "  Please review the output above for details." -ForegroundColor Yellow
        Write-Host ""
    }
}
catch {
    Write-Host ""
    Write-Host "  ✗ TEST EXCEPTION: $($_.Exception.Message)" -ForegroundColor Red
    $exitCode = 1
}

# Summary
Write-Host "================================================================================" -ForegroundColor Cyan
Write-Host " Quick Validation Complete" -ForegroundColor Cyan
Write-Host "================================================================================" -ForegroundColor Cyan
Write-Host ""

if ($exitCode -eq 0) {
    Write-Host "✅ WinSSL certificate loading feature is FUNCTIONAL" -ForegroundColor Green
    Write-Host ""
    Write-Host "Next steps:" -ForegroundColor Cyan
    Write-Host "  1. Run full test suite: .\run_winssl_tests.ps1" -ForegroundColor Gray
    Write-Host "  2. Test real-world scenarios (HTTPS client/server)" -ForegroundColor Gray
    Write-Host "  3. Update documentation with test results" -ForegroundColor Gray
    Write-Host ""
}
else {
    Write-Host "❌ WinSSL certificate loading has ISSUES" -ForegroundColor Red
    Write-Host ""
    Write-Host "Action items:" -ForegroundColor Cyan
    Write-Host "  1. Review test output above" -ForegroundColor Gray
    Write-Host "  2. Fix identified issues" -ForegroundColor Gray
    Write-Host "  3. Re-run this script" -ForegroundColor Gray
    Write-Host ""
}

exit $exitCode
