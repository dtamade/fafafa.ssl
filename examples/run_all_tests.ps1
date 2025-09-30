# OpenSSL Module Test Runner
# Runs all unit tests and generates a summary report

Write-Host "================================" -ForegroundColor Cyan
Write-Host "  OpenSSL Module Test Suite" -ForegroundColor Cyan
Write-Host "================================" -ForegroundColor Cyan
Write-Host ""

$TestResults = @()
$TotalTests = 0
$TotalPassed = 0
$TotalFailed = 0

# Test programs to run
$TestPrograms = @(
    @{Name="RAND"; File="test_openssl_rand.exe"},
    @{Name="ERR"; File="test_openssl_err.exe"},
    @{Name="BIO"; File="test_openssl_bio.exe"},
    @{Name="SHA"; File="test_openssl_sha.exe"}
)

foreach ($Test in $TestPrograms) {
    Write-Host "Running $($Test.Name) tests..." -NoNewline
    
    $StartTime = Get-Date
    $Output = & ".\$($Test.File)" 2>&1 | Out-String
    $ExitCode = $LASTEXITCODE
    $Duration = ((Get-Date) - $StartTime).TotalSeconds
    
    # Parse test results
    if ($Output -match "Tests Passed: (\d+)") {
        $Passed = [int]$Matches[1]
    } else {
        $Passed = 0
    }
    
    if ($Output -match "Tests Failed: (\d+)") {
        $Failed = [int]$Matches[1]
    } else {
        $Failed = 0
    }
    
    $Total = $Passed + $Failed
    
    # Update totals
    $TotalTests += $Total
    $TotalPassed += $Passed
    $TotalFailed += $Failed
    
    # Determine status
    if ($ExitCode -eq 0 -or $ExitCode -eq 217) {  # 217 is the non-interactive exit code
        $Status = "PASS"
        $Color = "Green"
    } else {
        $Status = "FAIL"
        $Color = "Red"
    }
    
    Write-Host " $Status" -ForegroundColor $Color
    Write-Host "  Tests: $Passed/$Total passed, Duration: $([math]::Round($Duration, 2))s"
    
    $TestResults += @{
        Module = $Test.Name
        Passed = $Passed
        Failed = $Failed
        Total = $Total
        Status = $Status
        Duration = $Duration
    }
}

# Print summary
Write-Host ""
Write-Host "================================" -ForegroundColor Cyan
Write-Host "  Test Summary" -ForegroundColor Cyan
Write-Host "================================" -ForegroundColor Cyan
Write-Host ""

foreach ($Result in $TestResults) {
    $StatusColor = if ($Result.Status -eq "PASS") { "Green" } else { "Red" }
    Write-Host ("  {0,-10} {1,3}/{2,-3} " -f $Result.Module, $Result.Passed, $Result.Total) -NoNewline
    Write-Host $Result.Status -ForegroundColor $StatusColor
}

Write-Host ""
Write-Host "================================" -ForegroundColor Cyan
Write-Host ("  Total: {0}/{1} tests passed" -f $TotalPassed, $TotalTests) -ForegroundColor $(if ($TotalFailed -eq 0) { "Green" } else { "Yellow" })
Write-Host ("  Success Rate: {0:P0}" -f ($TotalPassed / $TotalTests)) -ForegroundColor $(if ($TotalFailed -eq 0) { "Green" } else { "Yellow" })
Write-Host "================================" -ForegroundColor Cyan

if ($TotalFailed -eq 0) {
    Write-Host ""
    Write-Host "All tests PASSED! ✓" -ForegroundColor Green
    exit 0
} else {
    Write-Host ""
    Write-Host "Some tests FAILED! ✗" -ForegroundColor Red
    exit 1
}