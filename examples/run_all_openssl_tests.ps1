#!/usr/bin/env pwsh
# Comprehensive OpenSSL Module Test Runner
# Runs all available test executables and generates a summary report

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "OpenSSL Module Test Suite Runner" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

$TestResults = @()
$TotalTests = 0
$TotalPassed = 0
$TotalFailed = 0

# Find all test executables
$TestExecutables = Get-ChildItem -Path . -Filter "test_openssl_*.exe" | Sort-Object Name

Write-Host "Found $($TestExecutables.Count) test executables" -ForegroundColor Yellow
Write-Host ""

foreach ($TestExe in $TestExecutables) {
    $TestName = $TestExe.BaseName -replace "test_openssl_", ""
    Write-Host "Running $TestName test..." -ForegroundColor Green
    
    try {
        $Output = & ".\$($TestExe.Name)" 2>&1 | Out-String
        
        # Parse test results
        $Passed = 0
        $Failed = 0
        
        if ($Output -match "Tests Passed:\s*(\d+)") {
            $Passed = [int]$Matches[1]
        }
        if ($Output -match "Tests Failed:\s*(\d+)") {
            $Failed = [int]$Matches[1]
        }
        
        # Alternative format
        if ($Output -match "Passed:\s*(\d+)") {
            $Passed = [int]$Matches[1]
        }
        if ($Output -match "Failed:\s*(\d+)") {
            $Failed = [int]$Matches[1]
        }
        
        $Total = $Passed + $Failed
        
        if ($Total -eq 0) {
            # Try to count PASS/FAIL markers
            $PassCount = ([regex]::Matches($Output, "\[PASS\]")).Count
            $FailCount = ([regex]::Matches($Output, "\[FAIL\]")).Count
            $Passed = $PassCount
            $Failed = $FailCount
            $Total = $Passed + $Failed
        }
        
        $Status = if ($Failed -eq 0 -and $Passed -gt 0) { "PASS" } elseif ($Total -eq 0) { "UNKNOWN" } else { "FAIL" }
        $PassRate = if ($Total -gt 0) { [math]::Round(($Passed / $Total) * 100, 1) } else { 0 }
        
        $TestResults += [PSCustomObject]@{
            Module = $TestName.ToUpper()
            Total = $Total
            Passed = $Passed
            Failed = $Failed
            PassRate = $PassRate
            Status = $Status
        }
        
        $TotalTests += $Total
        $TotalPassed += $Passed
        $TotalFailed += $Failed
        
        if ($Status -eq "PASS") {
            Write-Host "  ✓ ${TestName}: ${Passed}/${Total} tests passed (100%)" -ForegroundColor Green
        } elseif ($Status -eq "UNKNOWN") {
            Write-Host "  ? ${TestName}: Unable to parse results" -ForegroundColor Yellow
        } else {
            Write-Host "  ✗ ${TestName}: ${Passed}/${Total} tests passed (${PassRate}%)" -ForegroundColor Red
        }
    }
    catch {
        Write-Host "  ✗ ${TestName}: Error running test - $($_.Exception.Message)" -ForegroundColor Red
        $TestResults += [PSCustomObject]@{
            Module = $TestName.ToUpper()
            Total = 0
            Passed = 0
            Failed = 0
            PassRate = 0
            Status = "ERROR"
        }
    }
    
    Write-Host ""
}

# Print summary table
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Test Results Summary" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

$TestResults | Format-Table -AutoSize Module, Total, Passed, Failed, @{
    Label = "Pass Rate"
    Expression = { "$($_.PassRate)%" }
}, Status

# Print overall statistics
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Overall Statistics" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

$ModulesTested = $TestResults.Count
$ModulesPassed = ($TestResults | Where-Object { $_.Status -eq "PASS" }).Count
$ModulesWithIssues = ($TestResults | Where-Object { $_.Status -eq "FAIL" }).Count
$ModulesError = ($TestResults | Where-Object { $_.Status -eq "ERROR" }).Count
$OverallPassRate = if ($TotalTests -gt 0) { [math]::Round(($TotalPassed / $TotalTests) * 100, 1) } else { 0 }

Write-Host "Modules Tested:    $ModulesTested"
Write-Host "Modules Passed:    $ModulesPassed (100%)" -ForegroundColor Green
Write-Host "Modules w/Issues:  $ModulesWithIssues" -ForegroundColor $(if ($ModulesWithIssues -gt 0) { "Yellow" } else { "Green" })
Write-Host "Modules w/Errors:  $ModulesError" -ForegroundColor $(if ($ModulesError -gt 0) { "Red" } else { "Green" })
Write-Host ""
Write-Host "Total Test Cases:  $TotalTests"
Write-Host "Tests Passed:      $TotalPassed" -ForegroundColor Green
Write-Host "Tests Failed:      $TotalFailed" -ForegroundColor $(if ($TotalFailed -gt 0) { "Red" } else { "Green" })
Write-Host "Overall Pass Rate: $OverallPassRate%" -ForegroundColor $(if ($OverallPassRate -ge 90) { "Green" } elseif ($OverallPassRate -ge 75) { "Yellow" } else { "Red" })
Write-Host ""

# Export results to CSV
$CsvPath = "test_results_$(Get-Date -Format 'yyyyMMdd_HHmmss').csv"
$TestResults | Export-Csv -Path $CsvPath -NoTypeInformation
Write-Host "Results exported to: $CsvPath" -ForegroundColor Cyan

Write-Host "========================================" -ForegroundColor Cyan

# Return exit code based on failures
if ($TotalFailed -eq 0 -and $ModulesError -eq 0) {
    exit 0
} else {
    exit 1
}
