# OpenSSL Pascal Bindings - Automated Test Runner
# Run all test programs and generate a report

$ErrorActionPreference = "Continue"
$TestDir = "D:\projects\Pascal\lazarus\My\libs\fafafa.ssl\examples"
$SrcDir = "D:\projects\Pascal\lazarus\My\libs\fafafa.ssl\src"
$SrcOpenSSLDir = "D:\projects\Pascal\lazarus\My\libs\fafafa.ssl\src\openssl"

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "OpenSSL Pascal Bindings Test Suite" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Find all test files
$TestFiles = Get-ChildItem -Path $TestDir -Filter "test_openssl_*.pas" | 
    Where-Object { $_.Name -notmatch "(complete|simple|basic|load)" } |
    Sort-Object Name

$TotalTests = 0
$PassedTests = 0
$FailedTests = 0
$CompileErrors = 0
$Results = @()

foreach ($TestFile in $TestFiles) {
    $ModuleName = $TestFile.BaseName -replace "test_openssl_", ""
    Write-Host "Testing: $ModuleName" -NoNewline -ForegroundColor Yellow
    Write-Host " ($($TestFile.Name))" -ForegroundColor Gray
    
    # Compile
    $CompileOutput = & fpc `
        "-Fu$SrcDir" "-Fu$SrcOpenSSLDir" `
        "$($TestFile.FullName)" 2>&1
    
    if ($LASTEXITCODE -ne 0) {
        Write-Host "  [COMPILE ERROR]" -ForegroundColor Red
        $CompileErrors++
        $Results += [PSCustomObject]@{
            Module = $ModuleName
            Status = "COMPILE_ERROR"
            Tests = 0
            Passed = 0
            Failed = 0
            PassRate = "N/A"
        }
        # Show first few error lines
        $CompileOutput | Select-String "Error:" | Select-Object -First 3 | ForEach-Object {
            Write-Host "    $_" -ForegroundColor DarkRed
        }
        continue
    }
    
    # Run test
    $ExePath = Join-Path $TestDir "$($TestFile.BaseName).exe"
    if (Test-Path $ExePath) {
        $TestOutput = & $ExePath 2>&1
        
        # Parse output for test results
        $TestsPassed = 0
        $TestsFailed = 0
        $TestsTotal = 0
        
        $TestOutputStr = $TestOutput | Out-String
        if ($TestOutputStr -match "Tests Passed[:\s]+(\d+)") {
            $TestsPassed = [int]$Matches[1]
        }
        if ($TestOutputStr -match "Tests Failed[:\s]+(\d+)") {
            $TestsFailed = [int]$Matches[1]
        }
        if ($TestOutputStr -match "Total Tests[:\s]+(\d+)") {
            $TestsTotal = [int]$Matches[1]
        }
        
        # Fallback: check for "All tests PASSED" or similar
        if ($TestsTotal -eq 0) {
            if ($TestOutputStr -match "completed successfully|All tests (PASSED|passed)|SUCCESS") {
                $TestsPassed = 1
                $TestsTotal = 1
            }
        }
        
        $TotalTests += $TestsTotal
        $PassedTests += $TestsPassed
        $FailedTests += $TestsFailed
        
        $PassRate = if ($TestsTotal -gt 0) { 
            [math]::Round(($TestsPassed / $TestsTotal) * 100, 1) 
        } else { 
            0 
        }
        
        $Status = if ($TestsFailed -eq 0 -and $TestsTotal -gt 0) { "PASS" } else { "FAIL" }
        $Color = if ($Status -eq "PASS") { "Green" } else { "Red" }
        
        Write-Host "  [$Status] Tests: $TestsTotal, Passed: $TestsPassed, Failed: $TestsFailed ($PassRate%)" -ForegroundColor $Color
        
        $Results += [PSCustomObject]@{
            Module = $ModuleName
            Status = $Status
            Tests = $TestsTotal
            Passed = $TestsPassed
            Failed = $TestsFailed
            PassRate = "$PassRate%"
        }
    }
    else {
        Write-Host "  [ERROR] Executable not found" -ForegroundColor Red
    }
    
    Write-Host ""
}

# Summary
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "Test Summary" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

$Results | Format-Table -AutoSize

$ModulesTested = $Results.Count
$ModulesPassed = ($Results | Where-Object { $_.Status -eq "PASS" }).Count
$ModulesFailed = ($Results | Where-Object { $_.Status -eq "FAIL" }).Count

Write-Host "Modules Tested: $ModulesTested" -ForegroundColor Cyan
Write-Host "Modules Passed: $ModulesPassed" -ForegroundColor Green
Write-Host "Modules Failed: $ModulesFailed" -ForegroundColor $(if ($ModulesFailed -gt 0) { "Red" } else { "Green" })
Write-Host "Compile Errors: $CompileErrors" -ForegroundColor $(if ($CompileErrors -gt 0) { "Red" } else { "Green" })
Write-Host ""
Write-Host "Total Test Cases: $TotalTests" -ForegroundColor Cyan
Write-Host "Tests Passed: $PassedTests" -ForegroundColor Green
Write-Host "Tests Failed: $FailedTests" -ForegroundColor $(if ($FailedTests -gt 0) { "Red" } else { "Green" })

$OverallPassRate = if ($TotalTests -gt 0) { 
    [math]::Round(($PassedTests / $TotalTests) * 100, 1) 
} else { 
    0 
}
Write-Host "Overall Pass Rate: $OverallPassRate%" -ForegroundColor Cyan
Write-Host ""

# Save results to file
$ReportFile = Join-Path $TestDir "test_results.txt"
$Results | Format-Table -AutoSize | Out-File $ReportFile
Add-Content $ReportFile "`nSummary:"
Add-Content $ReportFile "Modules Tested: $ModulesTested"
Add-Content $ReportFile "Modules Passed: $ModulesPassed"
Add-Content $ReportFile "Modules Failed: $ModulesFailed"
Add-Content $ReportFile "Compile Errors: $CompileErrors"
Add-Content $ReportFile "Total Test Cases: $TotalTests"
Add-Content $ReportFile "Tests Passed: $PassedTests"
Add-Content $ReportFile "Tests Failed: $FailedTests"
Add-Content $ReportFile "Overall Pass Rate: $OverallPassRate%"

Write-Host "Report saved to: $ReportFile" -ForegroundColor Gray