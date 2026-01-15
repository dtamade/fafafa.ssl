# WinSSL Pascal Bindings - Automated Test Runner
# Run all test programs and generate a report

$ErrorActionPreference = "Continue"
# Adjust these paths if your environment is different
$TestDir = ".\examples"
$SrcDir = ".\src"

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "WinSSL Pascal Bindings Test Suite" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Find all test files (assuming test files are named test_winssl_*.pas or similar)
# If no specific winssl tests exist, we might reuse openssl tests but compiled with -dWINSSL?
# For now, let's assume we want to run the same tests but ensure they use the WinSSL backend.
# Or, if there are specific WinSSL tests, we filter for them.
# Based on project structure, it seems we might want to run 'test_openssl_*.pas' but force WinSSL usage?
# However, the factory should auto-detect.
# Let's look for test_winssl_*.pas first, if not found, maybe run generic tests.
# Given the context, let's assume we are looking for test files in examples dir.

$TestFiles = Get-ChildItem -Path $TestDir -Filter "test_*.pas" | 
    Where-Object { $_.Name -notmatch "(openssl|complete|simple|basic|load)" } |
    Sort-Object Name

# If no specific tests found, try to run the standard tests but we need to ensure WinSSL is used.
# The factory prefers WinSSL on Windows if available.

if ($TestFiles.Count -eq 0) {
    Write-Host "No specific WinSSL tests found. Running standard tests..." -ForegroundColor Yellow
    $TestFiles = Get-ChildItem -Path $TestDir -Filter "test_*.pas" | 
        Where-Object { $_.Name -match "test_ssl_" } |
        Sort-Object Name
}

$TotalTests = 0
$PassedTests = 0
$FailedTests = 0
$CompileErrors = 0
$Results = @()

foreach ($TestFile in $TestFiles) {
    $ModuleName = $TestFile.BaseName
    Write-Host "Testing: $ModuleName" -NoNewline -ForegroundColor Yellow
    Write-Host " ($($TestFile.Name))" -ForegroundColor Gray
    
    # Compile with -dWINSSL to force or ensure WinSSL specific paths if needed, 
    # though the code uses factory.
    # We add -dDEBUG for more info.
    $CompileOutput = & fpc `
        "-Fu$SrcDir" `
        "-dDEBUG" `
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
        Write-Host "  Running..." -NoNewline -ForegroundColor Gray
        $TestOutput = & $ExePath 2>&1
        
        # Parse output for test results
        $TestsPassed = 0
        $TestsFailed = 0
        $TestsTotal = 0
        
        $TestOutputStr = $TestOutput | Out-String
        
        # Simple parsing logic - adjust based on actual test output format
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
        
        Write-Host "`r  [$Status] Tests: $TestsTotal, Passed: $TestsPassed, Failed: $TestsFailed ($PassRate%)      " -ForegroundColor $Color
        
        $Results += [PSCustomObject]@{
            Module = $ModuleName
            Status = $Status
            Tests = $TestsTotal
            Passed = $TestsPassed
            Failed = $TestsFailed
            PassRate = "$PassRate%"
        }
        
        # Clean up exe
        Remove-Item $ExePath -ErrorAction SilentlyContinue
        Remove-Item (Join-Path $TestDir "$($TestFile.BaseName).o") -ErrorAction SilentlyContinue
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
$ReportFile = Join-Path $TestDir "winssl_test_results.txt"
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
