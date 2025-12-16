# OpenSSL Pascal Bindings - Automated Test Runner
# Run all test programs and generate a report

$ErrorActionPreference = "Continue"

# Adjust these paths if your environment is different
$TestDir = ".\tests" 
# Note: Previous script pointed to examples, but we see 'tests' dir being used for test_new_api.
# Checking actual directory structure... the user has 'tests/test_new_api.pas'.
# But run_all_tests.ps1 says $TestDir = ...\examples. 
# I should probably update this to point to where tests actually ARE.
# Given I just ran ./tests/test_new_api, I'll use .\tests relative to root.
$SrcDir = ".\src"

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "OpenSSL Pascal Bindings Test Suite" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Find all test files
$TestFiles = Get-ChildItem -Path $TestDir -Filter "test_*.pas" | 
    Sort-Object Name

if ($TestFiles.Count -eq 0) {
    Write-Host "No tests found in $TestDir!" -ForegroundColor Red
    Exit
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
    
    # Compile with -dOPENSSL just in case, though factory defaults to it on Linux.
    $CompileOutput = & fpc `
        "-Fu$SrcDir" `
        "-dOPENSSL" `
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
    $ExePath = Join-Path $TestDir "$($TestFile.BaseName)" # Linux no extension
    if (-not (Test-Path $ExePath)) {
         $ExePath = Join-Path $TestDir "$($TestFile.BaseName).exe" # Windows extension
    }

    if (Test-Path $ExePath) {
        Write-Host "  Running..." -NoNewline -ForegroundColor Gray
        
        # On Linux, insure executable permission
        if ($IsLinux) {
            chmod +x $ExePath
        }

        $TestOutput = & $ExePath 2>&1
        
        # Parse output for test results
        $TestsPassed = 0
        $TestsFailed = 0
        $TestsTotal = 0
        
        $TestOutputStr = $TestOutput | Out-String
        
        if ($TestOutputStr -match "Tests Passed[:\s]+(\d+)") {
            $TestsPassed = [int]$Matches[1]
        }
        else {
             if ($TestOutputStr -match "Passed:\s+(\d+)") {
                $TestsPassed = [int]$Matches[1]
            }
        }

        if ($TestOutputStr -match "Tests Failed[:\s]+(\d+)") {
            $TestsFailed = [int]$Matches[1]
        }
        else {
             if ($TestOutputStr -match "Failed:\s+(\d+)") {
                $TestsFailed = [int]$Matches[1]
            }
        }

        if ($TestOutputStr -match "Total Tests[:\s]+(\d+)") {
            $TestsTotal = [int]$Matches[1]
        }
        else {
             if ($TestOutputStr -match "Total:\s+(\d+)") {
                $TestsTotal = [int]$Matches[1]
            }
        }
        
        # Fallback
        if ($TestsTotal -eq 0) {
             if ($TestOutputStr -match "All tests passed!") {
                  # In test_new_api, we output specific counts, but if parsing failing, assume success implies all passed?
                  # Actually test_new_api output format is:
                  # Test Summary:
                  #   Passed: 35
                  #   Failed: 0
                  #   Total:  35
                  # My regex above should catch this.
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
        
        # Clean up
        # Remove-Item $ExePath -ErrorAction SilentlyContinue
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
$ReportFile = Join-Path $TestDir "openssl_test_results.txt"
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
