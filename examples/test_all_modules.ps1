#!/usr/bin/env pwsh
# Comprehensive OpenSSL Module Test Runner
# Compiles and runs ALL test programs

param(
    [switch]$Verbose,
    [switch]$StopOnError
)

$ErrorActionPreference = "Continue"
$SourcePath = "D:\projects\Pascal\lazarus\My\libs\fafafa.ssl\src"

# Results tracking
$Results = @()
$TotalPrograms = 0
$CompiledPrograms = 0
$PassedPrograms = 0
$FailedPrograms = 0
$TotalUnitTests = 0
$PassedUnitTests = 0
$FailedUnitTests = 0

Write-Host ""
Write-Host "============================================" -ForegroundColor Cyan
Write-Host "  OpenSSL Complete Module Test Suite" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "Source Path: $SourcePath" -ForegroundColor Gray
Write-Host ""

# Get all test .lpr files
$TestPrograms = Get-ChildItem -Filter "test_openssl_*.lpr" | Sort-Object Name

Write-Host "Found $($TestPrograms.Count) test programs" -ForegroundColor Cyan
Write-Host ""

foreach ($Test in $TestPrograms) {
    $TotalPrograms++
    $TestName = $Test.BaseName
    $ExeName = "$TestName.exe"
    
    Write-Host "[$TotalPrograms/$($TestPrograms.Count)] $TestName" -ForegroundColor Yellow
    Write-Host "  Compiling..." -NoNewline
    
    # Compile
    $CompileStart = Get-Date
    $CompileOutput = & fpc $Test.Name "-Fu$SourcePath" -vew 2>&1
    $CompileDuration = ((Get-Date) - $CompileStart).TotalSeconds
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host " ✅ OK ($([math]::Round($CompileDuration, 2))s)" -ForegroundColor Green
        $CompiledPrograms++
        
        # Run test
        if (Test-Path $ExeName) {
            Write-Host "  Running..." -NoNewline
            
            $RunStart = Get-Date
            try {
                $TestOutput = & ".\$ExeName" 2>&1 | Out-String
                $TestExitCode = $LASTEXITCODE
                $RunDuration = ((Get-Date) - $RunStart).TotalSeconds
            }
            catch {
                $TestExitCode = -1
                $TestOutput = $_.Exception.Message
                $RunDuration = 0
            }
            
            # Parse results
            $Passed = 0
            $Failed = 0
            
            if ($TestOutput -match "Tests Passed:\s+(\d+)") {
                $Passed = [int]$Matches[1]
            }
            if ($TestOutput -match "Tests Failed:\s+(\d+)") {
                $Failed = [int]$Matches[1]
            }
            if ($TestOutput -match "Total Tests:\s+(\d+)") {
                $Total = [int]$Matches[1]
            }
            else {
                $Total = $Passed + $Failed
            }
            
            # Determine status
            $Status = "UNKNOWN"
            $StatusColor = "Yellow"
            
            if ($TestExitCode -eq 0) {
                $Status = "PASSED"
                $StatusColor = "Green"
                $PassedPrograms++
                $PassedUnitTests += $Passed
                $FailedUnitTests += $Failed
                $TotalUnitTests += $Total
            }
            elseif ($TestOutput -match "All tests PASSED") {
                $Status = "PASSED"
                $StatusColor = "Green"
                $PassedPrograms++
                $PassedUnitTests += $Passed
                $FailedUnitTests += $Failed
                $TotalUnitTests += $Total
            }
            else {
                $Status = "FAILED"
                $StatusColor = "Red"
                $FailedPrograms++
                
                if ($Verbose) {
                    Write-Host ""
                    Write-Host "  Output:" -ForegroundColor DarkYellow
                    $TestOutput -split "`n" | Select-Object -Last 10 | ForEach-Object {
                        Write-Host "    $_" -ForegroundColor DarkGray
                    }
                }
            }
            
            Write-Host " $Status" -ForegroundColor $StatusColor
            if ($Total -gt 0) {
                Write-Host "    Tests: $Passed/$Total passed" -ForegroundColor Gray
            }
            
            $Results += [PSCustomObject]@{
                Module = $TestName -replace '^test_openssl_', ''
                Status = $Status
                Compiled = "Yes"
                UnitTests = $Total
                Passed = $Passed
                Failed = $Failed
                Duration = [math]::Round($RunDuration, 2)
            }
            
            # Clean up
            Remove-Item $ExeName -ErrorAction SilentlyContinue
            Remove-Item "*.o", "*.ppu" -ErrorAction SilentlyContinue
        }
        else {
            Write-Host "  ⚠️  No executable" -ForegroundColor Yellow
            $Results += [PSCustomObject]@{
                Module = $TestName -replace '^test_openssl_', ''
                Status = "NO EXE"
                Compiled = "Yes"
                UnitTests = 0
                Passed = 0
                Failed = 0
                Duration = 0
            }
        }
    }
    else {
        Write-Host " ❌ COMPILE ERROR" -ForegroundColor Red
        
        if ($Verbose) {
            $CompileOutput | Select-Object -Last 5 | ForEach-Object {
                Write-Host "    $_" -ForegroundColor DarkRed
            }
        }
        
        $Results += [PSCustomObject]@{
            Module = $TestName -replace '^test_openssl_', ''
            Status = "COMPILE ERROR"
            Compiled = "No"
            UnitTests = 0
            Passed = 0
            Failed = 0
            Duration = 0
        }
        
        $FailedPrograms++
        
        if ($StopOnError) {
            Write-Host ""
            Write-Host "Stopping due to -StopOnError flag" -ForegroundColor Red
            break
        }
    }
    
    Write-Host ""
}

# Summary Report
Write-Host ""
Write-Host "============================================" -ForegroundColor Cyan
Write-Host "  Test Summary Report" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
Write-Host ""

# Group by status
$PassedModules = $Results | Where-Object { $_.Status -eq "PASSED" } | Sort-Object Module
$FailedModules = $Results | Where-Object { $_.Status -in @("FAILED", "COMPILE ERROR", "NO EXE") } | Sort-Object Module

if ($PassedModules) {
    Write-Host "✅ Passed Modules ($($PassedModules.Count)):" -ForegroundColor Green
    $PassedModules | Format-Table Module, UnitTests, Passed, Failed, Duration -AutoSize
}

if ($FailedModules) {
    Write-Host "❌ Failed/Problematic Modules ($($FailedModules.Count)):" -ForegroundColor Red
    $FailedModules | Format-Table Module, Status, Compiled -AutoSize
}

# Statistics
Write-Host ""
Write-Host "============================================" -ForegroundColor Cyan
Write-Host "  Overall Statistics" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
Write-Host ""

$PassRate = if ($TotalPrograms -gt 0) { [math]::Round(($PassedPrograms / $TotalPrograms) * 100, 1) } else { 0 }
$CompileRate = if ($TotalPrograms -gt 0) { [math]::Round(($CompiledPrograms / $TotalPrograms) * 100, 1) } else { 0 }

Write-Host "Programs:" -ForegroundColor Cyan
Write-Host "  Total:    $TotalPrograms"
Write-Host "  Compiled: $CompiledPrograms ($CompileRate%)"
Write-Host "  Passed:   " -NoNewline
Write-Host "$PassedPrograms ($PassRate%)" -ForegroundColor $(if ($PassRate -ge 80) { "Green" } elseif ($PassRate -ge 50) { "Yellow" } else { "Red" })
Write-Host "  Failed:   $FailedPrograms"
Write-Host ""

if ($TotalUnitTests -gt 0) {
    $UnitPassRate = [math]::Round(($PassedUnitTests / $TotalUnitTests) * 100, 1)
    Write-Host "Unit Tests:" -ForegroundColor Cyan
    Write-Host "  Total:  $TotalUnitTests"
    Write-Host "  Passed: " -NoNewline
    Write-Host "$PassedUnitTests ($UnitPassRate%)" -ForegroundColor $(if ($UnitPassRate -ge 95) { "Green" } elseif ($UnitPassRate -ge 80) { "Yellow" } else { "Red" })
    Write-Host "  Failed: $FailedUnitTests"
    Write-Host ""
}

# Exit code
Write-Host ""
if ($FailedPrograms -eq 0 -and $FailedUnitTests -eq 0) {
    Write-Host "🎉 All tests PASSED!" -ForegroundColor Green
    exit 0
}
else {
    Write-Host "⚠️  Some tests failed or had errors" -ForegroundColor Yellow
    exit 1
}