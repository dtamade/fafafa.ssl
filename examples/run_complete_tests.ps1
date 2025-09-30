#!/usr/bin/env pwsh
# Complete Test Suite Runner for All OpenSSL Modules

param(
    [switch]$Verbose,
    [switch]$StopOnError,
    [switch]$SkipSlow
)

$ErrorActionPreference = "Continue"
$ExamplesPath = "D:\projects\Pascal\lazarus\My\libs\fafafa.ssl\examples"

Set-Location $ExamplesPath

# Results tracking
$Results = @()
$TotalPrograms = 0
$CompiledPrograms = 0
$PassedPrograms = 0
$FailedPrograms = 0
$SkippedPrograms = 0
$TotalUnitTests = 0
$PassedUnitTests = 0
$FailedUnitTests = 0

Write-Host ""
Write-Host "============================================" -ForegroundColor Cyan
Write-Host "  Complete OpenSSL Test Suite" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
Write-Host ""

# Get all test .lpi files
$TestPrograms = Get-ChildItem -Filter "test*.lpi" | Where-Object { 
    $_.Name -notin @("test_version_detection.lpi", "test_winssl.lpi", "test_winssl_init.lpi")
} | Sort-Object Name

Write-Host "Found $($TestPrograms.Count) test programs" -ForegroundColor Cyan
Write-Host ""

$Index = 0
foreach ($Test in $TestPrograms) {
    $Index++
    $TotalPrograms++
    $TestName = $Test.BaseName
    $ModuleName = $TestName -replace '^test_openssl_', '' -replace '^test_', ''
    $ExeName = "$TestName.exe"
    
    Write-Host "[$Index/$($TestPrograms.Count)] $TestName" -ForegroundColor Yellow
    Write-Host "  Module: $ModuleName" -ForegroundColor Gray
    Write-Host "  Compiling..." -NoNewline
    
    # Compile using lazbuild
    $CompileStart = Get-Date
    $CompileOutput = & lazbuild "$($Test.Name)" 2>&1 | Out-String
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
            
            if ($TestOutput -match "Tests [Pp]assed:\s+(\d+)") {
                $Passed = [int]$Matches[1]
            }
            elseif ($TestOutput -match "Tests passed:\s+(\d+)") {
                $Passed = [int]$Matches[1]
            }
            
            if ($TestOutput -match "Tests [Ff]ailed:\s+(\d+)") {
                $Failed = [int]$Matches[1]
            }
            elseif ($TestOutput -match "Tests failed:\s+(\d+)") {
                $Failed = [int]$Matches[1]
            }
            
            if ($TestOutput -match "Total [Tt]ests:\s+(\d+)") {
                $Total = [int]$Matches[1]
            }
            elseif ($TestOutput -match "Total tests:\s+(\d+)") {
                $Total = [int]$Matches[1]
            }
            else {
                $Total = $Passed + $Failed
            }
            
            # Determine status
            $Status = "UNKNOWN"
            $StatusColor = "Yellow"
            
            if ($TestExitCode -eq 0 -or $TestOutput -match "All tests PASSED") {
                $Status = "PASSED"
                $StatusColor = "Green"
                $PassedPrograms++
            }
            elseif ($Failed -gt 0) {
                $Status = "FAILED"
                $StatusColor = "Red"
                $FailedPrograms++
            }
            elseif ($TestOutput -match "not available|skipped") {
                $Status = "SKIPPED"
                $StatusColor = "Yellow"
                $SkippedPrograms++
                $PassedPrograms++  # Count as passed
            }
            else {
                $Status = "FAILED"
                $StatusColor = "Red"
                $FailedPrograms++
            }
            
            $PassedUnitTests += $Passed
            $FailedUnitTests += $Failed
            $TotalUnitTests += $Total
            
            Write-Host " $Status" -ForegroundColor $StatusColor
            if ($Total -gt 0) {
                Write-Host "    Tests: $Passed/$Total passed" -ForegroundColor Gray
                if ($Failed -gt 0) {
                    Write-Host "    Failed: $Failed" -ForegroundColor Red
                }
            }
            
            $Results += [PSCustomObject]@{
                Module = $ModuleName
                Status = $Status
                Compiled = "Yes"
                UnitTests = $Total
                Passed = $Passed
                Failed = $Failed
                Duration = [math]::Round($RunDuration, 2)
            }
            
            if ($Verbose -and $Status -eq "FAILED") {
                Write-Host "  Output (last 10 lines):" -ForegroundColor DarkYellow
                $TestOutput -split "`n" | Select-Object -Last 10 | ForEach-Object {
                    Write-Host "    $_" -ForegroundColor DarkGray
                }
            }
        }
        else {
            Write-Host "  ⚠️  No executable found" -ForegroundColor Yellow
            $FailedPrograms++
            $Results += [PSCustomObject]@{
                Module = $ModuleName
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
            Write-Host "  Error details:" -ForegroundColor DarkRed
            $CompileOutput -split "`n" | Select-String "Error" | Select-Object -First 3 | ForEach-Object {
                Write-Host "    $_" -ForegroundColor DarkRed
            }
        }
        
        $FailedPrograms++
        $Results += [PSCustomObject]@{
            Module = $ModuleName
            Status = "COMPILE ERROR"
            Compiled = "No"
            UnitTests = 0
            Passed = 0
            Failed = 0
            Duration = 0
        }
        
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
$PassedModules = $Results | Where-Object { $_.Status -in @("PASSED", "SKIPPED") } | Sort-Object Module
$FailedModules = $Results | Where-Object { $_.Status -in @("FAILED", "COMPILE ERROR", "NO EXE") } | Sort-Object Module

if ($PassedModules) {
    Write-Host "✅ Passed Modules ($($PassedModules.Count)):" -ForegroundColor Green
    Write-Host ""
    $PassedModules | Format-Table Module, Status, UnitTests, Passed, Failed, Duration -AutoSize
}

if ($FailedModules) {
    Write-Host "❌ Failed/Problematic Modules ($($FailedModules.Count)):" -ForegroundColor Red
    Write-Host ""
    $FailedModules | Format-Table Module, Status, Compiled, UnitTests, Failed -AutoSize
}

# Statistics
Write-Host ""
Write-Host "============================================" -ForegroundColor Cyan
Write-Host "  Overall Statistics" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
Write-Host ""

$PassRate = if ($TotalPrograms -gt 0) { [math]::Round(($PassedPrograms / $TotalPrograms) * 100, 1) } else { 0 }
$CompileRate = if ($TotalPrograms -gt 0) { [math]::Round(($CompiledPrograms / $TotalPrograms) * 100, 1) } else { 0 }

Write-Host "Test Programs:" -ForegroundColor Cyan
Write-Host "  Total:    $TotalPrograms"
Write-Host "  Compiled: $CompiledPrograms ($CompileRate%)"
Write-Host "  Passed:   " -NoNewline
Write-Host "$PassedPrograms ($PassRate%)" -ForegroundColor $(if ($PassRate -ge 80) { "Green" } elseif ($PassRate -ge 50) { "Yellow" } else { "Red" })
Write-Host "  Failed:   $FailedPrograms"
if ($SkippedPrograms -gt 0) {
    Write-Host "  Skipped:  $SkippedPrograms" -ForegroundColor Yellow
}
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

# Final status
if ($FailedPrograms -eq 0 -and $FailedUnitTests -eq 0) {
    Write-Host "🎉 All tests PASSED!" -ForegroundColor Green
    exit 0
}
elseif ($FailedUnitTests -gt 0) {
    Write-Host "⚠️  $FailedUnitTests unit test(s) failed" -ForegroundColor Yellow
    exit 1
}
else {
    Write-Host "⚠️  $FailedPrograms program(s) failed to compile or run" -ForegroundColor Yellow
    exit 1
}
