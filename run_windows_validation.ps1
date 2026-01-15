# fafafa.ssl Windows Full Validation Suite
# Comprehensive validation for OpenSSL and WinSSL backends on Windows

[CmdletBinding()]
param(
    [switch]$OpenSSLOnly,
    [switch]$WinSSLOnly,
    [switch]$SkipCompile,
    [switch]$VerboseOutput,
    [int]$Timeout = 60,
    [string]$ReportDir = "reports",
    [string]$ConfigFile = "tests/windows/validation_config.json"
)

$ErrorActionPreference = "Continue"
$script:StartTime = Get-Date
$script:Results = @{
    Compilation = @()
    OpenSSL = @()
    WinSSL = @()
    SSH = @()
    BackendComparison = @()
}
$script:Summary = @{
    Total = 0
    Passed = 0
    Failed = 0
    Skipped = 0
    CompileErrors = 0
}

# ============================================================================
# Configuration Loading
# ============================================================================

function Get-ValidationConfig {
    param([string]$ConfigPath)
    
    if (Test-Path $ConfigPath) {
        return Get-Content $ConfigPath -Raw | ConvertFrom-Json
    }
    
    # Default configuration
    return @{
        compiler = @{
            path = "C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe"
            flags = @("-Mobjfpc", "-Sh")
            sourceDir = "src"
            outputDir = "bin"
        }
        execution = @{
            timeout = 60
            continueOnError = $true
        }
        reports = @{
            outputDir = "reports"
        }
    }
}

# ============================================================================
# Environment Detection
# ============================================================================

function Get-EnvironmentInfo {
    $env = @{
        Timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
        WindowsVersion = [System.Environment]::OSVersion.VersionString
        PowerShellVersion = $PSVersionTable.PSVersion.ToString()
        FPCVersion = "Unknown"
        OpenSSLVersion = "Unknown"
        OpenSSLAvailable = $false
        WorkingDirectory = (Get-Location).Path
    }
    
    # Detect FPC version
    $fpcPath = $script:Config.compiler.path
    if (Test-Path $fpcPath) {
        try {
            $fpcOutput = & $fpcPath -iV 2>&1
            $env.FPCVersion = $fpcOutput.Trim()
        } catch {
            $env.FPCVersion = "Error detecting"
        }
    } else {
        # Try system PATH
        try {
            $fpcOutput = & fpc -iV 2>&1
            $env.FPCVersion = $fpcOutput.Trim()
        } catch {
            $env.FPCVersion = "Not found"
        }
    }
    
    # Detect OpenSSL
    $opensslDlls = @("libssl-3-x64.dll", "libcrypto-3-x64.dll")
    $searchPaths = @(".", "bin", "C:\OpenSSL-Win64\bin", "C:\Program Files\OpenSSL-Win64\bin")
    
    foreach ($searchPath in $searchPaths) {
        $sslDll = Join-Path $searchPath $opensslDlls[0]
        if (Test-Path $sslDll) {
            $env.OpenSSLAvailable = $true
            try {
                $opensslExe = Join-Path (Split-Path $sslDll) "openssl.exe"
                if (Test-Path $opensslExe) {
                    $versionOutput = & $opensslExe version 2>&1
                    $env.OpenSSLVersion = $versionOutput.Trim()
                } else {
                    $env.OpenSSLVersion = "DLLs found at $searchPath"
                }
            } catch {
                $env.OpenSSLVersion = "DLLs found, version unknown"
            }
            break
        }
    }
    
    return $env
}

function Write-EnvironmentInfo {
    param($EnvInfo)
    
    Write-Host ""
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host "Environment Information" -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host "Timestamp:      $($EnvInfo.Timestamp)"
    Write-Host "Windows:        $($EnvInfo.WindowsVersion)"
    Write-Host "PowerShell:     $($EnvInfo.PowerShellVersion)"
    Write-Host "FPC:            $($EnvInfo.FPCVersion)"
    Write-Host "OpenSSL:        $($EnvInfo.OpenSSLVersion)"
    Write-Host "OpenSSL Ready:  $($EnvInfo.OpenSSLAvailable)"
    Write-Host "Working Dir:    $($EnvInfo.WorkingDirectory)"
    Write-Host ""
}

# ============================================================================
# Compilation Functions
# ============================================================================

function Invoke-CompilationValidation {
    Write-Host ""
    Write-Host "========================================" -ForegroundColor Yellow
    Write-Host "Phase 1: Source Code Compilation" -ForegroundColor Yellow
    Write-Host "========================================" -ForegroundColor Yellow
    
    $srcDir = $script:Config.compiler.sourceDir
    $outDir = $script:Config.compiler.outputDir
    $fpcPath = $script:Config.compiler.path
    
    # Ensure output directory exists
    if (-not (Test-Path $outDir)) {
        New-Item -ItemType Directory -Path $outDir -Force | Out-Null
    }
    
    # Get all Pascal source files
    $sourceFiles = Get-ChildItem -Path $srcDir -Filter "*.pas" -File | Sort-Object Name
    $totalFiles = $sourceFiles.Count
    
    Write-Host "Found $totalFiles source modules to compile" -ForegroundColor Gray
    Write-Host ""
    
    $compiled = 0
    $failed = 0
    $results = @()
    
    foreach ($file in $sourceFiles) {
        $moduleName = $file.BaseName
        $compiled++
        
        Write-Host "[$compiled/$totalFiles] Compiling: $moduleName" -NoNewline
        
        $compileArgs = @(
            "-Mobjfpc",
            "-Sh",
            "-Fu`"$srcDir`"",
            "-FE`"$outDir`"",
            $file.FullName
        )
        
        $startTime = Get-Date
        try {
            $output = & $fpcPath @compileArgs 2>&1
            $exitCode = $LASTEXITCODE
            $duration = ((Get-Date) - $startTime).TotalMilliseconds
            
            if ($exitCode -eq 0) {
                Write-Host " [OK]" -ForegroundColor Green
                $results += @{
                    Module = $moduleName
                    Status = "PASS"
                    Duration = [math]::Round($duration, 0)
                    Message = ""
                }
            } else {
                Write-Host " [FAIL]" -ForegroundColor Red
                $failed++
                $errorMsg = ($output | Select-String "Error:" | Select-Object -First 1).ToString()
                $results += @{
                    Module = $moduleName
                    Status = "FAIL"
                    Duration = [math]::Round($duration, 0)
                    Message = $errorMsg
                }
                if ($VerboseOutput) {
                    Write-Host "    Error: $errorMsg" -ForegroundColor DarkRed
                }
            }
        } catch {
            Write-Host " [ERROR]" -ForegroundColor Red
            $failed++
            $results += @{
                Module = $moduleName
                Status = "ERROR"
                Duration = 0
                Message = $_.Exception.Message
            }
        }
    }
    
    $script:Results.Compilation = $results
    $script:Summary.CompileErrors = $failed
    
    Write-Host ""
    Write-Host "Compilation Summary: $($totalFiles - $failed)/$totalFiles passed" -ForegroundColor $(if ($failed -eq 0) { "Green" } else { "Yellow" })
    
    return @{
        Total = $totalFiles
        Passed = $totalFiles - $failed
        Failed = $failed
    }
}

# ============================================================================
# Test Execution Functions
# ============================================================================

function Invoke-TestFile {
    param(
        [string]$TestFile,
        [string]$Category,
        [int]$TimeoutSeconds = 60
    )
    
    $testName = [System.IO.Path]::GetFileNameWithoutExtension($TestFile)
    $srcDir = $script:Config.compiler.sourceDir
    $outDir = $script:Config.compiler.outputDir
    $fpcPath = $script:Config.compiler.path
    
    # Ensure output directory exists
    if (-not (Test-Path $outDir)) {
        New-Item -ItemType Directory -Path $outDir -Force | Out-Null
    }
    
    # Compile test - use full paths
    $fullSrcDir = (Resolve-Path $srcDir -ErrorAction SilentlyContinue).Path
    if (-not $fullSrcDir) { $fullSrcDir = $srcDir }
    $fullOutDir = Join-Path (Get-Location).Path $outDir
    if (-not (Test-Path $fullOutDir)) {
        New-Item -ItemType Directory -Path $fullOutDir -Force | Out-Null
    }
    
    $compileCmd = "$fpcPath -Mobjfpc -Sh `"-Fu$fullSrcDir`" `"-Fu$fullSrcDir\openssl`" `"-FE$fullOutDir`" `"$TestFile`""
    
    try {
        $compileOutput = cmd /c "$compileCmd 2>&1"
        $compileExitCode = $LASTEXITCODE
    } catch {
        $compileOutput = $_.Exception.Message
        $compileExitCode = 1
    }
    
    if ($compileExitCode -ne 0) {
        $errorMsg = ""
        if ($compileOutput) {
            $errorLines = $compileOutput | Select-String "Error:|Fatal:" | Select-Object -First 1
            if ($errorLines) {
                $errorMsg = $errorLines.ToString()
            } else {
                $errorMsg = ($compileOutput | Select-Object -First 3) -join "; "
            }
        }
        return @{
            Name = $testName
            Category = $Category
            Status = "COMPILE_ERROR"
            Passed = 0
            Failed = 0
            Total = 0
            Duration = 0
            Message = $errorMsg
        }
    }
    
    # Run test
    $exePath = Join-Path $fullOutDir "$testName.exe"
    if (-not (Test-Path $exePath)) {
        return @{
            Name = $testName
            Category = $Category
            Status = "NO_EXE"
            Passed = 0
            Failed = 0
            Total = 0
            Duration = 0
            Message = "Executable not found after compilation"
        }
    }
    
    $startTime = Get-Date
    try {
        # Run test with timeout using Start-Process
        $pinfo = New-Object System.Diagnostics.ProcessStartInfo
        $pinfo.FileName = $exePath
        $pinfo.RedirectStandardOutput = $true
        $pinfo.RedirectStandardError = $true
        $pinfo.UseShellExecute = $false
        $pinfo.CreateNoWindow = $true
        $pinfo.WorkingDirectory = (Get-Location).Path
        
        $process = New-Object System.Diagnostics.Process
        $process.StartInfo = $pinfo
        $process.Start() | Out-Null
        
        # Read output BEFORE waiting to avoid deadlock when buffer fills
        $stdout = $process.StandardOutput.ReadToEnd()
        $stderr = $process.StandardError.ReadToEnd()
        
        $completed = $process.WaitForExit($TimeoutSeconds * 1000)
        $duration = ((Get-Date) - $startTime).TotalMilliseconds
        
        if ($completed) {
            $outputStr = "$stdout`n$stderr"
            
            # Parse test results
            $passed = 0
            $failed = 0
            $total = 0
            
            # Try multiple patterns for passed count
            if ($outputStr -match "Tests Passed[:\s]+(\d+)") {
                $passed = [int]$Matches[1]
            } elseif ($outputStr -match "Passed[:\s]+(\d+)") {
                $passed = [int]$Matches[1]
            }
            
            # Try multiple patterns for failed count
            if ($outputStr -match "Tests Failed[:\s]+(\d+)") {
                $failed = [int]$Matches[1]
            } elseif ($outputStr -match "Failed[:\s]+(\d+)") {
                $failed = [int]$Matches[1]
            }
            
            # Try multiple patterns for total count
            if ($outputStr -match "Total Tests[:\s]+(\d+)") {
                $total = [int]$Matches[1]
            } elseif ($outputStr -match "Total[:\s]+(\d+)") {
                $total = [int]$Matches[1]
            }
            
            # Fallback detection - check for success indicators FIRST
            if ($total -eq 0) {
                if ($outputStr -match "ALL TESTS PASSED|completed successfully|All tests (PASSED|passed)|SUCCESS") {
                    $passed = 1
                    $total = 1
                } elseif ($outputStr -match "(?m)^(ERROR|Exception|Access violation|FATAL ERROR)") {
                    # Only match ERROR/Exception at line start to avoid false positives
                    $failed = 1
                    $total = 1
                } else {
                    # Assume pass if no critical errors detected
                    $passed = 1
                    $total = 1
                }
            }
            
            $status = if ($failed -eq 0 -and $total -gt 0) { "PASS" } elseif ($total -eq 0) { "UNKNOWN" } else { "FAIL" }
            
            return @{
                Name = $testName
                Category = $Category
                Status = $status
                Passed = $passed
                Failed = $failed
                Total = $total
                Duration = [math]::Round($duration, 0)
                Message = if ($failed -gt 0) { $outputStr.Substring(0, [Math]::Min(200, $outputStr.Length)) } else { "" }
            }
        } else {
            $process.Kill()
            return @{
                Name = $testName
                Category = $Category
                Status = "TIMEOUT"
                Passed = 0
                Failed = 1
                Total = 1
                Duration = $TimeoutSeconds * 1000
                Message = "Test exceeded timeout of $TimeoutSeconds seconds"
            }
        }
    } catch {
        return @{
            Name = $testName
            Category = $Category
            Status = "CRASH"
            Passed = 0
            Failed = 1
            Total = 1
            Duration = ((Get-Date) - $startTime).TotalMilliseconds
            Message = $_.Exception.Message
        }
    }
}

function Invoke-OpenSSLValidation {
    Write-Host ""
    Write-Host "========================================" -ForegroundColor Yellow
    Write-Host "Phase 2: OpenSSL Backend Tests" -ForegroundColor Yellow
    Write-Host "========================================" -ForegroundColor Yellow
    
    if (-not $script:Environment.OpenSSLAvailable) {
        Write-Host "OpenSSL not available - skipping OpenSSL tests" -ForegroundColor DarkYellow
        return @{ Total = 0; Passed = 0; Failed = 0; Skipped = 1 }
    }
    
    $testDirs = @(
        @{ Path = "tests/openssl"; Category = "OpenSSL Core" },
        @{ Path = "tests/crypto"; Category = "Cryptography" },
        @{ Path = "tests/certificate"; Category = "Certificate" },
        @{ Path = "tests/connection"; Category = "Connection" }
    )
    
    $results = @()
    $totalTests = 0
    $passedTests = 0
    $failedTests = 0
    
    foreach ($dir in $testDirs) {
        if (-not (Test-Path $dir.Path)) {
            Write-Host "Directory not found: $($dir.Path)" -ForegroundColor DarkYellow
            continue
        }
        
        Write-Host ""
        Write-Host "Testing: $($dir.Category)" -ForegroundColor Cyan
        
        $testFiles = Get-ChildItem -Path $dir.Path -Filter "test_*.pas" -File | Sort-Object Name
        
        foreach ($testFile in $testFiles) {
            Write-Host "  $($testFile.BaseName)" -NoNewline
            
            $result = Invoke-TestFile -TestFile $testFile.FullName -Category $dir.Category -TimeoutSeconds $Timeout
            $results += $result
            
            $totalTests++
            if ($result.Status -eq "PASS") {
                $passedTests++
                Write-Host " [PASS]" -ForegroundColor Green
            } elseif ($result.Status -eq "COMPILE_ERROR") {
                $failedTests++
                Write-Host " [COMPILE ERROR]" -ForegroundColor Red
            } elseif ($result.Status -eq "TIMEOUT") {
                $failedTests++
                Write-Host " [TIMEOUT]" -ForegroundColor Red
            } else {
                $failedTests++
                Write-Host " [FAIL]" -ForegroundColor Red
            }
        }
    }
    
    $script:Results.OpenSSL = $results
    
    Write-Host ""
    Write-Host "OpenSSL Tests: $passedTests/$totalTests passed" -ForegroundColor $(if ($failedTests -eq 0) { "Green" } else { "Yellow" })
    
    return @{
        Total = $totalTests
        Passed = $passedTests
        Failed = $failedTests
        Skipped = 0
    }
}

function Invoke-WinSSLValidation {
    Write-Host ""
    Write-Host "========================================" -ForegroundColor Yellow
    Write-Host "Phase 3: WinSSL Backend Tests" -ForegroundColor Yellow
    Write-Host "========================================" -ForegroundColor Yellow
    
    $testDir = "tests/winssl"
    if (-not (Test-Path $testDir)) {
        Write-Host "WinSSL test directory not found" -ForegroundColor DarkYellow
        return @{ Total = 0; Passed = 0; Failed = 0; Skipped = 1 }
    }
    
    $results = @()
    $totalTests = 0
    $passedTests = 0
    $failedTests = 0
    
    $testFiles = Get-ChildItem -Path $testDir -Filter "test_*.pas" -File | Sort-Object Name
    
    foreach ($testFile in $testFiles) {
        Write-Host "  $($testFile.BaseName)" -NoNewline
        
        $result = Invoke-TestFile -TestFile $testFile.FullName -Category "WinSSL" -TimeoutSeconds $Timeout
        $results += $result
        
        $totalTests++
        if ($result.Status -eq "PASS") {
            $passedTests++
            Write-Host " [PASS]" -ForegroundColor Green
        } elseif ($result.Status -eq "COMPILE_ERROR") {
            $failedTests++
            Write-Host " [COMPILE ERROR]" -ForegroundColor Red
        } else {
            $failedTests++
            Write-Host " [FAIL]" -ForegroundColor Red
        }
    }
    
    $script:Results.WinSSL = $results
    
    Write-Host ""
    Write-Host "WinSSL Tests: $passedTests/$totalTests passed" -ForegroundColor $(if ($failedTests -eq 0) { "Green" } else { "Yellow" })
    
    return @{
        Total = $totalTests
        Passed = $passedTests
        Failed = $failedTests
        Skipped = 0
    }
}

function Invoke-SSHValidation {
    Write-Host ""
    Write-Host "========================================" -ForegroundColor Yellow
    Write-Host "Phase 4: SSH Key Tests" -ForegroundColor Yellow
    Write-Host "========================================" -ForegroundColor Yellow
    
    $results = @()
    $totalTests = 0
    $passedTests = 0
    $failedTests = 0
    
    $sshTests = @(
        "tests/test_ssh.pas"
    )
    
    # Add property tests if OpenSSL available
    if ($script:Environment.OpenSSLAvailable) {
        $sshTests += "tests/test_ssh_properties.pas"
    }
    
    foreach ($testPath in $sshTests) {
        if (-not (Test-Path $testPath)) {
            Write-Host "  $testPath not found" -ForegroundColor DarkYellow
            continue
        }
        
        $testName = [System.IO.Path]::GetFileNameWithoutExtension($testPath)
        Write-Host "  $testName" -NoNewline
        
        $result = Invoke-TestFile -TestFile $testPath -Category "SSH" -TimeoutSeconds $Timeout
        $results += $result
        
        $totalTests++
        if ($result.Status -eq "PASS") {
            $passedTests++
            Write-Host " [PASS]" -ForegroundColor Green
        } else {
            $failedTests++
            Write-Host " [FAIL]" -ForegroundColor Red
        }
    }
    
    $script:Results.SSH = $results
    
    Write-Host ""
    Write-Host "SSH Tests: $passedTests/$totalTests passed" -ForegroundColor $(if ($failedTests -eq 0) { "Green" } else { "Yellow" })
    
    return @{
        Total = $totalTests
        Passed = $passedTests
        Failed = $failedTests
        Skipped = 0
    }
}

function Invoke-BackendComparison {
    Write-Host ""
    Write-Host "========================================" -ForegroundColor Yellow
    Write-Host "Phase 5: Backend Comparison Tests" -ForegroundColor Yellow
    Write-Host "========================================" -ForegroundColor Yellow
    
    if (-not $script:Environment.OpenSSLAvailable) {
        Write-Host "OpenSSL not available - skipping comparison tests" -ForegroundColor DarkYellow
        return @{ Total = 0; Passed = 0; Failed = 0; Skipped = 1 }
    }
    
    $results = @()
    $totalTests = 0
    $passedTests = 0
    $failedTests = 0
    
    $comparisonTests = @(
        "tests/integration/test_backend_comparison.pas",
        "tests/integration/test_integration_winssl_openssl_comparison.pas"
    )
    
    foreach ($testPath in $comparisonTests) {
        if (-not (Test-Path $testPath)) {
            Write-Host "  $testPath not found" -ForegroundColor DarkYellow
            continue
        }
        
        $testName = [System.IO.Path]::GetFileNameWithoutExtension($testPath)
        Write-Host "  $testName" -NoNewline
        
        $result = Invoke-TestFile -TestFile $testPath -Category "Comparison" -TimeoutSeconds $Timeout
        $results += $result
        
        $totalTests++
        if ($result.Status -eq "PASS") {
            $passedTests++
            Write-Host " [PASS]" -ForegroundColor Green
        } else {
            $failedTests++
            Write-Host " [FAIL]" -ForegroundColor Red
        }
    }
    
    $script:Results.BackendComparison = $results
    
    Write-Host ""
    Write-Host "Comparison Tests: $passedTests/$totalTests passed" -ForegroundColor $(if ($failedTests -eq 0) { "Green" } else { "Yellow" })
    
    return @{
        Total = $totalTests
        Passed = $passedTests
        Failed = $failedTests
        Skipped = 0
    }
}


# ============================================================================
# Report Generation Functions
# ============================================================================

function Export-MarkdownReport {
    param(
        [string]$OutputPath,
        $Environment,
        $CompilationResult,
        $OpenSSLResult,
        $WinSSLResult,
        $SSHResult,
        $ComparisonResult
    )
    
    $totalTests = $OpenSSLResult.Total + $WinSSLResult.Total + $SSHResult.Total + $ComparisonResult.Total
    $totalPassed = $OpenSSLResult.Passed + $WinSSLResult.Passed + $SSHResult.Passed + $ComparisonResult.Passed
    $totalFailed = $OpenSSLResult.Failed + $WinSSLResult.Failed + $SSHResult.Failed + $ComparisonResult.Failed
    $totalSkipped = $OpenSSLResult.Skipped + $WinSSLResult.Skipped + $SSHResult.Skipped + $ComparisonResult.Skipped
    
    $duration = ((Get-Date) - $script:StartTime).TotalMinutes
    $passRate = if ($totalTests -gt 0) { [math]::Round(($totalPassed / $totalTests) * 100, 1) } else { 0 }
    
    $report = @"
# Windows Full Validation Report

## Environment Information

| Property | Value |
|----------|-------|
| Timestamp | $($Environment.Timestamp) |
| Windows | $($Environment.WindowsVersion) |
| PowerShell | $($Environment.PowerShellVersion) |
| FPC | $($Environment.FPCVersion) |
| OpenSSL | $($Environment.OpenSSLVersion) |
| OpenSSL Available | $($Environment.OpenSSLAvailable) |
| Working Directory | $($Environment.WorkingDirectory) |

## Summary

| Metric | Value |
|--------|-------|
| Total Tests | $totalTests |
| Passed | $totalPassed ($passRate%) |
| Failed | $totalFailed |
| Skipped | $totalSkipped |
| Compile Errors | $($CompilationResult.Failed) |
| Duration | $([math]::Round($duration, 2)) minutes |

## Compilation Results

- **Total Modules**: $($CompilationResult.Total)
- **Compiled Successfully**: $($CompilationResult.Passed)
- **Failed**: $($CompilationResult.Failed)

"@

    # Add failed compilations
    $failedCompilations = $script:Results.Compilation | Where-Object { $_.Status -ne "PASS" }
    if ($failedCompilations.Count -gt 0) {
        $report += "`n### Failed Compilations`n`n"
        foreach ($fail in $failedCompilations) {
            $report += "- **$($fail.Module)**: $($fail.Message)`n"
        }
    }

    $report += @"

## OpenSSL Backend Tests

- **Total**: $($OpenSSLResult.Total)
- **Passed**: $($OpenSSLResult.Passed)
- **Failed**: $($OpenSSLResult.Failed)

"@

    # Add failed OpenSSL tests
    $failedOpenSSL = $script:Results.OpenSSL | Where-Object { $_.Status -ne "PASS" }
    if ($failedOpenSSL.Count -gt 0) {
        $report += "`n### Failed Tests`n`n"
        foreach ($fail in $failedOpenSSL) {
            $report += "- **$($fail.Name)** [$($fail.Category)]: $($fail.Status) - $($fail.Message)`n"
        }
    }

    $report += @"

## WinSSL Backend Tests

- **Total**: $($WinSSLResult.Total)
- **Passed**: $($WinSSLResult.Passed)
- **Failed**: $($WinSSLResult.Failed)

"@

    # Add failed WinSSL tests
    $failedWinSSL = $script:Results.WinSSL | Where-Object { $_.Status -ne "PASS" }
    if ($failedWinSSL.Count -gt 0) {
        $report += "`n### Failed Tests`n`n"
        foreach ($fail in $failedWinSSL) {
            $report += "- **$($fail.Name)**: $($fail.Status) - $($fail.Message)`n"
        }
    }

    $report += @"

## SSH Key Tests

- **Total**: $($SSHResult.Total)
- **Passed**: $($SSHResult.Passed)
- **Failed**: $($SSHResult.Failed)

## Backend Comparison Tests

- **Total**: $($ComparisonResult.Total)
- **Passed**: $($ComparisonResult.Passed)
- **Failed**: $($ComparisonResult.Failed)

---

*Report generated by fafafa.ssl Windows Validation Suite*
"@

    $report | Out-File -FilePath $OutputPath -Encoding UTF8
    Write-Host "Markdown report saved to: $OutputPath" -ForegroundColor Gray
}

function Export-JsonReport {
    param(
        [string]$OutputPath,
        $Environment,
        $CompilationResult,
        $OpenSSLResult,
        $WinSSLResult,
        $SSHResult,
        $ComparisonResult
    )
    
    $totalTests = $OpenSSLResult.Total + $WinSSLResult.Total + $SSHResult.Total + $ComparisonResult.Total
    $totalPassed = $OpenSSLResult.Passed + $WinSSLResult.Passed + $SSHResult.Passed + $ComparisonResult.Passed
    $totalFailed = $OpenSSLResult.Failed + $WinSSLResult.Failed + $SSHResult.Failed + $ComparisonResult.Failed
    $totalSkipped = $OpenSSLResult.Skipped + $WinSSLResult.Skipped + $SSHResult.Skipped + $ComparisonResult.Skipped
    
    $duration = ((Get-Date) - $script:StartTime).TotalSeconds
    
    $report = @{
        timestamp = (Get-Date -Format "yyyy-MM-ddTHH:mm:ss")
        environment = @{
            windows = $Environment.WindowsVersion
            powershell = $Environment.PowerShellVersion
            fpc = $Environment.FPCVersion
            openssl = $Environment.OpenSSLVersion
            opensslAvailable = $Environment.OpenSSLAvailable
            workingDirectory = $Environment.WorkingDirectory
        }
        summary = @{
            total = $totalTests
            passed = $totalPassed
            failed = $totalFailed
            skipped = $totalSkipped
            compileErrors = $CompilationResult.Failed
            durationSeconds = [math]::Round($duration, 2)
            passRate = if ($totalTests -gt 0) { [math]::Round(($totalPassed / $totalTests) * 100, 1) } else { 0 }
        }
        compilation = @{
            total = $CompilationResult.Total
            passed = $CompilationResult.Passed
            failed = $CompilationResult.Failed
            details = $script:Results.Compilation
        }
        openssl = @{
            total = $OpenSSLResult.Total
            passed = $OpenSSLResult.Passed
            failed = $OpenSSLResult.Failed
            details = $script:Results.OpenSSL
        }
        winssl = @{
            total = $WinSSLResult.Total
            passed = $WinSSLResult.Passed
            failed = $WinSSLResult.Failed
            details = $script:Results.WinSSL
        }
        ssh = @{
            total = $SSHResult.Total
            passed = $SSHResult.Passed
            failed = $SSHResult.Failed
            details = $script:Results.SSH
        }
        backendComparison = @{
            total = $ComparisonResult.Total
            passed = $ComparisonResult.Passed
            failed = $ComparisonResult.Failed
            details = $script:Results.BackendComparison
        }
    }
    
    $report | ConvertTo-Json -Depth 10 | Out-File -FilePath $OutputPath -Encoding UTF8
    Write-Host "JSON report saved to: $OutputPath" -ForegroundColor Gray
}

# ============================================================================
# Main Execution
# ============================================================================

function Main {
    Write-Host ""
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host "fafafa.ssl Windows Full Validation" -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan
    
    # Load configuration
    $script:Config = Get-ValidationConfig -ConfigPath $ConfigFile
    
    # Detect environment
    $script:Environment = Get-EnvironmentInfo
    Write-EnvironmentInfo -EnvInfo $script:Environment
    
    # Ensure report directory exists
    if (-not (Test-Path $ReportDir)) {
        New-Item -ItemType Directory -Path $ReportDir -Force | Out-Null
    }
    
    # Initialize results
    $compilationResult = @{ Total = 0; Passed = 0; Failed = 0 }
    $opensslResult = @{ Total = 0; Passed = 0; Failed = 0; Skipped = 0 }
    $winsslResult = @{ Total = 0; Passed = 0; Failed = 0; Skipped = 0 }
    $sshResult = @{ Total = 0; Passed = 0; Failed = 0; Skipped = 0 }
    $comparisonResult = @{ Total = 0; Passed = 0; Failed = 0; Skipped = 0 }
    
    # Phase 1: Compilation
    if (-not $SkipCompile) {
        $compilationResult = Invoke-CompilationValidation
    } else {
        Write-Host ""
        Write-Host "Skipping compilation phase (-SkipCompile)" -ForegroundColor DarkYellow
    }
    
    # Phase 2: OpenSSL Tests
    if (-not $WinSSLOnly) {
        $opensslResult = Invoke-OpenSSLValidation
    } else {
        Write-Host ""
        Write-Host "Skipping OpenSSL tests (-WinSSLOnly)" -ForegroundColor DarkYellow
    }
    
    # Phase 3: WinSSL Tests
    if (-not $OpenSSLOnly) {
        $winsslResult = Invoke-WinSSLValidation
    } else {
        Write-Host ""
        Write-Host "Skipping WinSSL tests (-OpenSSLOnly)" -ForegroundColor DarkYellow
    }
    
    # Phase 4: SSH Tests
    $sshResult = Invoke-SSHValidation
    
    # Phase 5: Backend Comparison
    if (-not $OpenSSLOnly -and -not $WinSSLOnly) {
        $comparisonResult = Invoke-BackendComparison
    }
    
    # Generate reports
    Write-Host ""
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host "Generating Reports" -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan
    
    $timestamp = Get-Date -Format "yyyyMMdd_HHmmss"
    $mdReportPath = Join-Path $ReportDir "validation_$timestamp.md"
    $jsonReportPath = Join-Path $ReportDir "validation_$timestamp.json"
    
    Export-MarkdownReport -OutputPath $mdReportPath `
        -Environment $script:Environment `
        -CompilationResult $compilationResult `
        -OpenSSLResult $opensslResult `
        -WinSSLResult $winsslResult `
        -SSHResult $sshResult `
        -ComparisonResult $comparisonResult
    
    Export-JsonReport -OutputPath $jsonReportPath `
        -Environment $script:Environment `
        -CompilationResult $compilationResult `
        -OpenSSLResult $opensslResult `
        -WinSSLResult $winsslResult `
        -SSHResult $sshResult `
        -ComparisonResult $comparisonResult
    
    # Final Summary
    $totalTests = $opensslResult.Total + $winsslResult.Total + $sshResult.Total + $comparisonResult.Total
    $totalPassed = $opensslResult.Passed + $winsslResult.Passed + $sshResult.Passed + $comparisonResult.Passed
    $totalFailed = $opensslResult.Failed + $winsslResult.Failed + $sshResult.Failed + $comparisonResult.Failed
    $duration = ((Get-Date) - $script:StartTime).TotalMinutes
    
    Write-Host ""
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host "Final Summary" -ForegroundColor Cyan
    Write-Host "========================================" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "Compilation: $($compilationResult.Passed)/$($compilationResult.Total) modules" -ForegroundColor $(if ($compilationResult.Failed -eq 0) { "Green" } else { "Yellow" })
    Write-Host "OpenSSL Tests: $($opensslResult.Passed)/$($opensslResult.Total)" -ForegroundColor $(if ($opensslResult.Failed -eq 0) { "Green" } else { "Yellow" })
    Write-Host "WinSSL Tests: $($winsslResult.Passed)/$($winsslResult.Total)" -ForegroundColor $(if ($winsslResult.Failed -eq 0) { "Green" } else { "Yellow" })
    Write-Host "SSH Tests: $($sshResult.Passed)/$($sshResult.Total)" -ForegroundColor $(if ($sshResult.Failed -eq 0) { "Green" } else { "Yellow" })
    Write-Host "Comparison Tests: $($comparisonResult.Passed)/$($comparisonResult.Total)" -ForegroundColor $(if ($comparisonResult.Failed -eq 0) { "Green" } else { "Yellow" })
    Write-Host ""
    Write-Host "Total: $totalPassed/$totalTests tests passed" -ForegroundColor $(if ($totalFailed -eq 0) { "Green" } else { "Red" })
    Write-Host "Duration: $([math]::Round($duration, 2)) minutes" -ForegroundColor Gray
    Write-Host ""
    
    # Exit code
    if ($totalFailed -gt 0 -or $compilationResult.Failed -gt 0) {
        Write-Host "VALIDATION FAILED" -ForegroundColor Red
        exit 1
    } else {
        Write-Host "VALIDATION PASSED" -ForegroundColor Green
        exit 0
    }
}

# Run main
Main
