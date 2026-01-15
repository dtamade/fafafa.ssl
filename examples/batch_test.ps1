# OpenSSL 模块批量测试脚本
param(
    [string[]]$Tests = @()
)

$TotalPassed = 0
$TotalFailed = 0
$TestResults = @()

function Test-Module {
    param(
        [string]$ModuleName,
        [string]$TestFile
    )
    
    Write-Host "`n=== Testing $ModuleName ===" -ForegroundColor Cyan
    
    # Check if test file exists
    if (!(Test-Path $TestFile)) {
        Write-Host "Test file not found: $TestFile" -ForegroundColor Yellow
        return @{ Name = $ModuleName; Status = "Not Found"; Passed = 0; Failed = 0 }
    }
    
    # Check if .lpi exists, create if not
    $LpiFile = $TestFile -replace '\.(pas|lpr)$', '.lpi'
    if (!(Test-Path $LpiFile)) {
        Write-Host "Creating .lpi file for $ModuleName..." -ForegroundColor Yellow
        # Use existing lpi as template would be better, but skip for now
    }
    
    # Compile
    Write-Host "Compiling..." -NoNewline
    $CompileResult = lazbuild -B --quiet $LpiFile 2>&1
    if ($LASTEXITCODE -ne 0) {
        Write-Host " FAILED" -ForegroundColor Red
        Write-Host ($CompileResult | Select-String "Error")
        return @{ Name = $ModuleName; Status = "Compile Error"; Passed = 0; Failed = 0 }
    }
    Write-Host " OK" -ForegroundColor Green
    
    # Run test
    $ExeFile = $TestFile -replace '\.(pas|lpr)$', '.exe'
    Write-Host "Running test..." -NoNewline
    $TestOutput = & $ExeFile 2>&1
    $ExitCode = $LASTEXITCODE
    
    if ($ExitCode -eq 0) {
        # Parse test results
        $PassedMatch = $TestOutput | Select-String "Tests? passed:\s*(\d+)"
        $FailedMatch = $TestOutput | Select-String "Tests? failed:\s*(\d+)"
        
        $Passed = if ($PassedMatch) { [int]$PassedMatch.Matches[0].Groups[1].Value } else { 0 }
        $Failed = if ($FailedMatch) { [int]$FailedMatch.Matches[0].Groups[1].Value } else { 0 }
        
        Write-Host " PASSED ($Passed/$($Passed+$Failed))" -ForegroundColor Green
        
        return @{ 
            Name = $ModuleName
            Status = "Success"
            Passed = $Passed
            Failed = $Failed
        }
    } else {
        Write-Host " FAILED (exit code: $ExitCode)" -ForegroundColor Red
        return @{ Name = $ModuleName; Status = "Runtime Error"; Passed = 0; Failed = 1 }
    }
}

# 定义要测试的模块
$AllTests = @(
    @{ Name = "MD5"; File = "test_openssl_md5.pas" },
    @{ Name = "SHA3"; File = "test_openssl_sha3.pas" },
    @{ Name = "BLAKE2"; File = "test_openssl_blake2.pas" },
    @{ Name = "CMAC"; File = "test_openssl_cmac.pas" },
    @{ Name = "ChaCha20"; File = "test_openssl_chacha.pas" }
)

# 如果指定了测试列表，只运行这些测试
if ($Tests.Count -gt 0) {
    $AllTests = $AllTests | Where-Object { $Tests -contains $_.Name }
}

Write-Host "=====================================" -ForegroundColor Green
Write-Host "OpenSSL Batch Test Runner" -ForegroundColor Green
Write-Host "=====================================" -ForegroundColor Green

foreach ($Test in $AllTests) {
    $Result = Test-Module -ModuleName $Test.Name -TestFile $Test.File
    $TestResults += $Result
    $TotalPassed += $Result.Passed
    $TotalFailed += $Result.Failed
}

# 输出总结
Write-Host "`n=====================================" -ForegroundColor Green
Write-Host "Test Summary" -ForegroundColor Green
Write-Host "=====================================" -ForegroundColor Green

foreach ($Result in $TestResults) {
    $StatusColor = switch ($Result.Status) {
        "Success" { "Green" }
        "Not Found" { "Yellow" }
        default { "Red" }
    }
    Write-Host ("{0,-15} : {1,-15} ({2}/{3})" -f $Result.Name, $Result.Status, $Result.Passed, ($Result.Passed + $Result.Failed)) -ForegroundColor $StatusColor
}

Write-Host "`nTotal: $TotalPassed passed, $TotalFailed failed" -ForegroundColor $(if ($TotalFailed -eq 0) { "Green" } else { "Yellow" })