# OpenSSL Pascal 绑定 - 核心功能测试套件
# 编译并运行核心加密功能测试

param(
    [switch]$SkipCompile,
    [switch]$Verbose
)

Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  OpenSSL 核心功能测试套件" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

$baseDir = Split-Path -Parent $PSCommandPath
$srcDir = Join-Path (Split-Path -Parent $baseDir) "src"
$outDir = Join-Path $baseDir "bin"

# 确保输出目录存在
if (-not (Test-Path $outDir)) {
    New-Item -ItemType Directory -Path $outDir -Force | Out-Null
}

# 核心测试列表（按优先级排序）
$coreTests = @(
    # 加密算法
    "test_aes",
    "test_des", 
    "test_chacha20",
    "test_blowfish",
    "test_camellia",
    
    # 哈希函数
    "test_sha",
    "test_sha3_simple",
    "test_blake2",
    "test_sm3",
    
    # AEAD模式
    "test_aead_simple",
    "test_gcm_simple",
    
    # HMAC和MAC
    "test_hmac_comprehensive",
    "test_cmac_evp",
    
    # KDF
    "test_kdf_comprehensive",
    
    # 签名和验证
    "test_signature_comprehensive",
    "test_ecdsa_comprehensive",
    
    # 算法可用性
    "test_algorithm_availability"
)

$compiled = 0
$compileErrors = 0
$totalTests = 0
$passedTests = 0
$failedTests = 0
$skippedTests = 0
$results = @()

# 编译测试
if (-not $SkipCompile) {
    Write-Host "=== 第一阶段：编译测试程序 ===" -ForegroundColor Yellow
    Write-Host ""
    
    foreach ($testName in $coreTests) {
        $testFile = Join-Path $baseDir "$testName.pas"
        
        if (-not (Test-Path $testFile)) {
            Write-Host "  [跳过] $testName - 源文件不存在" -ForegroundColor Gray
            $skippedTests++
            continue
        }
        
        Write-Host "  编译: $testName" -NoNewline
        
        $exeFile = Join-Path $outDir "$testName.exe"
        $compileCmd = "fpc -Fusrc -FE`"$outDir`" -FU`"$outDir`" -vi -vw `"$testFile`" 2>&1"
        
        $compileOutput = Invoke-Expression $compileCmd
        
        if ($LASTEXITCODE -eq 0 -and (Test-Path $exeFile)) {
            Write-Host " ✓" -ForegroundColor Green
            $compiled++
            
            if ($Verbose) {
                Write-Host "    编译成功：$exeFile" -ForegroundColor DarkGray
            }
        } else {
            Write-Host " ✗" -ForegroundColor Red
            $compileErrors++
            
            if ($Verbose) {
                Write-Host "    编译错误：" -ForegroundColor Red
                $compileOutput | Select-Object -Last 10 | ForEach-Object {
                    Write-Host "    $_" -ForegroundColor DarkRed
                }
            }
        }
    }
    
    Write-Host ""
    Write-Host "编译完成：成功 $compiled，失败 $compileErrors，跳过 $skippedTests" -ForegroundColor Cyan
    Write-Host ""
}

# 运行测试
Write-Host "=== 第二阶段：执行测试 ===" -ForegroundColor Yellow
Write-Host ""

$testExes = Get-ChildItem -Path $outDir -Filter "test_*.exe" | Sort-Object Name

foreach ($test in $testExes) {
    $testName = $test.BaseName
    
    Write-Host "  运行: $testName" -NoNewline
    
    $totalTests++
    $startTime = Get-Date
    
    try {
        $output = & $test.FullName 2>&1
        $exitCode = $LASTEXITCODE
        $endTime = Get-Date
        $duration = ($endTime - $startTime).TotalSeconds
        
        if ($exitCode -eq 0) {
            Write-Host " ✓" -ForegroundColor Green
            $passedTests++
            $status = "PASS"
            $color = "Green"
        } else {
            Write-Host " ✗" -ForegroundColor Red
            $failedTests++
            $status = "FAIL"
            $color = "Red"
            
            if ($Verbose) {
                Write-Host "    测试输出：" -ForegroundColor Red
                $output | Select-Object -Last 5 | ForEach-Object {
                    Write-Host "    $_" -ForegroundColor DarkRed
                }
            }
        }
        
        $results += [PSCustomObject]@{
            Test = $testName
            Status = $status
            Duration = [math]::Round($duration, 2)
            ExitCode = $exitCode
        }
    }
    catch {
        Write-Host " ✗ (异常)" -ForegroundColor Red
        $failedTests++
        
        $results += [PSCustomObject]@{
            Test = $testName
            Status = "ERROR"
            Duration = 0
            ExitCode = -1
        }
        
        if ($Verbose) {
            Write-Host "    异常: $_" -ForegroundColor Red
        }
    }
}

# 测试摘要
Write-Host ""
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  测试结果摘要" -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

if ($results.Count -gt 0) {
    $results | Format-Table -AutoSize
    
    Write-Host ""
    Write-Host "Total Tests:  $totalTests" -ForegroundColor White
    Write-Host "Passed:       $passedTests" -ForegroundColor Green
    Write-Host "Failed:       $failedTests" -ForegroundColor $(if ($failedTests -gt 0) { "Red" } else { "Green" })
    
    if ($totalTests -gt 0) {
        $successRate = [math]::Round(($passedTests / $totalTests) * 100, 1)
        Write-Host "Success Rate: $successRate%" -ForegroundColor White
    }
    
    Write-Host ""
    
    if ($failedTests -eq 0) {
        Write-Host "ALL TESTS PASSED!" -ForegroundColor Green
        exit 0
    } else {
        Write-Host "SOME TESTS FAILED!" -ForegroundColor Red
        exit 1
    }
} else {
    Write-Host "No executable test programs found" -ForegroundColor Yellow
    Write-Host "Tip: Run without -SkipCompile to compile tests" -ForegroundColor Yellow
    exit 2
}
