# run_winssl_tests.ps1
# WinSSL 集成测试自动化脚本

[CmdletBinding()]
param(
    [switch]$SkipCompile = $false
)

$ErrorActionPreference = "Stop"
$OriginalEncoding = [Console]::OutputEncoding
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8
$OriginalLocation = Get-Location
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$WinsslDir = Join-Path $ScriptDir "winssl"

if (-not (Test-Path $WinsslDir)) {
    throw "WinSSL test directory not found: $WinsslDir"
}

try {
    Set-Location $WinsslDir

Write-Host "======================================" -ForegroundColor Cyan
Write-Host "  WinSSL 集成测试套件" -ForegroundColor Cyan
Write-Host "======================================" -ForegroundColor Cyan
Write-Host ""

# 测试列表
$tests = @(
    @{
        Name = "WinSSL Unit Tests (Comprehensive)"
        Lpi = "test_winssl_unit_comprehensive.lpi"
        Exe = "bin\test_winssl_unit_comprehensive.exe"
        Description = "全面单元测试 - 68 个测试点"
    },
    @{
        Name = "WinSSL Integration Tests (Multi-Scenario)"
        Lpi = "test_winssl_integration_multi.lpi"
        Exe = "bin\test_winssl_integration_multi.exe"
        Description = "多场景集成测试 - 80 个测试点"
    },
    @{
        Name = "Backend Comparison Tests"
        Lpi = "..\integration\test_backend_comparison.lpi"
        Exe = "..\integration\bin\test_backend_comparison.exe"
        Description = "WinSSL vs OpenSSL 后端对比测试"
    },
    @{
        Name = "WinSSL Session Resumption Truth"
        Lpi = "test_winssl_session_resumption.lpi"
        Exe = "bin\test_winssl_session_resumption.exe"
        Description = "同一 Context 下的 resumed-handshake truth / evidence 测试"
        Env = @{
            FAFAFA_RUN_NETWORK_TESTS = "1"
            FAFAFA_WINSSL_SESSION_ATTEMPTS = "4"
        }
    },
    @{
        Name = "WinSSL Performance Benchmark"
        Lpi = "test_winssl_performance.lpi"
        Exe = "bin\test_winssl_performance.exe"
        Description = "性能基准测试 - 握手、传输、连接速率"
    },
    @{
        Name = "WinSSL Handshake Debug"
        Lpi = "test_winssl_handshake_debug.lpi"
        Exe = "bin\test_winssl_handshake_debug.exe"
        Description = "低级 Schannel 握手调试测试"
    },
    @{
        Name = "WinSSL HTTPS Client"
        Lpi = "test_winssl_https_client.lpi"
        Exe = "bin\test_winssl_https_client.exe"
        Description = "完整 HTTPS 客户端集成测试"
    }
)

$totalTests = $tests.Count
$passedTests = 0
$failedTests = 0
$failedTestNames = @()

function Write-EvidenceMarker {
    param(
        [string]$Marker
    )

    Write-Host ("[WINSSL-RUNTIME] " + $Marker) -ForegroundColor DarkCyan
}

Write-EvidenceMarker ("suite_start total=" + $totalTests)

# 编译测试
if (-not $SkipCompile) {
    Write-Host "编译测试程序..." -ForegroundColor Yellow
    Write-Host ""

    foreach ($test in $tests) {
        Write-Host "  编译: $($test.Name)" -NoNewline

        try {
            $output = & lazbuild $test.Lpi 2>&1

            if ($LASTEXITCODE -eq 0) {
                Write-Host " [OK]" -ForegroundColor Green
                Write-EvidenceMarker ("compile_result status=PASS lpi=" + $test.Lpi)
                if ($PSBoundParameters.ContainsKey('Verbose')) {
                    Write-Host "    输出: $($output -join "`n    ")" -ForegroundColor Gray
                }
            } else {
                Write-Host " [失败]" -ForegroundColor Red
                Write-Host "    编译错误:" -ForegroundColor Red
                Write-Host "    $($output -join "`n    ")" -ForegroundColor Red
                Write-EvidenceMarker ("compile_result status=FAIL lpi=" + $test.Lpi)
                Write-EvidenceMarker "suite_end status=FAIL phase=compile"
                exit 1
            }
        } catch {
            Write-Host " [错误]" -ForegroundColor Red
            Write-Host "    异常: $($_.Exception.Message)" -ForegroundColor Red
            Write-EvidenceMarker ("compile_result status=FAIL lpi=" + $test.Lpi + " reason=exception")
            Write-EvidenceMarker "suite_end status=FAIL phase=compile"
            exit 1
        }
    }

    Write-Host ""
    Write-Host "所有测试编译完成！" -ForegroundColor Green
    Write-Host ""
    Write-EvidenceMarker ("compile_phase status=PASS total=" + $totalTests)
}

# 运行测试
Write-Host "运行测试..." -ForegroundColor Yellow
Write-Host ""

foreach ($test in $tests) {
    $testIndex = $passedTests + $failedTests + 1
    $originalEnv = @{}
    Write-Host "[$testIndex/$totalTests] $($test.Name)" -ForegroundColor Cyan
    Write-Host "  描述: $($test.Description)" -ForegroundColor Gray
    Write-Host ""

    if (-not (Test-Path $test.Exe)) {
        Write-Host "  错误: 可执行文件不存在: $($test.Exe)" -ForegroundColor Red
        Write-EvidenceMarker ("test_result index=" + $testIndex + " status=FAIL reason=missing_exe")
        $failedTests++
        $failedTestNames += $test.Name
        continue
    }

    try {
        if ($test.ContainsKey('Env') -and $null -ne $test.Env) {
            foreach ($envName in $test.Env.Keys) {
                $originalEnv[$envName] = [System.Environment]::GetEnvironmentVariable($envName, 'Process')
                [System.Environment]::SetEnvironmentVariable($envName, $test.Env[$envName], 'Process')
                Write-Host ("  环境: " + $envName + "=" + $test.Env[$envName]) -ForegroundColor Gray
            }
            Write-Host ""
        }

        # 运行测试并捕获输出
        $startTime = Get-Date
        $output = & ".\$($test.Exe)" 2>&1
        $exitCode = $LASTEXITCODE
        $duration = (Get-Date) - $startTime
        $hasSessionResumeMarkers = ($output | Select-String -SimpleMatch "[WINSSL-SESSION-RESUME]" | Measure-Object).Count -gt 0

        # 显示输出
        if ($PSBoundParameters.ContainsKey('Verbose') -or $exitCode -ne 0 -or $hasSessionResumeMarkers) {
            Write-Host "  测试输出:" -ForegroundColor Gray
            Write-Host "  ---" -ForegroundColor Gray
            $output | ForEach-Object {
                if ($_ -match "PASS|通过|成功|SUCCESS") {
                    Write-Host "  $_" -ForegroundColor Green
                } elseif ($_ -match "FAIL|失败|错误|ERROR") {
                    Write-Host "  $_" -ForegroundColor Red
                } else {
                    Write-Host "  $_" -ForegroundColor Gray
                }
            }
            Write-Host "  ---" -ForegroundColor Gray
        }

        if ($hasSessionResumeMarkers) {
            $output | ForEach-Object {
                $line = [string]$_
                if ($line -match '^\[WINSSL-SESSION-RESUME\]\s*(.+)$') {
                    Write-EvidenceMarker ("session_resumption " + $matches[1].Trim())
                }
            }
        }

        # 检查结果
        if ($exitCode -eq 0) {
            Write-Host "  ✓ 通过 (耗时: $($duration.TotalSeconds.ToString("F2"))s)" -ForegroundColor Green
            Write-EvidenceMarker ("test_result index=" + $testIndex + " status=PASS duration_seconds=" + $duration.TotalSeconds.ToString("F2"))
            $passedTests++
        } else {
            Write-Host "  ✗ 失败 (退出码: $exitCode)" -ForegroundColor Red
            Write-EvidenceMarker ("test_result index=" + $testIndex + " status=FAIL exit_code=" + $exitCode + " duration_seconds=" + $duration.TotalSeconds.ToString("F2"))
            $failedTests++
            $failedTestNames += $test.Name
        }
    } catch {
        Write-Host "  ✗ 运行异常: $($_.Exception.Message)" -ForegroundColor Red
        Write-EvidenceMarker ("test_result index=" + $testIndex + " status=FAIL reason=exception")
        $failedTests++
        $failedTestNames += $test.Name
    } finally {
        if ($originalEnv.Count -gt 0) {
            foreach ($envName in $originalEnv.Keys) {
                [System.Environment]::SetEnvironmentVariable($envName, $originalEnv[$envName], 'Process')
            }
        }
    }

    Write-Host ""
}

# 汇总结果
Write-Host "======================================" -ForegroundColor Cyan
Write-Host "  测试结果汇总" -ForegroundColor Cyan
Write-Host "======================================" -ForegroundColor Cyan
Write-Host ""
Write-Host "  通过: $passedTests" -ForegroundColor Green
Write-Host "  失败: $failedTests" -ForegroundColor $(if ($failedTests -eq 0) { "Green" } else { "Red" })
Write-Host "  总计: $totalTests"
Write-Host ""

if ($failedTests -gt 0) {
    Write-Host "失败的测试:" -ForegroundColor Red
    foreach ($name in $failedTestNames) {
        Write-Host "  - $name" -ForegroundColor Red
    }
    Write-Host ""
}

$successRate = [math]::Round(($passedTests / $totalTests) * 100, 1)
Write-Host "成功率: $successRate%" -ForegroundColor $(if ($successRate -eq 100) { "Green" } else { "Yellow" })
Write-Host ""
Write-EvidenceMarker ("suite_summary passed=" + $passedTests + " failed=" + $failedTests + " total=" + $totalTests + " success_rate=" + $successRate)

# 恢复编码
[Console]::OutputEncoding = $OriginalEncoding

# 返回退出码
if ($failedTests -eq 0) {
    Write-Host "🎉 所有测试通过！" -ForegroundColor Green
    Write-EvidenceMarker "suite_end status=PASS"
    exit 0
} else {
    Write-Host "⚠️ 有测试失败" -ForegroundColor Red
    Write-EvidenceMarker "suite_end status=FAIL phase=runtime"
    exit 1
}
}
finally {
    [Console]::OutputEncoding = $OriginalEncoding
    Set-Location $OriginalLocation
}
