# ============================================================================
# OpenSSL 模块批量验证脚本
# ============================================================================
# 目的: 快速编译和验证所有 OpenSSL 模块
# 策略: 分层验证，记录结果
# ============================================================================

param(
    [switch]$SkipCompile = $false,
    [switch]$Verbose = $false
)

$ErrorActionPreference = "Continue"
$projectRoot = "D:\projects\Pascal\lazarus\My\libs\fafafa.ssl"
$srcDir = Join-Path $projectRoot "src"
$reportDir = Join-Path $projectRoot "docs\validation"

# 创建报告目录
if (!(Test-Path $reportDir)) {
    New-Item -ItemType Directory -Path $reportDir -Force | Out-Null
}

# 报告文件
$timestamp = Get-Date -Format "yyyyMMdd_HHmmss"
$compileLog = Join-Path $reportDir "compile_$timestamp.log"
$reportFile = Join-Path $reportDir "validation_report_$timestamp.md"

# 初始化统计
$stats = @{
    Total = 0
    Success = 0
    Failed = 0
    Warnings = 0
    Errors = @()
}

# 模块分类（根据 VALIDATION_ROADMAP.md）
$moduleGroups = @{
    "P0_Core" = @(
        "fafafa.ssl.openssl.core.pas",
        "fafafa.ssl.openssl.evp.pas",
        "fafafa.ssl.openssl.hmac.pas",
        "fafafa.ssl.openssl.kdf.pas",
        "fafafa.ssl.openssl.rand.pas"
    )
    "P1_Asymmetric" = @(
        "fafafa.ssl.openssl.rsa.pas",
        "fafafa.ssl.openssl.ecdsa.pas",
        "fafafa.ssl.openssl.dsa.pas"
    )
    "P1_PKI" = @(
        "fafafa.ssl.openssl.x509.pas",
        "fafafa.ssl.openssl.x509v3.pas",
        "fafafa.ssl.openssl.pem.pas",
        "fafafa.ssl.openssl.asn1.pas",
        "fafafa.ssl.openssl.bio.pas"
    )
    "P1_BigNum" = @(
        "fafafa.ssl.openssl.bn.pas"
    )
    "P2_KeyExchange" = @(
        "fafafa.ssl.openssl.dh.pas",
        "fafafa.ssl.openssl.ecdh.pas"
    )
    "P2_SSL" = @(
        "fafafa.ssl.openssl.ssl.pas"
    )
    "P2_PKCS" = @(
        "fafafa.ssl.openssl.pkcs7.pas",
        "fafafa.ssl.openssl.pkcs12.pas",
        "fafafa.ssl.openssl.pkcs.pas",
        "fafafa.ssl.openssl.cms.pas"
    )
    "P2_Helpers" = @(
        "fafafa.ssl.openssl.err.pas",
        "fafafa.ssl.openssl.buffer.pas",
        "fafafa.ssl.openssl.obj.pas",
        "fafafa.ssl.openssl.stack.pas"
    )
    "P3_Ciphers" = @(
        "fafafa.ssl.openssl.aes.pas",
        "fafafa.ssl.openssl.des.pas",
        "fafafa.ssl.openssl.chacha.pas",
        "fafafa.ssl.openssl.aria.pas",
        "fafafa.ssl.openssl.seed.pas",
        "fafafa.ssl.openssl.legacy_ciphers.pas",
        "fafafa.ssl.openssl.modes.pas"
    )
    "P3_Hash" = @(
        "fafafa.ssl.openssl.sha.pas",
        "fafafa.ssl.openssl.sha3.pas",
        "fafafa.ssl.openssl.sha3.evp.pas",
        "fafafa.ssl.openssl.blake2.pas",
        "fafafa.ssl.openssl.md.pas"
    )
    "P3_MAC" = @(
        "fafafa.ssl.openssl.cmac.pas",
        "fafafa.ssl.openssl.cmac.evp.pas"
    )
    "P3_Special" = @(
        "fafafa.ssl.openssl.sm.pas",
        "fafafa.ssl.openssl.scrypt_whirlpool.pas"
    )
    "P4_Advanced" = @(
        "fafafa.ssl.openssl.ocsp.pas",
        "fafafa.ssl.openssl.ts.pas",
        "fafafa.ssl.openssl.ct.pas",
        "fafafa.ssl.openssl.store.pas",
        "fafafa.ssl.openssl.srp.pas",
        "fafafa.ssl.openssl.conf.pas",
        "fafafa.ssl.openssl.param.pas",
        "fafafa.ssl.openssl.engine.pas",
        "fafafa.ssl.openssl.provider.pas"
    )
    "P5_Infrastructure" = @(
        "fafafa.ssl.openssl.types.pas",
        "fafafa.ssl.openssl.consts.pas",
        "fafafa.ssl.openssl.api.pas",
        "fafafa.ssl.openssl.crypto.pas",
        "fafafa.ssl.openssl.utils.pas",
        "fafafa.ssl.openssl.thread.pas",
        "fafafa.ssl.openssl.async.pas",
        "fafafa.ssl.openssl.lhash.pas",
        "fafafa.ssl.openssl.txt_db.pas",
        "fafafa.ssl.openssl.ui.pas",
        "fafafa.ssl.openssl.dso.pas",
        "fafafa.ssl.openssl.aead.pas",
        "fafafa.ssl.openssl.comp.pas",
        "fafafa.ssl.openssl.rand_old.pas"
    )
}

# 辅助函数：写日志
function Write-Log {
    param([string]$Message, [string]$Level = "INFO")
    $timestamp = Get-Date -Format "HH:mm:ss"
    $logMessage = "[$timestamp] [$Level] $Message"
    Add-Content -Path $compileLog -Value $logMessage
    
    if ($Verbose -or $Level -eq "ERROR") {
        switch ($Level) {
            "ERROR" { Write-Host $logMessage -ForegroundColor Red }
            "WARN"  { Write-Host $logMessage -ForegroundColor Yellow }
            "SUCCESS" { Write-Host $logMessage -ForegroundColor Green }
            default { Write-Host $logMessage }
        }
    }
}

# 辅助函数：编译单个模块
function Test-ModuleCompile {
    param([string]$ModulePath)
    
    $moduleName = Split-Path $ModulePath -Leaf
    $stats.Total++
    
    Write-Log "Testing module: $moduleName" "INFO"
    
    # 基本语法检查（使用 fpc -Sew 仅检查语法）
    $fpcPath = "fpc"  # 假设FPC在PATH中
    
    try {
        # 仅语法检查，不生成输出文件
        $result = & $fpcPath -Sew -vn "$ModulePath" 2>&1
        
        if ($LASTEXITCODE -eq 0) {
            Write-Log "✅ $moduleName - OK" "SUCCESS"
            $stats.Success++
            return @{
                Success = $true
                Module = $moduleName
                Warnings = 0
                Errors = @()
            }
        } else {
            Write-Log "❌ $moduleName - FAILED" "ERROR"
            $stats.Failed++
            $stats.Errors += $moduleName
            return @{
                Success = $false
                Module = $moduleName
                Warnings = 0
                Errors = @($result)
            }
        }
    } catch {
        Write-Log "❌ $moduleName - EXCEPTION: $_" "ERROR"
        $stats.Failed++
        $stats.Errors += $moduleName
        return @{
            Success = $false
            Module = $moduleName
            Warnings = 0
            Errors = @($_.Exception.Message)
        }
    }
}

# 主逻辑
Write-Host "============================================" -ForegroundColor Cyan
Write-Host "OpenSSL 模块批量验证" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
Write-Host ""

Write-Log "开始验证" "INFO"
Write-Log "项目根目录: $projectRoot" "INFO"
Write-Log "源码目录: $srcDir" "INFO"

$results = @{}

if (!$SkipCompile) {
    Write-Host "`n[阶段 1] 编译所有模块..." -ForegroundColor Yellow
    Write-Host "----------------------------------------`n"
    
    foreach ($group in $moduleGroups.Keys | Sort-Object) {
        Write-Host "验证组: $group" -ForegroundColor Cyan
        $groupResults = @()
        
        foreach ($module in $moduleGroups[$group]) {
            $modulePath = Join-Path $srcDir $module
            
            if (Test-Path $modulePath) {
                $result = Test-ModuleCompile -ModulePath $modulePath
                $groupResults += $result
            } else {
                Write-Log "⚠️ 文件不存在: $module" "WARN"
                $stats.Warnings++
            }
        }
        
        $results[$group] = $groupResults
        Write-Host ""
    }
} else {
    Write-Host "跳过编译阶段（使用 -SkipCompile）`n" -ForegroundColor Yellow
}

# 生成验证报告
Write-Host "`n[阶段 2] 生成验证报告..." -ForegroundColor Yellow
Write-Host "----------------------------------------`n"

$report = @"
# OpenSSL 模块验证报告

**生成时间:** $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")  
**验证范围:** 所有 OpenSSL 模块 (65个)  
**验证策略:** 分层快速验证

---

## 📊 总体统计

- **总模块数:** $($stats.Total)
- **编译成功:** $($stats.Success) ✅
- **编译失败:** $($stats.Failed) ❌
- **警告数量:** $($stats.Warnings) ⚠️
- **成功率:** $([math]::Round($stats.Success / $stats.Total * 100, 2))%

---

## 📋 分组验证结果

"@

foreach ($group in $moduleGroups.Keys | Sort-Object) {
    $groupStats = $results[$group]
    if ($groupStats) {
        $successCount = ($groupStats | Where-Object { $_.Success }).Count
        $totalCount = $groupStats.Count
        
        $report += @"

### $group
**状态:** $successCount/$totalCount 模块通过

| 模块 | 状态 |
|------|------|
"@
        
        foreach ($result in $groupStats) {
            $status = if ($result.Success) { "✅ 通过" } else { "❌ 失败" }
            $report += "`n| $($result.Module) | $status |"
        }
        
        $report += "`n"
    }
}

if ($stats.Failed -gt 0) {
    $report += @"

---

## ❌ 失败模块详情

"@
    
    foreach ($errorModule in $stats.Errors) {
        $report += "- **$errorModule**`n"
    }
}

$report += @"

---

## 🎯 下一步建议

"@

if ($stats.Failed -eq 0) {
    $report += @"

### ✅ 所有模块编译通过！

**建议行动:**
1. 继续 P1 模块集成测试（RSA、ECDSA、X.509）
2. 创建 P2 模块示例程序
3. 编写使用文档
4. 进行性能基准测试

"@
} else {
    $report += @"

### ⚠️ 存在编译失败模块

**建议行动:**
1. 优先修复 P1 高优先级模块
2. 检查依赖关系是否正确
3. 确认 OpenSSL 版本兼容性
4. 修复后重新验证

"@
}

$report += @"

---

**验证日志:** $compileLog  
**报告文件:** $reportFile

"@

# 保存报告
Set-Content -Path $reportFile -Value $report -Encoding UTF8

Write-Log "报告已生成: $reportFile" "SUCCESS"

# 显示摘要
Write-Host "`n============================================" -ForegroundColor Cyan
Write-Host "验证完成" -ForegroundColor Cyan
Write-Host "============================================" -ForegroundColor Cyan
Write-Host "总模块数: $($stats.Total)" -ForegroundColor White
Write-Host "成功: $($stats.Success)" -ForegroundColor Green
Write-Host "失败: $($stats.Failed)" -ForegroundColor $(if ($stats.Failed -eq 0) { "Green" } else { "Red" })
Write-Host "成功率: $([math]::Round($stats.Success / $stats.Total * 100, 2))%" -ForegroundColor $(if ($stats.Failed -eq 0) { "Green" } else { "Yellow" })
Write-Host "`n报告文件: $reportFile" -ForegroundColor Cyan
Write-Host "============================================`n" -ForegroundColor Cyan

# 返回状态码
exit $(if ($stats.Failed -eq 0) { 0 } else { 1 })
