# ============================================================================
# OpenSSL 模块批量验证脚本
# ============================================================================
# 目的: 快速编译和验证所有 OpenSSL 模块
# 策略: 分层验证，记录结果
# ============================================================================

[CmdletBinding()]
param(
    [string]$ProjectRoot = "",
    [string]$RunId = "",
    [string]$OutputDir = "test-reports",
    [switch]$SkipCompile = $false,
    [int]$MinModuleCount = 50
)

$ErrorActionPreference = "Continue"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
if ([string]::IsNullOrWhiteSpace($ProjectRoot)) {
    $ProjectRoot = Split-Path -Parent $ScriptDir
}

if ([string]::IsNullOrWhiteSpace($RunId)) {
    $RunId = Get-Date -Format "yyyyMMdd_HHmmss"
}

$ProjectRootAbs = (Resolve-Path $ProjectRoot).Path
$srcDir = Join-Path $ProjectRootAbs "src"

$IsVerbose = $PSBoundParameters.ContainsKey('Verbose')

$outDirAbs = Join-Path $ProjectRootAbs $OutputDir
if (!(Test-Path $outDirAbs)) {
    New-Item -ItemType Directory -Path $outDirAbs -Force | Out-Null
}

# 报告文件
$compileLog = Join-Path $outDirAbs "validate_all_modules_compile_${RunId}.log"
$reportFile = Join-Path $outDirAbs "validate_all_modules_report_${RunId}.md"

# 初始化统计
$stats = @{
    Total = 0
    Success = 0
    Failed = 0
    Warnings = 0
    Errors = @()
}

# OpenSSL 模块来源：
# - 当前仓库的 OpenSSL 单元命名以 `fafafa.ssl.openssl.*` 为前缀（包含 `fafafa.ssl.openssl.api*`）。
# - 这里按文件系统动态扫描，避免硬编码清单与实际文件集漂移导致的“假阳性 PASS”。
$allModules = @()
if (Test-Path $srcDir) {
    $allModules = Get-ChildItem -Path $srcDir -Filter "fafafa.ssl.openssl*.pas" -File | Sort-Object -Property Name
}

if ($allModules.Count -lt $MinModuleCount) {
    Write-Host ("[FAIL] Too few OpenSSL modules detected in src/: {0} (min: {1}). Check ProjectRoot/src layout." -f $allModules.Count, $MinModuleCount) -ForegroundColor Red
    exit 1
}

$stats.Total = $allModules.Count

$moduleGroups = [ordered]@{
    "OpenSSL_API" = $allModules | Where-Object { $_.Name -like "fafafa.ssl.openssl.api*.pas" }
    "OpenSSL_Backend" = $allModules | Where-Object { $_.Name -like "fafafa.ssl.openssl.*.pas" -and $_.Name -notlike "fafafa.ssl.openssl.api*.pas" }
}

function Get-FpcUnitArgs {
    param(
        [string]$SrcDir
    )

    $args = @()
    $args += ("-Fu" + $SrcDir)

    try {
        $fpc = Get-Command fpc -ErrorAction SilentlyContinue
        if (-not $fpc) { return $args }

        $binDir = Split-Path -Parent $fpc.Source
        $root = (Resolve-Path (Join-Path $binDir "..\\..")).Path

        $tp = (& fpc -iTP 2>$null).Trim()
        $to = (& fpc -iTO 2>$null).Trim()
        if ([string]::IsNullOrWhiteSpace($tp) -or [string]::IsNullOrWhiteSpace($to)) { return $args }

        $unitsBase = Join-Path $root ("units\\" + $tp + "-" + $to)
        if (-not (Test-Path $unitsBase)) { return $args }

        $args += ("-Fu" + $unitsBase)
        Get-ChildItem -Path $unitsBase -Directory -ErrorAction SilentlyContinue | ForEach-Object {
            $args += ("-Fu" + $_.FullName)
        }
    } catch {
        return $args
    }

    return $args
}

$FpcUnitArgs = Get-FpcUnitArgs -SrcDir $srcDir

# 辅助函数：写日志
function Write-Log {
    param([string]$Message, [string]$Level = "INFO")
    $timestamp = Get-Date -Format "HH:mm:ss"
    $logMessage = "[$timestamp] [$Level] $Message"
    Add-Content -Path $compileLog -Value $logMessage
    
    if ($script:IsVerbose -or $Level -eq "ERROR") {
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
    
    Write-Log "Testing module: $moduleName" "INFO"
    
    # 轻量编译检查：隔离 -FU 产物目录，避免污染 src/；不因 warning 退出（Windows runner 默认会产生 UnicodeString 转换 warning）。
    $fpcPath = "fpc"  # 假设FPC在PATH中
    
    try {
        # 语法/编译检查：将单元输出隔离到 reports 目录，避免污染 src/
        $unitOutDir = Join-Path $outDirAbs "validate_all_modules_units_${RunId}"
        if (!(Test-Path $unitOutDir)) {
            New-Item -ItemType Directory -Path $unitOutDir -Force | Out-Null
        }

        $fpcArgs = @()
        $fpcArgs += $script:FpcUnitArgs
        $fpcArgs += ("-FU" + $unitOutDir)
        $fpcArgs += "$ModulePath"

        $result = & $fpcPath @fpcArgs 2>&1
        
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
            if ($result) {
                Write-Log "---- compiler output (tail) ----" "ERROR"
                ($result | Select-Object -Last 60) | ForEach-Object { Write-Log ("  " + $_) "ERROR" }
                Write-Log "---- end compiler output ----" "ERROR"
            }
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
Write-Log "项目根目录: $ProjectRootAbs" "INFO"
Write-Log "源码目录: $srcDir" "INFO"
Write-Log "输出目录: $outDirAbs" "INFO"
Write-Log ("检测到 OpenSSL 单元: {0} (min: {1})" -f $allModules.Count, $MinModuleCount) "INFO"

if (-not (Get-Command fpc -ErrorAction SilentlyContinue)) {
    Write-Log "❌ fpc not found in PATH" "ERROR"
    exit 1
}

$results = @{}

if (!$SkipCompile) {
    Write-Host "`n[阶段 1] 编译所有模块..." -ForegroundColor Yellow
    Write-Host "----------------------------------------`n"
    
    foreach ($group in $moduleGroups.Keys | Sort-Object) {
        Write-Host "验证组: $group" -ForegroundColor Cyan
        $groupResults = @()
        
        foreach ($module in $moduleGroups[$group]) {
            $modulePath = $module.FullName
            $result = Test-ModuleCompile -ModulePath $modulePath
            $groupResults += $result
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
**验证范围:** 所有 OpenSSL 模块 ($($allModules.Count)个，min=$MinModuleCount)  
**验证策略:** 分层快速验证

---

## 📊 总体统计

- **总模块数:** $($stats.Total)
- **编译成功:** $($stats.Success) ✅
- **编译失败:** $($stats.Failed) ❌
- **警告数量:** $($stats.Warnings) ⚠️
- **成功率:** $(if ($stats.Total -gt 0) { [math]::Round($stats.Success / $stats.Total * 100, 2) } else { 0 })%

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
Write-Host "成功率: $(if ($stats.Total -gt 0) { [math]::Round($stats.Success / $stats.Total * 100, 2) } else { 0 })%" -ForegroundColor $(if ($stats.Failed -eq 0) { "Green" } else { "Yellow" })
Write-Host "`n报告文件: $reportFile" -ForegroundColor Cyan
Write-Host "============================================`n" -ForegroundColor Cyan

# 返回状态码
exit $(if ($stats.Failed -eq 0) { 0 } else { 1 })
