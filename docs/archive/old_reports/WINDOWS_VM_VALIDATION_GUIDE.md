# Windows 虚拟机验证自动化脚本

**创建日期**: 2025-10-29  
**用途**: 在 Windows 虚拟机上自动执行 WinSSL 验证

---

## 🚀 快速开始

### 选项 1: 完整自动化验证（推荐）

**步骤**:
1. 将项目复制到 Windows 虚拟机
2. 打开 PowerShell（管理员权限）
3. 运行验证脚本：

```powershell
cd C:\path\to\fafafa.ssl
.\tests\quick_winssl_validation.ps1
```

**输出**: 自动生成验证报告 `WINSSL_VALIDATION_REPORT.md`

---

### 选项 2: 分步验证（调试用）

**步骤**:
1. 编译验证
2. 单元测试
3. 集成测试
4. 生成报告

---

## 📋 前置条件检查

运行以下 PowerShell 脚本检查环境：

```powershell
# 检查环境
Write-Host "=== 环境检查 ===" -ForegroundColor Cyan

# 检查 Free Pascal
if (Get-Command fpc -ErrorAction SilentlyContinue) {
    $fpcVersion = fpc -iV
    Write-Host "✓ Free Pascal: $fpcVersion" -ForegroundColor Green
} else {
    Write-Host "✗ Free Pascal 未安装" -ForegroundColor Red
}

# 检查 Lazarus
if (Get-Command lazbuild -ErrorAction SilentlyContinue) {
    $lazVersion = lazbuild --version 2>&1 | Select-Object -First 1
    Write-Host "✓ Lazarus: $lazVersion" -ForegroundColor Green
} else {
    Write-Host "✗ Lazarus 未安装" -ForegroundColor Red
}

# 检查 PowerShell 版本
$psVersion = $PSVersionTable.PSVersion
Write-Host "✓ PowerShell: $psVersion" -ForegroundColor Green

# 检查 Windows 版本
$osVersion = [System.Environment]::OSVersion.Version
Write-Host "✓ Windows: $($osVersion.Major).$($osVersion.Minor) Build $($osVersion.Build)" -ForegroundColor Green

# 检查项目文件
if (Test-Path "tests\test_winssl_certificate_loading.pas") {
    Write-Host "✓ 测试文件存在" -ForegroundColor Green
} else {
    Write-Host "✗ 测试文件不存在" -ForegroundColor Red
}
```

---

## 📝 验证报告模板

验证完成后，脚本会自动生成 `WINSSL_VALIDATION_REPORT.md` 报告。

---

## 🔧 故障排查

### 如果验证失败

1. **查看详细错误**:
   ```powershell
   Get-Content tests\validation_log.txt
   ```

2. **检查编译错误**:
   ```powershell
   Get-Content tests\compile_errors.log
   ```

3. **单独测试**:
   ```powershell
   cd tests
   lazbuild test_winssl_certificate_loading.lpi
   .\bin\test_winssl_certificate_loading.exe
   ```

---

## 📞 获取帮助

如果遇到问题，请提供：
1. Windows 版本
2. Free Pascal 版本
3. 错误消息
4. 验证日志文件

