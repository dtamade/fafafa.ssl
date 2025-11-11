# Windows 虚拟机验证 - 自动化脚本

**创建日期**: 2025-10-29  
**用途**: 在 Windows 虚拟机上执行完整的 WinSSL 验证并生成报告

---

## 📋 使用方法

### 方法 1: 在 Windows 虚拟机上直接运行

1. **复制项目到 Windows 虚拟机**
   - 可以通过共享文件夹、网络共享或 Git 克隆

2. **打开 PowerShell（管理员权限）**
   ```powershell
   cd C:\path\to\fafafa.ssl
   ```

3. **运行快速验证**
   ```powershell
   Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope Process
   .\tests\quick_winssl_validation.ps1
   ```

4. **查看验证报告**
   - 报告会保存在 `WINSSL_VALIDATION_REPORT.md`

---

### 方法 2: 通过 SSH 远程执行（如果配置了 OpenSSH Server）

**在 Linux 主机上**:
```bash
# 假设 Windows 虚拟机 IP 是 192.168.1.100
ssh user@192.168.1.100 "cd C:\path\to\fafafa.ssl && powershell -ExecutionPolicy Bypass -File tests\quick_winssl_validation.ps1"
```

---

### 方法 3: 使用共享文件夹

1. **在 Linux 主机上共享项目目录**
   ```bash
   # 使用 Samba 或 NFS
   ```

2. **在 Windows 虚拟机中映射网络驱动器**
   ```powershell
   net use Z: \\linux-host\fafafa.ssl
   cd Z:\
   ```

3. **运行验证脚本**
   ```powershell
   .\tests\quick_winssl_validation.ps1
   ```

---

## 🔍 验证报告位置

验证完成后，查看以下文件：

1. **完整报告**: `WINSSL_VALIDATION_REPORT.md`
2. **详细日志**: `tests\validation_log.txt`
3. **编译错误**: `tests\compile_errors.log`（如果有）
4. **测试输出**: `tests\test_output.log`

---

## 📊 验证结果解读

### ✅ 成功标志

- 所有测试编译成功
- 所有测试运行通过
- 无内存泄漏
- 性能指标正常

### ⚠️ 警告标志

- 部分测试失败（但非关键测试）
- 性能略低于预期
- 有编译警告（但非错误）

### ❌ 失败标志

- 关键测试失败
- 编译错误
- 运行时崩溃

---

## 🚀 下一步

验证完成后，请将 `WINSSL_VALIDATION_REPORT.md` 发送给我进行分析。

如果验证通过，我们可以：
1. 更新文档状态
2. 准备发布 v1.0.0-rc.1
3. 创建发布说明

如果验证失败，我会：
1. 分析失败原因
2. 提供修复方案
3. 准备修复代码

---

**准备开始验证**: 请告诉我虚拟机的访问方式，或直接在虚拟机上运行脚本！

