# Windows 验证完整准备清单

**创建日期**: 2025-10-29  
**目标**: WinSSL 后端 Windows 环境完整验证  
**预计总时长**: 2-4 小时（取决于发现问题数量）

---

## 📋 验证前准备

### 环境要求 ✅

#### 必需软件
- [ ] **Windows 10/11** (Build 19041 或更高，推荐 Build 20348+ 支持 TLS 1.3)
- [ ] **Lazarus IDE** 3.0+ 或 **Free Pascal** 3.3.1+
- [ ] **PowerShell** 5.1+ (Windows 自带)
- [ ] **Git** (用于克隆/更新代码)

#### 可选但推荐
- [ ] **Visual Studio** 或 **Windows SDK** (调试符号支持)
- [ ] **WinDbg** 或 **Visual Studio Debugger** (调试工具)
- [ ] **Process Monitor** (Sysinternals, 用于故障排查)

### 项目文件验证 ✅

#### 源代码
- [ ] `src/fafafa.ssl.winssl.*.pas` - 12 个 WinSSL 模块存在
- [ ] `src/fafafa.ssl.factory.pas` - 工厂模式实现
- [ ] `src/fafafa.ssl.intf.pas` - 统一接口定义

#### 测试程序
- [ ] `tests/test_winssl_certificate_loading.pas` - 证书加载测试 (529 行)
- [ ] `tests/test_winssl_handshake_debug.pas` - 握手调试测试
- [ ] `tests/test_winssl_https_client.pas` - HTTPS 客户端测试
- [ ] `tests/test_winssl_certificate.pas` - 证书存储测试
- [ ] `tests/test_winssl_lib_simple.pas` - 库基础测试

#### 测试脚本
- [ ] `tests/quick_winssl_validation.ps1` - 快速验证脚本 (2分钟)
- [ ] `tests/run_winssl_tests.ps1` - 完整测试套件 (15分钟)

#### 项目文件
- [ ] `tests/test_winssl_certificate_loading.lpi` - Lazarus 项目文件
- [ ] 其他 `.lpi` 文件都存在

### 环境配置检查 ✅

#### OpenSSL (用于 OpenSSL 后端回归测试)
- [ ] OpenSSL 3.x 安装 (可选，WinSSL 不需要)
- [ ] `libssl-3-x64.dll` 和 `libcrypto-3-x64.dll` 在 PATH 或项目目录

#### Windows 证书存储
- [ ] 当前用户证书存储可访问 (`Cert:\CurrentUser\My`)
- [ ] 本地计算机证书存储可访问 (`Cert:\LocalMachine\Root`) - 需要管理员权限
- [ ] 能够创建测试证书

#### 网络连接
- [ ] 能够访问互联网 (用于 HTTPS 客户端测试)
- [ ] 防火墙允许测试程序出站连接

---

## 🚀 验证步骤

### 阶段 1: 快速验证 (2分钟) ⚡ **最先执行**

**脚本**: `tests/quick_winssl_validation.ps1`

**执行**:
```powershell
cd tests
.\quick_winssl_validation.ps1
```

**预期结果**:
- ✅ 编译成功
- ✅ 证书加载测试通过
- ✅ 基础功能正常

**如果失败**: 记录错误信息，继续阶段 2 进行详细诊断

---

### 阶段 2: 编译验证 (10分钟)

**目标**: 确保所有 WinSSL 模块可以编译

**执行**:
```powershell
cd tests
.\run_winssl_tests.ps1 -CompileOnly
```

**检查项**:
- [ ] 所有 12 个 WinSSL 模块编译成功
- [ ] 无编译错误
- [ ] 无编译警告（或仅有预期警告）
- [ ] 生成的可执行文件存在

**失败处理**:
1. 记录编译错误
2. 检查 Free Pascal 版本
3. 检查单元路径配置
4. 检查 Windows 平台定义

---

### 阶段 3: 单元测试 (15分钟)

**目标**: 运行所有 WinSSL 单元测试

**执行**:
```powershell
cd tests
.\run_winssl_tests.ps1
```

**测试程序清单** (按优先级):

| 优先级 | 测试程序 | 描述 | 预期时间 |
|--------|----------|------|----------|
| 🔴 CRITICAL | `test_winssl_certificate_loading` | 证书加载功能 (2025-10-28) | 2分钟 |
| 🔴 HIGH | `test_winssl_lib_simple` | 库基础功能 | 1分钟 |
| 🔴 HIGH | `test_winssl_certificate` | 证书存储访问 | 2分钟 |
| 🟡 MEDIUM | `test_winssl_handshake_debug` | TLS 握手 | 3分钟 |
| 🟡 MEDIUM | `test_winssl_https_client` | HTTPS 客户端 | 5分钟 |
| 🟢 LOW | `test_winssl_performance` | 性能测试 | 2分钟 |

**检查项**:
- [ ] 所有测试程序编译成功
- [ ] 所有测试程序运行无崩溃
- [ ] 测试通过率 ≥ 90%
- [ ] 无内存泄漏（通过任务管理器观察）

**失败处理**:
1. 记录失败的测试
2. 收集错误输出
3. 截图（如有必要）
4. 检查 Windows 事件日志

---

### 阶段 4: 集成测试 (15分钟)

**目标**: 测试真实场景

**测试场景**:

#### 场景 1: HTTPS 客户端连接
```powershell
# 测试连接到真实 HTTPS 站点
.\bin\test_winssl_https_client.exe www.google.com
.\bin\test_winssl_https_client.exe www.github.com
```

**检查**:
- [ ] 成功建立 TLS 连接
- [ ] 证书验证正常
- [ ] SNI 工作正常
- [ ] 数据加密/解密正常

#### 场景 2: 证书存储访问
```powershell
# 测试从 Windows 证书存储加载证书
.\bin\test_winssl_certificate.exe
```

**检查**:
- [ ] 能够枚举证书
- [ ] 能够加载证书
- [ ] 证书信息正确显示

#### 场景 3: 自签名证书
```powershell
# 测试自签名证书支持
.\bin\test_winssl_certificate_loading.exe
```

**检查**:
- [ ] 能够创建自签名证书
- [ ] 能够加载自签名证书
- [ ] 服务器模式可以使用自签名证书

---

### 阶段 5: 错误处理测试 (10分钟)

**目标**: 验证错误处理正确性

**测试**:
- [ ] 无效证书文件 → 正确错误消息
- [ ] 网络连接失败 → 正确超时处理
- [ ] 证书过期 → 正确检测
- [ ] 内存不足 → 优雅降级
- [ ] 并发访问 → 无竞态条件

---

### 阶段 6: 性能测试 (5分钟)

**目标**: 验证性能在可接受范围

**指标**:
- [ ] TLS 握手时间 < 1秒
- [ ] 证书加载时间 < 100ms
- [ ] 数据加密速度 > 10 MB/s
- [ ] 内存使用 < 100MB (单个连接)

---

## 📊 验证结果记录

### 测试结果表格

| 测试项 | 状态 | 备注 |
|--------|------|------|
| 快速验证 | [ ] 通过 / [ ] 失败 | |
| 编译验证 | [ ] 通过 / [ ] 失败 | |
| 单元测试 | [ ] 通过 / [ ] 失败 | 通过率: ___% |
| 集成测试 | [ ] 通过 / [ ] 失败 | |
| 错误处理 | [ ] 通过 / [ ] 失败 | |
| 性能测试 | [ ] 通过 / [ ] 失败 | |

### 发现的问题

| ID | 严重性 | 描述 | 复现步骤 | 状态 |
|----|--------|------|----------|------|
| #1 | 🔴/🟡/🟢 | | | |
| #2 | 🔴/🟡/🟢 | | | |

**严重性定义**:
- 🔴 **严重**: 阻塞功能，必须修复
- 🟡 **中等**: 影响体验，建议修复
- 🟢 **轻微**: 可接受，后续修复

### 性能指标

| 指标 | 预期 | 实际 | 状态 |
|------|------|------|------|
| TLS 握手时间 | < 1s | | |
| 证书加载时间 | < 100ms | | |
| 数据加密速度 | > 10 MB/s | | |
| 内存使用 | < 100MB | | |

---

## ✅ 验收标准

### 必须通过 (🔴)

- [ ] **编译成功**: 所有 WinSSL 模块编译无错误
- [ ] **证书加载**: 证书加载功能正常工作
- [ ] **HTTPS 客户端**: 能够连接真实 HTTPS 站点
- [ ] **证书存储**: 能够访问 Windows 证书存储
- [ ] **握手成功**: TLS 握手流程正常

### 应该通过 (🟡)

- [ ] **性能**: 性能指标在预期范围内
- [ ] **错误处理**: 错误消息清晰有用
- [ ] **内存管理**: 无内存泄漏
- [ ] **并发安全**: 多线程环境稳定

### 可以接受 (🟢)

- [ ] **测试覆盖率**: ≥ 85%
- [ ] **文档完整性**: 代码注释完整

---

## 🔧 故障排查指南

### 常见问题

#### 问题 1: 编译失败 - "unit not found"
**原因**: 单元路径配置错误  
**解决**:
1. 检查 `-Fu` 参数包含 `src` 目录
2. 检查 `.lpi` 文件中的单元路径设置
3. 确认所有源文件存在

#### 问题 2: 运行时错误 - "Access Violation"
**原因**: 内存访问错误  
**解决**:
1. 使用调试器定位错误位置
2. 检查空指针访问
3. 检查数组越界
4. 检查 Windows API 调用参数

#### 问题 3: 证书加载失败
**原因**: 证书格式或权限问题  
**解决**:
1. 检查证书文件格式 (PEM/DER)
2. 检查文件读取权限
3. 检查证书存储访问权限
4. 验证证书有效性

#### 问题 4: TLS 握手失败
**原因**: 网络或配置问题  
**解决**:
1. 检查网络连接
2. 检查防火墙设置
3. 检查 TLS 版本支持
4. 检查 SNI 配置

#### 问题 5: 性能问题
**原因**: 配置或环境问题  
**解决**:
1. 检查是否使用 Release 构建
2. 检查防病毒软件是否干扰
3. 检查系统资源使用
4. 对比 OpenSSL 后端性能

---

## 📝 验证报告模板

验证完成后，请填写以下报告:

```markdown
# WinSSL Windows 验证报告

**日期**: YYYY-MM-DD
**验证者**: [姓名]
**Windows 版本**: Windows XX Build XXXXX
**编译器版本**: Free Pascal X.X.X / Lazarus X.X.X

## 执行摘要
[简要说明验证结果]

## 测试结果
[详细测试结果]

## 发现的问题
[问题列表]

## 性能指标
[性能测试结果]

## 结论
[ ] 通过 - 可以发布
[ ] 条件通过 - 需要修复部分问题
[ ] 失败 - 需要重大修复

## 建议
[下一步行动建议]
```

---

## 🎯 下一步行动

### 如果验证通过 ✅
1. 更新 `CURRENT_STATUS.md` - 标记 WinSSL 已验证
2. 更新 `README.md` - 更新 WinSSL 状态
3. 准备发布 v1.0.0-rc.1
4. 创建发布说明

### 如果验证失败 ❌
1. 记录所有问题
2. 按优先级分类问题
3. 创建修复计划
4. 修复后重新验证

### 如果条件通过 ⚠️
1. 记录所有已知问题
2. 创建工作区说明
3. 创建问题跟踪
4. 计划后续修复

---

## 📚 参考文档

- `PROJECT_ASSESSMENT_AND_PLAN.md` - 项目评估和计划
- `PLAN1_WINSSL_CODE_QUALITY_REPORT.md` - WinSSL 代码质量报告
- `PLAN4_WINDOWS_VALIDATION_SCRIPTS.md` - Windows 验证脚本文档
- `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md` - 详细验证清单

---

**准备完成**: 所有检查项完成即可开始验证  
**验证时长**: 预计 2-4 小时  
**关键路径**: 阶段 1 → 阶段 2 → 阶段 3 → 阶段 4

**⚠️ 重要**: 验证是发布前的关键步骤，请仔细执行并记录所有结果。

