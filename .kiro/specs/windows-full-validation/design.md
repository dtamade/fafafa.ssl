# Design Document: Windows Full Validation

## Overview

本设计文档描述 fafafa.ssl 库在 Windows 环境下的全量验证套件架构。该套件将系统性地验证所有 115+ 源代码模块的编译、OpenSSL 后端 (65+ API 模块)、WinSSL 后端 (10+ 模块) 的功能完整性。

## Architecture

### 高层架构

```
┌─────────────────────────────────────────────────────────────────┐
│                    run_windows_validation.ps1                    │
│                      (主入口 PowerShell 脚本)                     │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐              │
│  │ 编译验证器   │  │ OpenSSL     │  │ WinSSL      │              │
│  │ Compiler    │  │ Validator   │  │ Validator   │              │
│  │ Validator   │  │             │  │             │              │
│  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘              │
│         │                │                │                      │
│         ▼                ▼                ▼                      │
│  ┌─────────────────────────────────────────────────────────────┐│
│  │                    Test Executor                             ││
│  │              (编译并运行测试程序)                              ││
│  └─────────────────────────────────────────────────────────────┘│
│         │                │                │                      │
│         ▼                ▼                ▼                      │
│  ┌─────────────────────────────────────────────────────────────┐│
│  │                    Report Generator                          ││
│  │              (生成 Markdown + JSON 报告)                      ││
│  └─────────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────────┘
```

### 组件设计

#### 1. 主入口脚本 (run_windows_validation.ps1)

**职责**:
- 解析命令行参数
- 协调各验证器执行
- 汇总结果并生成报告
- 管理执行超时和错误恢复

**参数**:
```powershell
param(
    [switch]$OpenSSLOnly,      # 仅测试 OpenSSL 后端
    [switch]$WinSSLOnly,       # 仅测试 WinSSL 后端
    [switch]$SkipCompile,      # 跳过编译步骤
    [switch]$Verbose,          # 详细输出
    [int]$Timeout = 60,        # 每个测试超时秒数
    [string]$ReportDir = "reports"  # 报告输出目录
)
```

#### 2. 编译验证器

**职责**: 验证所有源代码模块可以成功编译

**实现**:
```
输入: src/ 目录下所有 .pas 文件
处理: 使用 FPC 编译每个模块
输出: 编译结果列表 (成功/失败/错误信息)
```

**编译命令**:
```
C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe -Mobjfpc -Sh -Fu"src" -FE"bin" <file>
```

#### 3. OpenSSL 验证器

**职责**: 验证 OpenSSL 后端所有功能

**测试分类**:

| 类别 | 测试目录 | 测试数量 |
|------|----------|----------|
| API 模块 | tests/openssl/ | 13 |
| 加密功能 | tests/crypto/ | 55+ |
| 证书功能 | tests/certificate/ | 30+ |
| 连接功能 | tests/connection/ | 17 |
| 集成测试 | tests/integration/ | 30+ |

**依赖检查**:
- libssl-3-x64.dll
- libcrypto-3-x64.dll
- OpenSSL 版本 >= 3.0

#### 4. WinSSL 验证器

**职责**: 验证 Windows SChannel 后端所有功能

**测试分类**:

| 类别 | 测试目录 | 测试数量 |
|------|----------|----------|
| API 基础 | tests/winssl/ | 24 |
| 证书功能 | tests/winssl/ | 5 |
| 连接功能 | tests/winssl/ | 8 |
| 企业功能 | tests/winssl/ | 2 |

**依赖检查**:
- Windows 10/11 x64
- SChannel 可用

#### 5. 报告生成器

**职责**: 生成结构化测试报告

**Markdown 报告格式**:
```markdown
# Windows Full Validation Report

## 环境信息
- 日期: YYYY-MM-DD HH:MM:SS
- Windows 版本: ...
- FPC 版本: 3.2.2
- OpenSSL 版本: 3.x.x

## 摘要
- 总测试数: N
- 通过: N (XX%)
- 失败: N (XX%)
- 跳过: N (XX%)

## 详细结果
### 编译验证
...
### OpenSSL 测试
...
### WinSSL 测试
...

## 失败测试详情
...
```

**JSON 报告格式**:
```json
{
  "timestamp": "2026-01-06T...",
  "environment": {
    "os": "Windows 11",
    "fpc_version": "3.2.2",
    "openssl_version": "3.x.x"
  },
  "summary": {
    "total": 200,
    "passed": 195,
    "failed": 3,
    "skipped": 2
  },
  "compilation": [...],
  "openssl_tests": [...],
  "winssl_tests": [...],
  "failures": [...]
}
```

## Data Models

### TestResult 结构

```pascal
type
  TTestStatus = (tsPass, tsFail, tsSkip, tsError);
  
  TTestResult = record
    Name: string;           // 测试名称
    Module: string;         // 所属模块
    Status: TTestStatus;    // 测试状态
    Duration: Integer;      // 执行时间 (ms)
    Message: string;        // 错误/跳过信息
  end;
```

### ValidationReport 结构

```pascal
type
  TValidationReport = record
    Timestamp: TDateTime;
    Environment: TEnvironmentInfo;
    CompilationResults: array of TCompileResult;
    OpenSSLResults: array of TTestResult;
    WinSSLResults: array of TTestResult;
    Summary: TSummary;
  end;
```

## Component Details

### 测试执行流程

```
1. 初始化
   ├── 解析命令行参数
   ├── 检测环境 (FPC, OpenSSL, Windows 版本)
   └── 创建报告目录

2. 编译验证 (如果未跳过)
   ├── 遍历 src/*.pas
   ├── 编译每个模块
   └── 记录编译结果

3. OpenSSL 验证 (如果未指定 WinSSLOnly)
   ├── 检查 OpenSSL DLL 可用性
   ├── 编译并运行 tests/openssl/*.pas
   ├── 编译并运行 tests/crypto/*.pas
   ├── 编译并运行 tests/certificate/*.pas
   ├── 编译并运行 tests/connection/*.pas
   └── 编译并运行 tests/integration/*.pas

4. WinSSL 验证 (如果未指定 OpenSSLOnly)
   ├── 检查 SChannel 可用性
   └── 编译并运行 tests/winssl/*.pas

5. 后端对比 (如果两个后端都测试)
   └── 运行 tests/integration/test_backend_comparison.pas

6. 报告生成
   ├── 生成 Markdown 报告
   ├── 生成 JSON 报告
   └── 输出摘要到控制台
```

### 错误处理策略

| 错误类型 | 处理方式 |
|----------|----------|
| 编译失败 | 记录错误，继续下一个模块 |
| 测试超时 | 终止进程，标记为超时，继续 |
| 测试崩溃 | 捕获异常，记录崩溃信息，继续 |
| DLL 缺失 | 跳过相关测试，记录跳过原因 |
| 权限不足 | 报告警告，尝试继续 |

### 测试文件映射

#### OpenSSL API 模块测试映射

| 源模块 | 测试文件 |
|--------|----------|
| fafafa.ssl.openssl.api.aead.pas | tests/crypto/test_aead_*.pas |
| fafafa.ssl.openssl.api.aes.pas | tests/crypto/test_aes_simple.pas |
| fafafa.ssl.openssl.api.bio.pas | tests/certificate/test_bio_comprehensive.pas |
| fafafa.ssl.openssl.api.bn.pas | tests/certificate/test_bn_comprehensive.pas, tests/integration/test_bn_simple.pas |
| fafafa.ssl.openssl.api.chacha.pas | tests/crypto/test_chacha20.pas |
| fafafa.ssl.openssl.api.des.pas | tests/crypto/test_des_simple.pas |
| fafafa.ssl.openssl.api.dh.pas | tests/crypto/test_dh_simple.pas |
| fafafa.ssl.openssl.api.dsa.pas | tests/integration/test_dsa_simple.pas |
| fafafa.ssl.openssl.api.ec.pas | tests/integration/test_ec_*.pas |
| fafafa.ssl.openssl.api.ecdh.pas | tests/crypto/test_ecdh_simple.pas |
| fafafa.ssl.openssl.api.ecdsa.pas | tests/crypto/test_ecdsa_comprehensive.pas, tests/integration/test_ecdsa_simple.pas |
| fafafa.ssl.openssl.api.evp.pas | tests/crypto/test_evp_*.pas |
| fafafa.ssl.openssl.api.hmac.pas | tests/crypto/test_hmac_comprehensive.pas, tests/integration/test_hmac_simple.pas |
| fafafa.ssl.openssl.api.kdf.pas | tests/crypto/test_kdf_comprehensive.pas |
| fafafa.ssl.openssl.api.pem.pas | tests/test_pem.pas, tests/certificate/test_pem_simple.pas |
| fafafa.ssl.openssl.api.pkcs12.pas | tests/certificate/test_p2_pkcs12_*.pas |
| fafafa.ssl.openssl.api.pkcs7.pas | tests/certificate/test_p2_pkcs7_*.pas |
| fafafa.ssl.openssl.api.rand.pas | tests/crypto/test_rand_direct.pas, tests/integration/test_rand_simple.pas |
| fafafa.ssl.openssl.api.rsa.pas | tests/crypto/test_rsa_*.pas, tests/integration/test_rsa_*.pas |
| fafafa.ssl.openssl.api.sha.pas | tests/crypto/test_sha_simple.pas |
| fafafa.ssl.openssl.api.sha3.evp.pas | tests/crypto/test_sha3_*.pas |
| fafafa.ssl.openssl.api.ssl.pas | tests/connection/test_ssl_*.pas |
| fafafa.ssl.openssl.api.x509.pas | tests/test_x509.pas, tests/integration/test_x509_*.pas |

#### WinSSL 模块测试映射

| 源模块 | 测试文件 |
|--------|----------|
| fafafa.ssl.winssl.api.pas | tests/winssl/test_winssl_api_basic.pas |
| fafafa.ssl.winssl.certificate.pas | tests/winssl/test_winssl_certificate*.pas |
| fafafa.ssl.winssl.certstore.pas | tests/winssl/test_winssl_cert_*.pas |
| fafafa.ssl.winssl.connection.pas | tests/winssl/test_winssl_https_client.pas, test_winssl_handshake_*.pas |
| fafafa.ssl.winssl.context.pas | tests/winssl/test_winssl_unit_comprehensive.pas |
| fafafa.ssl.winssl.enterprise.pas | tests/winssl/test_winssl_enterprise.pas |
| fafafa.ssl.winssl.errors.pas | tests/winssl/test_winssl_errors.pas, test_winssl_error_mapping_online.pas |
| fafafa.ssl.winssl.lib.pas | tests/winssl/test_winssl_lib_simple.pas, test_winssl_library_basic.pas |
| fafafa.ssl.winssl.utils.pas | tests/winssl/test_winssl_utils.pas |

## Acceptance Criteria Mapping

| 需求 | 设计组件 | 验证方式 |
|------|----------|----------|
| REQ-1: 源代码编译验证 | 编译验证器 | 遍历 src/*.pas 编译 |
| REQ-2: OpenSSL API 测试 | OpenSSL 验证器 | 运行 tests/openssl/, tests/crypto/ |
| REQ-3: OpenSSL 高级功能 | OpenSSL 验证器 | 运行 tests/certificate/, tests/connection/ |
| REQ-4: WinSSL 后端测试 | WinSSL 验证器 | 运行 tests/winssl/ |
| REQ-5: 加密功能测试 | OpenSSL/WinSSL 验证器 | 运行 tests/crypto/ |
| REQ-6: 证书功能测试 | OpenSSL/WinSSL 验证器 | 运行 tests/certificate/ |
| REQ-7: TLS 连接测试 | OpenSSL/WinSSL 验证器 | 运行 tests/connection/ |
| REQ-8: SSH 密钥测试 | OpenSSL 验证器 | 运行 tests/test_ssh.pas |
| REQ-9: 测试报告生成 | 报告生成器 | 生成 MD + JSON |
| REQ-10: 自动化执行 | 主入口脚本 | PowerShell 参数支持 |
| REQ-11: 现有测试集成 | 测试执行器 | 扫描并运行现有测试 |
| REQ-12: 后端对比测试 | 对比验证器 | 运行 test_backend_comparison.pas |

## File Structure

```
fafafa.ssl/
├── run_windows_validation.ps1      # 主入口脚本 (新建)
├── tests/
│   └── windows/
│       ├── validation_config.json  # 验证配置 (新建)
│       ├── WINDOWS_VALIDATION_CHECKLIST.md  # 现有
│       └── VALIDATION_BUNDLE.md    # 现有
└── reports/                        # 报告输出目录 (新建)
    ├── validation_YYYYMMDD_HHMMSS.md
    └── validation_YYYYMMDD_HHMMSS.json
```

## Dependencies

### 外部依赖

| 依赖 | 版本 | 用途 |
|------|------|------|
| Free Pascal Compiler | 3.2.2 | 编译 Pascal 源代码 |
| OpenSSL | 3.x | OpenSSL 后端测试 |
| Windows SChannel | - | WinSSL 后端测试 |
| PowerShell | 5.1+ | 脚本执行 |

### 内部依赖

- 所有 src/*.pas 源代码模块
- 所有 tests/**/*.pas 测试程序
- 现有测试框架 tests/framework/

## Performance Considerations

### 执行时间优化

1. **并行编译**: 可选启用多线程编译 (FPC 不原生支持，但可并行启动多个 FPC 进程)
2. **增量测试**: 支持 -SkipCompile 跳过已验证的编译步骤
3. **超时控制**: 每个测试默认 60 秒超时，避免卡死

### 预期执行时间

| 阶段 | 预期时间 |
|------|----------|
| 编译验证 (115+ 模块) | 5-10 分钟 |
| OpenSSL 测试 (150+ 测试) | 10-15 分钟 |
| WinSSL 测试 (24 测试) | 3-5 分钟 |
| 报告生成 | < 1 分钟 |
| **总计** | **20-30 分钟** |

## Security Considerations

1. **测试隔离**: 测试程序在独立进程中运行，崩溃不影响主脚本
2. **临时文件清理**: 测试完成后清理 bin/ 目录中的临时可执行文件
3. **敏感信息**: 报告中不包含系统敏感信息 (如完整路径可配置隐藏)
4. **网络测试**: TLS 连接测试仅连接公共 HTTPS 服务器
