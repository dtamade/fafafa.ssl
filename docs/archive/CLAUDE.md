# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

---

## 项目概述

**fafafa.ssl** 是一个用于 Free Pascal 和 Lazarus 的多后端 SSL/TLS 抽象框架，**不只是 OpenSSL 绑定**。它提供统一的 API，可跨多个 SSL/TLS 后端工作。

**核心优势**：通过原生 Schannel (WinSSL 后端) 实现 Windows 零依赖部署，同时通过 OpenSSL 支持保持跨平台兼容性。

**状态**：接近生产就绪（78% 测试覆盖率，51/65 模块已验证，部分功能待完善）

---

## 构建和测试

### 编译命令

**推荐：对所有 .lpi 项目使用 lazbuild**
```bash
# 单个项目
lazbuild path/to/project.lpi

# 多个项目
lazbuild project1.lpi project2.lpi project3.lpi

# 批量编译所有测试
lazbuild tests/*.lpi
```

**仅用于简单的独立测试文件：**
```bash
fpc -Fusrc -FEtests/bin -Futests/bin test_module.pas
```

**为什么用 lazbuild？**
- 包含完整项目配置（搜索路径、编译选项、依赖关系）
- 与 IDE 编译一致
- 更好的可维护性

### 运行测试

**核心功能测试：**
```powershell
# Windows PowerShell
cd tests
.\run_core_tests.ps1

# 选项：
.\run_core_tests.ps1 -SkipCompile  # 只运行，不重新编译
.\run_core_tests.ps1 -Verbose      # 显示详细输出
```

**所有测试：**
```powershell
.\run_all_tests.ps1
```

**单个测试：**
```bash
# 编译
lazbuild tests/test_specific.lpi
# 或：fpc -Fusrc -FEtests/bin tests/test_specific.pas

# 运行
tests/bin/test_specific.exe
```

---

## 代码架构

### 三层设计

```
┌─────────────────────────────────────────┐
│     应用层                              │
│  (使用 fafafa.ssl 的代码)              │
└──────────────┬──────────────────────────┘
               │ uses fafafa.ssl.factory
               ↓
┌─────────────────────────────────────────┐
│     抽象接口层                          │
│  • fafafa.ssl.abstract.intf            │
│  • fafafa.ssl.abstract.types           │
│  • fafafa.ssl.factory (创建实现)      │
└──────────────┬──────────────────────────┘
               │ implements
               ↓
┌─────────────────────────────────────────┐
│        后端实现                         │
│                                         │
│  OpenSSL (65+ 模块):                   │
│  • fafafa.ssl.openssl.api.*            │
│                                         │
│  WinSSL (8 模块):                      │
│  • fafafa.ssl.winssl.*                 │
│                                         │
│  未来：MbedTLS, LibreSSL 等            │
└─────────────────────────────────────────┘
```

### 核心接口 (fafafa.ssl.abstract.intf)

所有后端实现必须实现这些 CORBA 接口：

- **ISSLLibrary**：库初始化、版本信息、功能检测、工厂方法
- **ISSLContext**：SSL 配置、证书/密钥管理、密码套件、连接工厂
- **ISSLConnection**：TLS 握手、数据传输、连接状态、会话管理
- **ISSLCertificate**：证书加载、验证、信息提取
- **ISSLCertificateStore**：证书存储、系统存储访问、链构建
- **ISSLSession**：会话恢复和缓存

### 模块组织

**抽象层：**
- `fafafa.ssl.abstract.intf` - 核心接口（ISSLLibrary, ISSLContext 等）
- `fafafa.ssl.abstract.types` - 共享类型和枚举
- `fafafa.ssl.factory` - 后端选择和实例化
- `fafafa.ssl.intf` - 传统兼容层（重定向到 abstract.intf）

**OpenSSL 后端（`fafafa.ssl.openssl.api.*` 中的 65+ 模块）：**
- 核心：`core`, `crypto`, `evp`, `bio`, `err`
- 对称加密：`aes`, `des`, `chacha`, `aria`, `camellia`, `seed`, `sm4`
- 哈希：`sha`, `sha3`, `blake2`, `md`, `sm3`
- 非对称加密：`rsa`, `dsa`, `ec`, `ecdsa`, `ecdh`, `dh`
- PKI：`x509`, `x509v3`, `pem`, `pkcs7`, `pkcs12`, `cms`, `ocsp`, `ct`, `ts`
- MAC/KDF：`hmac`, `cmac`, `kdf`
- SSL/TLS：`ssl`（主要协议实现）
- 高级：`engine`, `provider`, `async`, `comp`, `store`

**WinSSL 后端（`fafafa.ssl.winssl.*` 中的 8 模块）：**
- `types` - Schannel 类型和常量
- `api` - Windows API 绑定（secur32.dll, crypt32.dll）
- `utils` - 辅助函数（错误处理、协议映射、缓冲区）
- `lib` - ISSLLibrary 实现（TWinSSLLibrary）
- `context` - ISSLContext 实现（TWinSSLContext）
- `connection` - ISSLConnection 实现（TWinSSLConnection）
- `certificate` - ISSLCertificate 实现（TWinSSLCertificate）
- `optimized` - 性能优化实现

### 工厂模式使用

```pascal
uses
  fafafa.ssl.factory,
  fafafa.ssl.abstract.intf;

var
  Lib: ISSLLibrary;
  Ctx: ISSLContext;
  Conn: ISSLConnection;
begin
  // 工厂自动选择最佳可用后端
  // Windows: WinSSL（如果可用），否则 OpenSSL
  // Linux/macOS: OpenSSL
  Lib := CreateSSLLibrary();

  if not Lib.Initialize then
    raise Exception.Create('SSL 初始化失败');

  WriteLn('使用: ', Lib.GetLibraryType);  // sslLibraryWinSSL 或 sslLibraryOpenSSL

  // 创建上下文和连接
  Ctx := Lib.CreateContext(sslContextClient);
  Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);

  Conn := Ctx.CreateConnection(SocketHandle);
  if Conn.Connect then
    WriteLn('TLS 握手成功');
end;
```

---

## 开发指南

### 测试驱动开发（必须遵守）

**红 → 绿 → 重构 → 文档**

1. **红**：先写失败的测试
2. **绿**：写最少代码使测试通过
3. **重构**：在保持测试通过的同时改进代码
4. **文档**：更新文档并添加使用示例

**每个功能在实现之前必须有测试。**

### 测试结构模板

```pascal
program test_feature_module;

{$mode objfpc}{$H+}{$J-}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  SysUtils, Classes, fafafa.ssl.module;

var
  TotalTests, PassedTests, FailedTests: Integer;

procedure Test(const TestName: string; Condition: Boolean);
begin
  Inc(TotalTests);
  Write(TestName + ': ');
  if Condition then
  begin
    WriteLn('PASS');
    Inc(PassedTests);
  end
  else
  begin
    WriteLn('FAIL');
    Inc(FailedTests);
  end;
end;

procedure TestFeature1_ExpectedBehavior_Condition;
var
  LResult: Integer;
begin
  // Arrange（准备）

  // Act（执行）

  // Assert（断言）
  Test('Feature1 应该做 X', LResult = ExpectedValue);
end;

begin
  TotalTests := 0;
  PassedTests := 0;
  FailedTests := 0;

  WriteLn('测试：模块名称');
  WriteLn('=' + StringOfChar('=', 50));

  TestFeature1_ExpectedBehavior_Condition;
  // ... 更多测试

  WriteLn('=' + StringOfChar('=', 50));
  WriteLn(Format('结果：%d/%d 通过 (%.1f%%)',
    [PassedTests, TotalTests, PassedTests * 100.0 / TotalTests]));

  if FailedTests > 0 then
    Halt(1);
end.
```

### 命名约定

**变量：**
- 局部变量：`L` 前缀（`LCount`, `LBuffer`, `LResult`）
- 参数：`a` 前缀（`aFileName`, `aOptions`, `aCallback`）
- 字段：`F` 前缀（`FHandle`, `FContext`, `FInitialized`）

**类型：**
- 类：`T` 前缀（`TWinSSLContext`, `TSSLConfig`）
- 接口：`I` 前缀（`ISSLLibrary`, `ISSLConnection`）
- 枚举：小写带前缀（`sslProtocolTLS12`, `sslErrorNone`）

**常量：**
- 全大写加下划线（`MAX_BUFFER_SIZE`, `DEFAULT_TIMEOUT`）

**函数：**
- 以动词开头（`CreateSSLLibrary`, `ValidateCertificate`, `GetProtocolName`）

### 代码风格

```pascal
{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}  // Windows 控制台输出必需

// 缩进：2 个空格（不用制表符）
procedure Example(aParam: Integer);
var
  LLocal: string;
begin
  if aParam > 0 then
  begin
    LLocal := 'positive';
    DoSomething(LLocal);
  end;
end;
```

**最佳实践：**
- 函数保持在 50 行以下
- 将超过 1000 行的文件拆分为更小的模块
- 为公共方法和复杂逻辑添加注释
- 使用描述性名称而不是注释

---

## 常见开发任务

### 添加新的 OpenSSL 模块

1. 创建 API 绑定文件：`src/fafafa.ssl.openssl.api.newmodule.pas`
2. 定义类型、常量和函数指针
3. 在 initialization 段实现动态加载
4. 创建测试：`tests/test_newmodule.pas` 及其 .lpi 项目文件
5. 将其添加到文档的模块列表

**参考现有模块如 `fafafa.ssl.openssl.api.aes.pas` 的结构。**

### 添加 WinSSL 功能

1. 如需要，添加 Windows API 绑定到 `fafafa.ssl.winssl.api.pas`
2. 添加类型/常量到 `fafafa.ssl.winssl.types.pas`
3. 在适当的类中实现（`lib`、`context` 或 `connection`）
4. 如有用，添加辅助函数到 `fafafa.ssl.winssl.utils.pas`
5. 编写测试并在 Windows 10/11 上验证

### 修复编译错误

**常见问题：**

1. **运行时 "函数未加载" 错误**
   ```pascal
   // 缺少模块加载调用
   LoadOpenSSLCore;
   LoadEVP(GetCryptoLibHandle);  // 添加这个！
   ```

2. **类型不匹配错误**
   ```pascal
   // 使用显式类型转换
   MyFunc := TMyFuncType(GetProcAddress(Handle, 'FuncName'));
   ```

3. **内联变量声明（FPC 3.3.1+）**
   ```pascal
   // 在过程/函数开始处声明，不要内联
   var
     LValue: Integer;
   begin
     LValue := 42;  // 不是：var LValue := 42;
   end;
   ```

### 运行特定测试套件

```powershell
# OpenSSL 核心测试
cd tests
.\run_core_tests.ps1

# P2 模块（PKI 功能）
lazbuild test_p2_pkcs7.lpi test_p2_pkcs12.lpi test_p2_ocsp.lpi
tests\bin\test_p2_pkcs7.exe
tests\bin\test_p2_pkcs12.exe
tests\bin\test_p2_ocsp.exe

# WinSSL 测试（仅 Windows）
lazbuild tests\test_winssl_api_basic.lpi
tests\bin\test_winssl_api_basic.exe
```

---

## 关键实现细节

### OpenSSL 3.x vs 1.1.x 兼容性

**本库通过运行时检测同时支持 OpenSSL 3.x 和 1.1.x：**

```pascal
// 自动版本检测
LoadOpenSSLCore;  // 先尝试 3.x，回退到 1.1.x
WriteLn(GetOpenSSLVersionString);

// 强制指定版本
LoadOpenSSLCoreWithVersion(sslVersion_3_0);  // 仅 OpenSSL 3.x
LoadOpenSSLCoreWithVersion(sslVersion_1_1);  // 仅 OpenSSL 1.1.x
```

**主要区别：**
- OpenSSL 3.x 使用 `EVP_MD_fetch()` / `EVP_CIPHER_fetch()`（首选）
- OpenSSL 1.1.x 使用 `EVP_get_digestbyname()` / `EVP_get_cipherbyname()`
- 大多数模块中的代码自动处理两者
- 某些算法（SHA-3、SM3、SM4）需要 OpenSSL 3.x

### WinSSL（Windows Schannel）注意事项

**当前状态**：Phase 2.2 完成（核心组件功能正常）
- Library、Context、Connection 类已完全实现
- 客户端 TLS 握手工作
- 服务器支持部分实现
- 证书管理进行中

**平台要求：**
- Windows Vista+（Schannel 可用）
- Windows 7+ 用于 TLS 1.0/1.1/1.2
- Windows 10 20348+ 或 Windows 11 用于 TLS 1.3

**文件位置：**
- API：`src/fafafa.ssl.winssl.api.pas`
- 类型：`src/fafafa.ssl.winssl.types.pas`
- 工具：`src/fafafa.ssl.winssl.utils.pas`
- 实现：`src/fafafa.ssl.winssl.{lib,context,connection}.pas`

### 动态库加载模式

所有 OpenSSL 函数在运行时动态加载：

```pascal
// 1. 声明函数指针变量
var
  EVP_sha256: function: PEVP_MD; cdecl;

// 2. 在 initialization 中加载
initialization
  if OpenSSLLoaded then
  begin
    EVP_sha256 := GetProcAddress(LibHandle, 'EVP_sha256');
    if not Assigned(EVP_sha256) then
      WriteLn('警告：未找到 EVP_sha256');
  end;
```

**调用前务必检查函数是否已加载：**
```pascal
if Assigned(EVP_sha256) then
  MD := EVP_sha256()
else
  raise Exception.Create('EVP_sha256 不可用');
```

### 错误处理模式

```pascal
// OpenSSL
if not OpenSSLFunction(...) then
begin
  ErrCode := ERR_get_error();
  ErrMsg := ERR_error_string(ErrCode, nil);
  raise Exception.CreateFmt('操作失败：%s', [ErrMsg]);
end;

// WinSSL
Status := SecurityFunction(...);
if not IsSuccess(Status) then
begin
  Category := GetSchannelErrorCategory(Status);
  ErrMsg := GetSchannelErrorString(Status);
  raise Exception.CreateFmt('[%s] %s', [Category, ErrMsg]);
end;
```

---

## 模块优先级指南

在开发新功能或修复问题时，按模块层级排序：

**P0（核心）- 关键：**
- `openssl.api.core` - 库加载
- `openssl.api.crypto` - 核心加密函数
- `openssl.api.evp` - 高级加密 API
- `openssl.api.bio` - I/O 抽象
- `openssl.api.err` - 错误处理
- `abstract.intf` / `abstract.types` - 接口定义

**P1（高优先级）- 必需：**
- 对称加密：`aes`, `chacha`, `des`
- 哈希：`sha`, `md`
- 非对称加密：`rsa`, `ec`, `ecdsa`
- MAC/KDF：`hmac`, `kdf`
- PKI 基础：`x509`, `pem`
- SSL/TLS：`ssl`

**P2（中优先级）- 重要：**
- 高级 PKI：`pkcs7`, `pkcs12`, `cms`, `ocsp`, `ct`, `ts`
- 附加密码：`aria`, `camellia`, `seed`
- 附加哈希：`blake2`, `sha3`, `sm3`
- 基础设施：`store`, `comp`, `engine`

**P3+（低优先级）- 可选：**
- 传统算法（Blowfish、Whirlpool 等）
- 专用功能（SRP、async、providers）
- 工具（txt_db、ui、conf、param）

**测试覆盖率：**
- P0：100%（6/6 模块）
- P1：100%（14/14 模块）
- P2：36%（4/11 模块）- PKCS 系列测试待完成
- P3：100%（15/15 模块）

**测试状态说明**：
- 核心功能（P0/P1）：完全验证，生产可用
- 高级功能（P2）：部分验证，PKCS#7/12、CMS、OCSP 等模块测试进行中
- 辅助功能（P3）：完全验证

---

## 重要文件和文档

### 必读文档（按顺序）

1. **README.md** - 项目概述、特性、快速入门示例
2. **WORKING.md** - 最近开发历史和当前会话上下文
3. **WARP.md** - 开发标准和协作指南
4. **PROJECT_STATUS_2025-10-02.md** - 完整项目状态快照

### 架构文档

- **fafafa.ssl.abstract.intf**（src/）- 带详细注释的接口定义
- **ARCHITECTURE_FILE_ORGANIZATION.md** - 文件结构说明
- **OPENSSL3_COMPATIBILITY_STRATEGY.md** - OpenSSL 3.x 迁移方法

### 测试文档

- **tests/run_core_tests.ps1** - 主测试运行脚本
- **TESTING_README.md** - 测试策略和指南
- **TEST_PLAN.md** - 测试覆盖路线图
- docs/ 和根目录中的各种测试报告

### 模块参考

- **OPENSSL_MODULES.md** - OpenSSL 模块清单
- **MODULE_INVENTORY.md** - 完整模块列表及状态
- **CURRENT_STATUS.md** - 最新模块验证状态

### 阶段文档

- **PHASE2_WINSSL_ACTION_PLAN.md** - WinSSL 开发路线图
- **PHASE2_2_COMPLETION_REPORT.md** - WinSSL Phase 2.2 完成详情
- **WINSSL_PROGRESS_REPORT_PHASE3.md** - WinSSL 后续步骤

---

## 故障排除

### "未找到 OpenSSL" 错误

```pascal
// 检查 DLL 位置
// Windows：libcrypto-3-x64.dll、libssl-3-x64.dll 在 bin/ 或 PATH 中
// Linux：libcrypto.so.3、libssl.so.3 在 /usr/lib 或 LD_LIBRARY_PATH 中
// macOS：libcrypto.3.dylib、libssl.3.dylib 在 /usr/local/lib 中

// 验证加载
if not LoadOpenSSLCore then
  WriteLn('错误：', GetLastOSError);
```

### 测试失败并显示 "访问冲突"

通常由调用未加载的函数引起：

```pascal
// 修复：首先调用适当的 Load 函数
LoadOpenSSLCore;         // 核心函数
LoadEVP(GetCryptoLibHandle);  // EVP 函数
LoadSSL(GetSSLLibHandle);     // SSL 函数
```

### Unicode 编译错误

在 Windows 上添加代码页指令：

```pascal
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}
```

### Windows 上 WinSSL 不可用

检查 Windows 版本和 Schannel 可用性：

```pascal
Lib := CreateWinSSLLibrary();
if not Lib.Initialize then
  WriteLn('WinSSL 需要 Windows Vista 或更高版本');
```

---

## 项目历史背景

**最近的里程碑**（在进行更改时的背景）：

- **2025-10-06**：Phase 2.2 完成 - WinSSL 核心组件（Library、Context、Connection）完全实现
- **2025-10-06**：接口类型冲突解决 - 修复 TSSLProtocolVersion 枚举定义
- **2025-10-06**：WinSSL Utils 完成 - 100% 测试通过（29/29 测试）
- **2025-10-04**：OpenSSL 1.1.x 兼容性验证 - 100% 向后兼容
- **2025-10-04**：Phase 3 系统测试完成 - 96.3% 通过率（26/27 模块）
- **2025-10-02**：核心算法验证 - P0/P1/P3 100% 测试通过
- **2025-09-30**：Phase 1 完成 - SHA3/CMAC EVP 迁移

**开发理念**：
- TDD 必须遵守（先测试，后实现）
- 向后兼容（支持 OpenSSL 1.1.x 和 3.x）
- 跨平台（Windows/Linux/macOS）
- 生产质量（广泛测试、清晰错误处理）
- 多后端（OpenSSL、WinSSL、未来 MbedTLS）

---

## 当你需要帮助时

1. 查看 **WORKING.md** 了解当前会话上下文
2. 查阅 **README.md** 的使用示例
3. 查看类似模块的模式（例如 `openssl.api.aes.pas`）
4. 检查测试文件的 API 使用（`tests/test_*.pas`）
5. 在文档文件中搜索特定主题
6. 在调用函数之前验证模块是否已加载

**要遵循的常见模式：**
- 查看 `fafafa.ssl.openssl.api.aes.pas` 了解 OpenSSL API 绑定结构
- 查看 `fafafa.ssl.winssl.lib.pas` 了解接口实现模式
- 查看 `tests/test_aes.pas` 了解测试结构
- 查看 `WARP.md` 了解开发标准

---

**记住**：这是一个多后端框架，不仅仅是 OpenSSL 绑定。在进行更改时始终考虑抽象接口层，并在适用的情况下使用 OpenSSL 和 WinSSL 后端进行测试。
