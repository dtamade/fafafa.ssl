# fafafa.ssl - 快速入门指南

> **最后更新**: 2025-10-24  
> **状态**: v0.8 功能完成，进入 v0.9 RC 开发阶段 🚀

## 📋 项目简介

`fafafa.ssl` 是一个现代化的 Pascal SSL/TLS 库，提供统一的抽象接口，支持多种后端实现：

- **OpenSSL** - 完整的加密功能（AES, RSA, X.509, PKCS#7/12, CMS等）
- **WinSSL (Schannel)** - Windows 原生 SSL/TLS，支持企业级功能

## ⚡ 5 分钟上手

### 前置条件

- **Free Pascal Compiler (FPC)** 3.2.0+
- **Lazarus IDE** 2.0+ (可选)
- **OpenSSL** 1.1.1+ 或 3.x (Windows: 将 DLL 放到 PATH 或程序目录)

### 安装

```bash
git clone https://github.com/dtamade/fafafa.ssl.git
cd fafafa.ssl
```

### 第一个程序：OpenSSL 版本检测

创建 `hello_ssl.pas`：

```pascal
program hello_ssl;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl,
  fafafa.ssl.openssl.api;

var
  LVersion: string;
begin
  WriteLn('=== fafafa.ssl 快速测试 ===');
  WriteLn;
  
  // 加载 OpenSSL 库
  if LoadOpenSSLLibrary then
  begin
    LVersion := GetOpenSSLVersion;
    WriteLn('✓ OpenSSL 加载成功');
    WriteLn('  版本: ', LVersion);
    WriteLn;
    WriteLn('后端支持:');
    WriteLn('  - OpenSSL: 可用');
    {$IFDEF WINDOWS}
    WriteLn('  - WinSSL:  可用');
    {$ENDIF}
  end
  else
  begin
    WriteLn('✗ OpenSSL 加载失败');
    WriteLn('  请确保 OpenSSL DLL 在系统路径中');
    ExitCode := 1;
  end;
end.
```

**编译运行**：

```bash
# 编译
fpc -Fusrc -Fusrc\openssl hello_ssl.pas

# 运行
hello_ssl.exe
```

**预期输出**：

```
=== fafafa.ssl 快速测试 ===

✓ OpenSSL 加载成功
  版本: OpenSSL 3.4.1 11 Feb 2025
  
后端支持:
  - OpenSSL: 可用
  - WinSSL:  可用
```

## 🎯 核心功能示例

### 1. SHA-256 哈希计算

```pascal
program hash_example;
uses
  fafafa.ssl.openssl.api.evp;
  
var
  LData: TBytes;
  LHash: TBytes;
begin
  LData := BytesOf('Hello, fafafa.ssl!');
  LHash := EVP_Digest(LData, EVP_sha256());
  WriteLn('SHA-256: ', BytesToHex(LHash));
end.
```

### 2. AES 加密/解密

```pascal
program aes_example;
uses
  fafafa.ssl.openssl.api.evp.cipher;
  
var
  LKey, LIV, LPlainText, LCipherText, LDecrypted: TBytes;
begin
  // 设置密钥和 IV (实际应用中应使用随机生成)
  SetLength(LKey, 32);  // AES-256
  SetLength(LIV, 16);
  
  LPlainText := BytesOf('Secret Message');
  
  // 加密
  LCipherText := EncryptData(LPlainText, LKey, LIV, EVP_aes_256_cbc());
  WriteLn('Encrypted: ', BytesToHex(LCipherText));
  
  // 解密
  LDecrypted := DecryptData(LCipherText, LKey, LIV, EVP_aes_256_cbc());
  WriteLn('Decrypted: ', BytesToString(LDecrypted));
end.
```

### 3. RSA 密钥生成和签名

```pascal
program rsa_example;
uses
  fafafa.ssl.openssl.api.rsa,
  fafafa.ssl.openssl.api.evp.pkey;
  
var
  LKeyPair: PEVP_PKEY;
  LData, LSignature: TBytes;
begin
  // 生成 2048 位 RSA 密钥对
  LKeyPair := GenerateRSAKey(2048);
  
  LData := BytesOf('Document to sign');
  
  // 签名
  LSignature := SignData(LData, LKeyPair, EVP_sha256());
  WriteLn('Signature: ', BytesToHex(LSignature));
  
  // 验证
  if VerifySignature(LData, LSignature, LKeyPair, EVP_sha256()) then
    WriteLn('✓ Signature valid')
  else
    WriteLn('✗ Signature invalid');
    
  EVP_PKEY_free(LKeyPair);
end.
```

### 4. X.509 证书加载和验证 (WinSSL)

```pascal
program cert_example;
uses
  fafafa.ssl.winssl.factory,
  fafafa.ssl.abstract.intf;
  
var
  LFactory: ISSLLibrary;
  LCert: ISSLCertificate;
  LStore: ISSLCertificateStore;
  LResult: TSSLCertVerifyResult;
begin
  // 创建 WinSSL 实例
  LFactory := CreateWinSSLLibrary;
  
  // 从文件加载证书
  LCert := LFactory.LoadCertificateFromFile('mycert.pem');
  
  // 获取系统根证书存储
  LStore := LFactory.OpenSystemStore('ROOT');
  
  // 增强验证（包括吊销检查）
  if LCert.VerifyEx(LStore, 
       [sslCertVerifyCheckRevocation, sslCertVerifyCheckOCSP], 
       LResult) then
    WriteLn('✓ 证书验证成功')
  else
    WriteLn('✗ 证书验证失败: ', LResult.ErrorMessage);
end.
```

## 📚 完整示例程序

在 `examples/` 目录下有 40+ 个完整的示例程序：

```bash
examples/
├── basic/
│   ├── 01_hello_openssl.lpr        # OpenSSL 基础
│   ├── 02_hash_sha256.lpr          # 哈希计算
│   ├── 03_symmetric_aes.lpr        # AES 加密
│   └── 04_asymmetric_rsa.lpr       # RSA 密钥操作
├── advanced/
│   ├── certificate_validation.lpr  # 证书验证
│   ├── pkcs7_sign_verify.lpr      # PKCS#7 签名
│   └── tls_client.lpr             # TLS 客户端
└── winssl/
    ├── system_cert_store.lpr      # 系统证书存储
    ├── enterprise_config.lpr      # 企业配置
    └── fips_detection.lpr         # FIPS 模式检测
```

**编译示例**：

```bash
cd examples\basic
lazbuild 01_hello_openssl.lpi
01_hello_openssl.exe
```

## 🧪 运行测试

项目包含 150+ 个自动化测试：

```powershell
# 运行全部测试
.\run_all_tests.ps1

# 查看测试结果
cat tests\bin\test_results.txt
```

**当前测试覆盖率**：
- **Priority 1 (核心)**: 97.9% (47/48 通过)
- **Priority 2 (扩展)**: 93.6% (44/47 通过)
- **Priority 3 (特定)**: 95.7% (22/23 通过)

## 🏗️ Lazarus 包集成

在 Lazarus IDE 中使用：

1. **打开包**: `Package` → `Open Package File (.lpk)` → 选择 `fafafa_ssl.lpk`
2. **编译**: 点击 `Compile`
3. **使用**: 在项目中添加 `fafafa_ssl` 到依赖

## 📖 深入学习

- **API 参考**: 查看 `src/` 目录下各模块的接口文档
- **架构设计**: `ARCHITECTURE_FILE_ORGANIZATION.md`
- **测试指南**: `README_TESTING.md`
- **项目愿景**: `PROJECT_VISION.md`

## 🆕 v0.8 新特性 (Phase A & B)

### Phase A: OpenSSL 模块完善
- ✅ PKCS#7/12 模块验证完成 (100%)
- ✅ CMS 模块修复，测试通过率 95%+
- ✅ 证书服务模块覆盖率 92.3%

### Phase B: WinSSL 企业级功能
- ✅ 增强证书验证 (`VerifyEx` 方法)
  - 吊销检查 (CRL/OCSP)
  - 详细错误报告
  - 证书链验证
- ✅ 企业功能集成
  - FIPS 模式检测
  - 组策略读取
  - 企业 CA 信任管理
- ✅ 友好错误处理
  - 中英文双语错误消息
  - 结构化日志系统

## 🚀 v0.9 RC 路线图 (进行中)

### Phase C: 代码重构 (当前)
- 🔄 拆分大文件 (`fafafa.ssl.openssl.pas` 10,000+ 行)
- 🔄 统一代码风格 (WARP.md 规范)
- 🔄 优化单元组织结构

### Phase D-G: 未来计划
- **Phase D**: 文档完善（快速入门、API 参考、用户指南）
- **Phase E**: 示例应用（10+ 实际场景）
- **Phase F**: 跨平台测试（Linux、macOS）
- **Phase G**: 性能优化与 CI/CD

## ❓ 常见问题

**Q: 如何选择后端？**  
A: OpenSSL 提供最完整的功能；WinSSL 适合 Windows 企业环境，无需额外依赖。

**Q: 支持哪些 OpenSSL 版本？**  
A: 官方测试 1.1.1 和 3.x，推荐使用 3.4.1。

**Q: 线程安全吗？**  
A: OpenSSL 3.x 默认线程安全；WinSSL 需注意证书上下文的线程访问。

**Q: 如何报告问题？**  
A: 在 GitHub Issues 提交，或查看 `docs/` 目录下的详细文档。

## 🤝 贡献

欢迎贡献代码、文档或测试！请参考：
- **代码规范**: `WARP.md`
- **测试指南**: `TESTING_STRATEGY.md`
- **提交规范**: Conventional Commits (feat/fix/docs/test)

## 📄 许可证

[待定 - 请在实际使用前添加许可证信息]

---

**开始探索吧！** 🎉

如有任何问题，请查看 `docs/` 目录或提交 Issue。
