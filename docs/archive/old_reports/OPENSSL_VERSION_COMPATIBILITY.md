# OpenSSL 版本兼容性指南

## 概述

本 Pascal OpenSSL 绑定库支持 OpenSSL 1.1.x 和 OpenSSL 3.x 两个主要版本，并提供智能的自动版本检测和加载机制。

## 支持的版本

### OpenSSL 3.x (推荐)
- **库文件**: 
  - Windows x64: `libcrypto-3-x64.dll`, `libssl-3-x64.dll`
  - Windows x86: `libcrypto-3.dll`, `libssl-3.dll`
  - Linux: `libcrypto.so.3`, `libssl.so.3`
  - macOS: `libcrypto.3.dylib`, `libssl.3.dylib`
- **状态**: ✅ 完全支持
- **优先级**: 第一优先级（自动加载时优先尝试）

### OpenSSL 1.1.x (向后兼容)
- **库文件**:
  - Windows x64: `libcrypto-1_1-x64.dll`, `libssl-1_1-x64.dll`
  - Windows x86: `libcrypto-1_1.dll`, `libssl-1_1.dll`
  - Linux: `libcrypto.so.1.1`, `libssl.so.1.1`
  - macOS: `libcrypto.1.1.dylib`, `libssl.1.1.dylib`
- **状态**: ✅ 完全支持
- **优先级**: 第二优先级（3.x 加载失败时回退）

## 使用方法

### 方式 1: 自动检测（推荐）

```pascal
uses
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.consts;

begin
  // 自动检测并加载可用的 OpenSSL 版本
  LoadOpenSSLCore;
  
  // 查询加载的版本
  WriteLn('Loaded: ', GetOpenSSLVersionString);
  
  // 使用 OpenSSL 功能...
  
  // 清理
  UnloadOpenSSLCore;
end.
```

**工作原理**:
1. 首先尝试加载 OpenSSL 3.x
2. 如果 3.x 不可用，自动回退到 1.1.x
3. 如果两个版本都不可用，抛出异常

### 方式 2: 指定版本

```pascal
uses
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.consts;

begin
  // 明确指定要加载的版本
  LoadOpenSSLCoreWithVersion(sslVersion_3_0);
  // 或
  LoadOpenSSLCoreWithVersion(sslVersion_1_1);
  
  // 使用 OpenSSL 功能...
  
  UnloadOpenSSLCore;
end.
```

### 版本查询 API

```pascal
// 获取当前加载的版本（枚举）
function GetOpenSSLVersion: TOpenSSLVersion;
// 返回: sslUnknown, sslVersion_1_1, 或 sslVersion_3_0

// 获取版本字符串（友好格式）
function GetOpenSSLVersionString: string;
// 返回示例: "3.x (libcrypto-3-x64.dll)"

// 检查是否已加载
function IsOpenSSLCoreLoaded: Boolean;
```

## API 兼容性

### 完全兼容的模块

以下模块在 OpenSSL 1.1.x 和 3.x 中 API 完全一致：

- ✅ **RAND** - 随机数生成
- ✅ **ERR** - 错误处理
- ✅ **BIO** - I/O 抽象
- ✅ **SHA** - SHA 哈希算法
- ✅ **MD5** - MD5 哈希
- ✅ **MD** - 消息摘要
- ✅ **EVP** - 高级加密 API
- ✅ **AES** - AES 对称加密
- ✅ **RSA** - RSA 非对称加密
- ✅ **HMAC** - 消息认证码
- ✅ **DH** - Diffie-Hellman 密钥交换
- ✅ **DSA** - 数字签名算法
- ✅ **BN** - 大数运算
- ✅ **ECDH** - 椭圆曲线 Diffie-Hellman
- ✅ **DES** - DES/3DES 加密
- ✅ **SEED** - SEED 块加密

### 有差异的模块

以下模块在不同版本间有 API 变化：

#### ⚠️ ECDSA
- **OpenSSL 1.1.x**: 使用传统的 `ECDSA_SIG` 结构
- **OpenSSL 3.x**: 部分内部函数已弃用
- **建议**: 使用 EVP API 替代直接调用 ECDSA 函数
- **状态**: 当前存在运行时兼容性问题（待修复）

#### 📝 其他变化
- OpenSSL 3.x 引入了新的 Provider 架构
- 某些旧的低级 API 在 3.x 中被标记为弃用（但仍可用）
- 建议优先使用 EVP 高级 API 以确保最佳兼容性

## 部署建议

### 开发环境

建议同时安装两个版本用于测试：

```
项目目录/bin/
├── libcrypto-3-x64.dll      # OpenSSL 3.x
├── libssl-3-x64.dll
├── libcrypto-1_1-x64.dll    # OpenSSL 1.1.x
└── libssl-1_1-x64.dll
```

### 生产环境

**推荐方式**: 只部署 OpenSSL 3.x
- 更新的加密算法支持
- 更好的安全性
- 更好的性能

**兼容方式**: 同时部署两个版本
- 最大兼容性
- 适用于旧系统环境

## 测试覆盖

当前测试状态（16/72 模块，22.2%）:

| OpenSSL 版本 | 测试状态 | 通过率 |
|-------------|---------|--------|
| OpenSSL 3.x | ✅ 132/134 | 98.5% |
| OpenSSL 1.1.x | ✅ 132/134 | 98.5% |

## 故障排查

### 问题: "Failed to load any OpenSSL library"

**原因**: 系统中没有找到任何兼容的 OpenSSL 库

**解决方案**:
1. 确认 DLL 文件在程序目录或系统 PATH 中
2. 检查文件名是否正确（区分 x64/x86）
3. 使用 Dependency Walker 检查 DLL 依赖

### 问题: 某些功能在 OpenSSL 3.x 中不工作

**原因**: API 已被弃用或改变

**解决方案**:
1. 查看 OpenSSL 3.x 迁移指南
2. 尝试使用 EVP 高级 API 替代
3. 临时使用 `LoadOpenSSLCoreWithVersion(sslVersion_1_1)` 加载 1.1.x

### 问题: 版本自动检测选择了错误的版本

**解决方案**: 使用 `LoadOpenSSLCoreWithVersion()` 明确指定版本

```pascal
// 强制使用 OpenSSL 1.1.x
LoadOpenSSLCoreWithVersion(sslVersion_1_1);
```

## 未来计划

- [ ] 修复 ECDSA 模块的 OpenSSL 3.x 兼容性
- [ ] 添加更多版本特定的功能检测
- [ ] 提供版本差异的详细文档
- [ ] 完善所有模块的双版本测试

## 参考资料

- [OpenSSL 3.0 迁移指南](https://www.openssl.org/docs/man3.0/man7/migration_guide.html)
- [OpenSSL 1.1.1 文档](https://www.openssl.org/docs/man1.1.1/)
- [OpenSSL 3.x 文档](https://www.openssl.org/docs/man3.0/)

---

**最后更新**: 2025-09-30  
**测试环境**: Windows 11, Free Pascal 3.3.1, OpenSSL 1.1.1 & 3.4.0