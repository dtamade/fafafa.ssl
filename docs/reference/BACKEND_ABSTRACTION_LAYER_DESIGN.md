# 后端抽象层设计文档

> **Batch**: B73
> **Status**: draft
> **Created**: 2026-02-07

## 概述

本文档描述 fafafa.ssl 的后端抽象层设计，该层允许在 OpenSSL、MbedTLS、WinSSL 等不同 TLS 实现之间无缝切换。

## 架构概览

```
┌─────────────────────────────────────────────────────────────┐
│                    应用层 (Application)                      │
├─────────────────────────────────────────────────────────────┤
│                  TSSLContextBuilder (Fluent API)            │
├─────────────────────────────────────────────────────────────┤
│                    TSSLFactory (工厂模式)                    │
├─────────────────────────────────────────────────────────────┤
│                  fafafa.ssl.base (抽象接口)                  │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────────┐   │
│  │ISSLContext│ │ISSLConn  │ │ISSLCert  │ │ISSLCertStore │   │
│  └──────────┘ └──────────┘ └──────────┘ └──────────────┘   │
├─────────────────────────────────────────────────────────────┤
│                    后端实现层 (Backends)                     │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────────┐   │
│  │ OpenSSL  │ │ MbedTLS  │ │ WinSSL   │ │ FreePascal   │   │
│  │ Backend  │ │ Backend  │ │ Backend  │ │ (Future)     │   │
│  └──────────┘ └──────────┘ └──────────┘ └──────────────┘   │
├─────────────────────────────────────────────────────────────┤
│                    原生库层 (Native Libraries)               │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐                    │
│  │libssl.so │ │libmbedtls│ │Schannel  │                    │
│  │libcrypto │ │.so/.dll  │ │.dll      │                    │
│  └──────────┘ └──────────┘ └──────────┘                    │
└─────────────────────────────────────────────────────────────┘
```

## 核心接口

### ISSLLibrary

```pascal
ISSLLibrary = interface
  function GetLibraryType: TSSLLibraryType;
  function GetVersion: string;
  function Initialize: Boolean;
  procedure Finalize;
  function IsInitialized: Boolean;
  function GetCapabilities: TSSLCapabilities;
  function CreateContext(AType: TSSLContextType): ISSLContext;
  function CreateCertificate: ISSLCertificate;
  function CreateCertificateStore: ISSLCertificateStore;
end;
```

### ISSLContext

```pascal
ISSLContext = interface
  function GetContextType: TSSLContextType;
  procedure SetProtocolVersions(AVersions: TSSLProtocolVersions);
  procedure LoadCertificate(const AFileName: string);
  procedure LoadPrivateKey(const AFileName: string; const APassword: string = '');
  procedure SetVerifyMode(AMode: TSSLVerifyModes);
  function CreateConnection(ASocket: TSocket): ISSLConnection;
end;
```

### ISSLConnection

```pascal
ISSLConnection = interface
  function Connect: Boolean;
  function Accept: Boolean;
  function Read(var ABuffer; ACount: Integer): Integer;
  function Write(const ABuffer; ACount: Integer): Integer;
  function Shutdown: Boolean;
  function GetPeerCertificate: ISSLCertificate;
  function GetNegotiatedProtocol: TSSLProtocolVersion;
end;
```

## 后端注册机制

### 自动注册

```pascal
// 在单元初始化时自动注册
initialization
  TSSLFactory.RegisterLibrary(
    sslOpenSSL,
    TOpenSSLLibrary,
    'OpenSSL',
    100  // 优先级
  );
```

### 手动选择

```pascal
// 显式指定后端
Ctx := TSSLFactory.CreateContext(sslClient, sslMbedTLS);

// 或通过 Builder
Ctx := TSSLContextBuilder.Create
  .WithBackend(sslMbedTLS)
  .BuildClient;
```

## 后端选择策略

### 默认策略

1. **Windows**: WinSSL > OpenSSL > MbedTLS
2. **Linux/macOS**: OpenSSL > MbedTLS
3. **嵌入式**: MbedTLS > OpenSSL

### 功能需求匹配

```pascal
// 根据功能需求自动选择
Ctx := TSSLContextBuilder.Create
  .WithRequirements([brTLS13, brOCSPStapling])
  .BuildClient;
// 自动选择支持这些功能的后端
```

## 能力矩阵

| 功能 | OpenSSL | MbedTLS | WinSSL |
|------|---------|---------|--------|
| TLS 1.3 | ✅ | ✅ | ✅* |
| OCSP Stapling | ✅ | ❌ | ❌ |
| Session Ticket | ✅ | ✅ | ⚠️ |
| PSK | ✅ | ✅ | ❌ |
| Ed25519 | ✅ | ⚠️ | ❌ |
| FIPS | ✅ | ❌ | ✅ |
| 系统证书存储 | ⚠️ | ⚠️ | ✅ |

*需要 Windows 10 1903+

## 实现状态

| 后端 | 状态 | 测试覆盖 |
|------|------|----------|
| OpenSSL | ✅ 完成 | 157/157 模块 |
| MbedTLS | ✅ 完成 | 73/73 框架测试 |
| WinSSL | ⚠️ 部分 | 待验证 |
| FreePascal | ✅ 已实现 | 活跃 backend，见 `docs/BACKEND_CAPABILITY_MATRIX.md` |

## 扩展新后端

### 步骤

1. 创建 `fafafa.ssl.<backend>.*.pas` 单元
2. 实现 `ISSLLibrary` 接口
3. 实现 `ISSLContext` 接口
4. 实现 `ISSLConnection` 接口
5. 在初始化时注册到工厂

### 示例

```pascal
unit fafafa.ssl.newbackend.lib;

type
  TNewBackendLibrary = class(TInterfacedObject, ISSLLibrary)
  public
    function GetLibraryType: TSSLLibraryType;
    function Initialize: Boolean;
    // ... 其他方法
  end;

initialization
  TSSLFactory.RegisterLibrary(
    sslNewBackend,
    TNewBackendLibrary,
    'New Backend',
    50
  );
```

## 相关文档

- `src/fafafa.ssl.base.pas` - 抽象接口定义
- `src/fafafa.ssl.factory.pas` - 工厂实现
- `docs/reference/BACKEND_SELECTOR_DESIGN.md` - 选择器设计
- `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md` - MbedTLS 能力
- `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md` - WinSSL 能力
