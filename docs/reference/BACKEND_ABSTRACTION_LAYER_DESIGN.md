# 后端抽象层设计文档

> **Batch**: B73
> **Status**: draft
> **Created**: 2026-02-07

## 概述

本文档描述 `fafafa.ssl` 的后端抽象层设计：不同 TLS 实现共享一套
public Pascal surface，由 builder / factory / base interfaces 把
OpenSSL、WolfSSL、MbedTLS、WinSSL、FreePascal 这些后端收敛到统一入口。

> 当前说明：
> `FreePascal` 不是 future backend，当前已是活跃 backend；当前 capability truth 统一以 `docs/BACKEND_CAPABILITY_MATRIX.md` 为准。
>
> 这份设计文档只保留抽象关系与接入方式，不再重复维护一套容易漂移的 capability 大表。

## 架构概览

```text
┌────────────────────────────────────────────────────────────────────┐
│                           Application                              │
├────────────────────────────────────────────────────────────────────┤
│          TSSLContextBuilder / TSSLConnectionBuilder               │
├────────────────────────────────────────────────────────────────────┤
│                 TSSLFactory / backend selector                    │
├────────────────────────────────────────────────────────────────────┤
│   fafafa.ssl.base / fafafa.ssl / optional public owner surfaces   │
├────────────────────────────────────────────────────────────────────┤
│ OpenSSL │ WolfSSL │ MbedTLS │ WinSSL │ FreePascal                 │
│ backend │ backend │ backend │ backend │ backend                   │
└────────────────────────────────────────────────────────────────────┘
```

## 核心接口

这里仅保留抽象层关注的最小骨架；精确签名以
`docs/reference/API_REFERENCE.md` 和 `src/fafafa.ssl.base.pas` 为准。

### ISSLLibrary

```pascal
ISSLLibrary = interface
  function GetLibraryType: TSSLLibraryType;
  function GetCapabilities: TSSLBackendCapabilities;
  function CreateContext(AType: TSSLContextType): ISSLContext;
end;
```

### ISSLContext

```pascal
ISSLContext = interface
  procedure SetProtocolVersions(AVersions: TSSLProtocolVersions);
  procedure LoadCertificate(const AFileName: string);
  procedure SetVerifyMode(AMode: TSSLVerifyModes);
  function CreateConnection(ASocket: THandle): ISSLConnection;
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
  function GetProtocolVersion: TSSLProtocolVersion;
end;
```

## 后端注册与接入

### 自动注册

```pascal
initialization
  TSSLFactory.RegisterLibrary(
    sslOpenSSL,
    TOpenSSLLibrary,
    'OpenSSL',
    100
  );
```

### 显式指定后端

```pascal
Ctx := TSSLContextBuilder.Create
  .WithBackend(sslMbedTLS)
  .BuildClient;
```

### 自动选择后端

```pascal
var
  Requirements: TSSLRequirements;
begin
  Requirements := CreateDefaultRequirements(optBalanced);
  Requirements.RequiredProtocols := [sslProtocolTLS13];
  Requirements.RequiredFeatures := [sslFeatOCSPStapling];

  Ctx := TSSLContextBuilder.Create
    .WithAutoBackendSelection(Requirements)
    .BuildClient;
end;
```

## 抽象层当前真值

- selector / builder 会读取当前 runtime-aware capability fields，而不是历史文档里的静态想象表。
- `WinSSL` 当前 `OCSPStaplingSupport=sslSupportNone`、`EarlyDataSupport=sslSupportNone`；设计层不能再把平台潜力写成已发布 capability。
- `FreePascal` 当前是活跃 native backend；其 `ALPN` / `SNI` / `Session Resumption` / `Early Data` / `OCSP Stapling` 仍要按 capability level 判断，而不是直接当成稳定 `✅`。
- 需要 backend 细粒度真值时，统一下钻：
  - `docs/BACKEND_CAPABILITY_MATRIX.md`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  - `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`

## 实现状态

| 后端 | 当前状态 | 说明 |
|------|----------|------|
| OpenSSL | ✅ 活跃 backend | 默认 capability 真值见 canonical matrix |
| WolfSSL | ✅ 活跃 backend | 早数据 / OCSP 等能力按 helper/runtime gate 决定 |
| MbedTLS | ✅ 活跃 backend | 某些能力仍明确不发布 public surface |
| WinSSL | ✅ 活跃 backend | Windows-only；0-RTT / server OCSP 当前不发布 |
| FreePascal | ✅ 活跃 backend | 纯 Pascal / native implementation |

## 扩展新后端

1. 创建 `fafafa.ssl.<backend>.*.pas` 单元
2. 实现 `ISSLLibrary` / `ISSLContext` / `ISSLConnection` 所需 surface
3. 明确 `GetCapabilities` 的 runtime truth
4. 在初始化阶段注册到 `TSSLFactory`
5. 同步补齐：
   - canonical matrix
   - dedicated backend doc
   - focused contract

## 相关文档

- `src/fafafa.ssl.base.pas` - 抽象接口定义
- `src/fafafa.ssl.factory.pas` - 工厂实现
- `src/fafafa.ssl.backend.selector.pas` - 自动选择实现
- `docs/BACKEND_CAPABILITY_MATRIX.md` - canonical capability truth
- `docs/BACKEND_SELECTION_GUIDE.md` - 自动选择对外用法
- `docs/reference/BACKEND_SELECTOR_DESIGN.md` - selector 设计说明
