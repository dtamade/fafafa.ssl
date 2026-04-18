# fafafa.ssl 架构设计文档

**版本**: 1.1.0
**最后更新**: 2026-02-05
**状态**: 稳定 (Production Ready)

---

## 目录

1. [概述](#概述)
2. [设计原则](#设计原则)
3. [架构层次](#架构层次)
4. [核心接口](#核心接口)
5. [可选接口](#可选接口)
6. [后端架构](#后端架构)
7. [工厂模式](#工厂模式)
8. [扩展性设计](#扩展性设计)
9. [v1.1 架构改进](#v11-架构改进)

---

## 概述

fafafa.ssl 是一个多后端 TLS/SSL 库，为 Free Pascal 提供统一的高级 API，同时支持多种底层 TLS 实现（OpenSSL, WinSSL, MbedTLS, WolfSSL）。

### 核心特性

- **多后端支持** - 单一 API，多种后端可选
- **接口驱动** - 完全基于接口的设计，无全局状态
- **工厂模式** - 自动检测和加载最佳后端
- **类型安全** - 编译时和运行时类型检查
- **零依赖部署** - 可静态链接所有依赖
- **跨平台** - Linux, Windows, macOS, FreeBSD

---

## 设计原则

### 1. 接口抽象优先

**原则**: 用户代码仅依赖接口，不依赖具体实现。

```pascal
// ✅ 好的设计 - 依赖接口
var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
begin
  Ctx := Factory.CreateContext(...);
  Conn := Ctx.CreateConnection(...);
end;

// ❌ 差的设计 - 依赖具体类
var
  Ctx: TOpenSSLContext;  // 紧耦合
begin
  Ctx := TOpenSSLContext.Create;
end;
```

### 2. 最少知识原则

**原则**: 接口仅暴露必要的方法，不暴露实现细节。

```pascal
// ✅ 核心接口 - 不暴露实现细节
ISSLContext = interface
  function CreateConnection(ASocket: THandle): ISSLConnection;
  // 无 GetNativeHandle - 实现细节
end;

// ✅ 可选接口 - 高级用户使用
ISSLNativeHandleAccess = interface
  function GetNativeHandle: Pointer;  // 仅需要时查询
end;
```

### 3. 开闭原则

**原则**: 对扩展开放，对修改封闭。

- 添加新后端无需修改现有代码
- 添加新功能通过可选接口扩展

### 4. 依赖倒置

**原则**: 高层模块不依赖低层模块，都依赖抽象。

```
用户代码 → ISSLContext → 后端实现
         ↓
      工厂模式
```

---

## 架构层次

```
┌─────────────────────────────────────────────────────┐
│                   用户应用层                         │
│            (HTTPS Client, Server, 等)              │
└─────────────────────────────────────────────────────┘
                         │
                         ↓
┌─────────────────────────────────────────────────────┐
│                  统一 API 层                         │
│   ISSLLibrary, ISSLContext, ISSLConnection, ...    │
└─────────────────────────────────────────────────────┘
                         │
                         ↓
┌─────────────────────────────────────────────────────┐
│                  工厂模式层                          │
│        TSSLFactory (自动检测和加载后端)             │
└─────────────────────────────────────────────────────┘
                         │
        ┌────────────────┼────────────────┐
        ↓                ↓                ↓
┌──────────────┐  ┌──────────────┐  ┌──────────────┐
│ OpenSSL 后端 │  │ WinSSL 后端  │  │ MbedTLS 后端 │
└──────────────┘  └──────────────┘  └──────────────┘
        ↓                ↓                ↓
┌──────────────┐  ┌──────────────┐  ┌──────────────┐
│  libssl.so   │  │  schannel    │  │ libmbedtls   │
│  libcrypto   │  │  (Windows)   │  │              │
└──────────────┘  └──────────────┘  └──────────────┘
```

---

## 核心接口

### 接口继承关系

```
IInterface (FreePascal 内置)
    ↑
    ├─ ISSLLibrary          (库管理)
    ├─ ISSLContext          (上下文管理)
    ├─ ISSLConnection       (连接管理)
    │   ├─ ISSLClientConnection  (客户端扩展)
    │   └─ ISSLServerConnection  (服务端扩展)
    ├─ ISSLCertificate      (证书管理)
    ├─ ISSLCertificateStore (证书存储)
    └─ ISSLSession          (会话管理)
```

### ISSLLibrary - 库管理

```pascal
ISSLLibrary = interface
  ['{GUID}']
  function Initialize: Boolean;
  procedure Finalize;
  function IsInitialized: Boolean;
  function GetVersionString: string;
  function GetLibraryType: TSSLLibraryType;

  function CreateContext(AContextType: TSSLContextType): ISSLContext;
  function CreateCertificate: ISSLCertificate;
  function CreateCertificateStore: ISSLCertificateStore;
  // ... 工厂方法
end;
```

### ISSLContext - 上下文管理

```pascal
ISSLContext = interface
  ['{GUID}']
  function CreateConnection(ASocket: THandle): ISSLConnection; overload;
  function CreateConnection(AStream: TStream): ISSLConnection; overload;

  function GetContextType: TSSLContextType;
  function IsValid: Boolean;

  procedure SetProtocolVersions(AMin, AMax: TSSLProtocolVersion);
  procedure SetVerifyMode(AMode: TSSLVerifyMode);
  procedure SetCipherList(const ACiphers: string);
  // ... 配置方法
end;
```

### ISSLConnection - 连接管理

```pascal
ISSLConnection = interface
  ['{GUID}']
  function Connect: Boolean;
  function Accept: Boolean;
  function Shutdown: Boolean;

  function Read(var ABuffer; ACount: Integer): Integer;
  function Write(const ABuffer; ACount: Integer): Integer;

  function GetState: TSSLConnectionState;
  function GetPeerCertificate: ISSLCertificate;
  // ... 连接方法
end;
```

---

## 可选接口

### ISSLNativeHandleAccess - 原生句柄访问 (v1.1+)

**设计目的**:
- 允许高级用户访问底层 C 库句柄
- 不强制所有后端实现（支持纯 Pascal 后端）

```pascal
ISSLNativeHandleAccess = interface
  ['{B2C4E6F8-1A2B-3C4D-5E6F-7A8B9C0D1E2F}']

  {** 获取后端原生句柄 *}
  function GetNativeHandle: Pointer;

  {** 获取后端类型 *}
  function GetBackendType: TSSLLibraryType;

  {** 检查原生句柄是否有效 *}
  function IsNativeHandleValid: Boolean;
end;
```

**使用模式**:

```pascal
// 检查并使用原生句柄
var
  Ctx: ISSLContext;
  NativeAccess: ISSLNativeHandleAccess;
begin
  Ctx := Factory.CreateContext(...);

  // 运行时检查是否支持
  if Supports(Ctx, ISSLNativeHandleAccess, NativeAccess) then
  begin
    // C 库后端 - 可以访问原生句柄
    Handle := NativeAccess.GetNativeHandle;
    BackendType := NativeAccess.GetBackendType;
  end
  else
  begin
    // 纯 Pascal 后端 - 无原生句柄
    WriteLn('Pure Pascal backend');
  end;
end;
```

### 其他可选接口

```pascal
// PKCS#11 硬件令牌支持
ISSLPkcs11Support = interface
  function LoadPkcs11Module(const AModulePath: string): Boolean;
  // ...
end;

// DANE/DNSSEC 支持
ISSLDaneSupport = interface
  function VerifyDaneRecord(const ADomain: string; ...): Boolean;
  // ...
end;
```

---

## 后端架构

### 后端接口实现

每个后端实现所有核心接口 + 可选接口：

```pascal
// OpenSSL 后端
TOpenSSLContext = class(TInterfacedObject,
                        ISSLContext,           // 核心
                        ISSLNativeHandleAccess) // 可选
private
  FCtx: PSSL_CTX;  // OpenSSL 原生句柄
public
  // ISSLContext 实现
  function CreateConnection(...): ISSLConnection; override;
  // ...

  // ISSLNativeHandleAccess 实现
  function GetNativeHandle: Pointer;
  function GetBackendType: TSSLLibraryType;
  function IsNativeHandleValid: Boolean;
end;

// 纯 Pascal 后端（未来）
TFreePascalSSLContext = class(TInterfacedObject, ISSLContext)
  // ✅ 仅实现 ISSLContext
  // ✅ 不实现 ISSLNativeHandleAccess
private
  FConfig: TPascalTLSConfig;  // 纯 Pascal 数据
public
  // ISSLContext 实现
  function CreateConnection(...): ISSLConnection; override;
  // ...
end;
```

### 后端文件组织

```
src/
├── fafafa.ssl.base.pas              # 核心接口定义
├── fafafa.ssl.factory.pas           # 工厂模式
│
├── fafafa.ssl.openssl.base.pas      # OpenSSL 基础定义
├── fafafa.ssl.openssl.api.*.pas     # OpenSSL API 绑定
├── fafafa.ssl.openssl.lib.pas       # ISSLLibrary 实现
├── fafafa.ssl.openssl.context.pas   # ISSLContext 实现
├── fafafa.ssl.openssl.connection.pas# ISSLConnection 实现
├── fafafa.ssl.openssl.certificate.pas# ISSLCertificate 实现
├── fafafa.ssl.openssl.native_handle.pas # 辅助函数
│
├── fafafa.ssl.winssl.*.pas          # WinSSL 后端
├── fafafa.ssl.mbedtls.*.pas         # MbedTLS 后端
└── fafafa.ssl.wolfssl.*.pas         # WolfSSL 后端
```

---

## 工厂模式

### TSSLFactory 核心功能

```pascal
TSSLFactory = class
public
  // 后端注册
  class procedure RegisterLibrary(
    AType: TSSLLibraryType;
    AClass: TSSLLibraryClass;
    const AName: string;
    APriority: Integer
  );

  // 自动检测
  class function DetectBestLibrary: TSSLLibraryType;

  // 创建实例
  class function CreateLibrary(
    AType: TSSLLibraryType = sslAutoDetect
  ): ISSLLibrary;

  class function CreateContext(
    AContextType: TSSLContextType;
    ALibType: TSSLLibraryType = sslAutoDetect
  ): ISSLContext;
end;
```

### 后端优先级

```
优先级顺序（Linux）:
1. OpenSSL (优先级 10) - 最成熟
2. MbedTLS (优先级 7)  - 轻量级
3. WolfSSL (优先级 5)  - 嵌入式优化

优先级顺序（Windows）:
1. WinSSL  (优先级 10) - 系统原生
2. OpenSSL (优先级 9)  - 兼容性好
3. MbedTLS (优先级 7)  - 备选
```

---

## 扩展性设计

### 1. 添加新后端

步骤：

1. **创建基础单元**:
   ```pascal
   unit fafafa.ssl.newbackend.base;
   // 类型定义和常量
   ```

2. **实现核心接口**:
   ```pascal
   unit fafafa.ssl.newbackend.lib;
   type
     TNewBackendSSLLibrary = class(TInterfacedObject, ISSLLibrary)
       // 实现所有 ISSLLibrary 方法
     end;
   ```

3. **注册后端**:
   ```pascal
   initialization
     TSSLFactory.RegisterLibrary(
       sslNewBackend,
       TNewBackendSSLLibrary,
       'NewBackend TLS',
       8  // 优先级
     );
   ```

4. **可选实现 ISSLNativeHandleAccess**（如果基于 C 库）

### 2. 添加新功能

通过可选接口扩展：

```pascal
// 定义新接口
ISSLAdvancedFeature = interface
  ['{NEW-GUID}']
  function DoAdvancedThing: Boolean;
end;

// 在支持的后端实现
TOpenSSLContext = class(..., ISSLAdvancedFeature)
  function DoAdvancedThing: Boolean;
end;

// 用户代码检查并使用
if Supports(Ctx, ISSLAdvancedFeature, AdvFeature) then
  AdvFeature.DoAdvancedThing;
```

---

## v1.1 架构改进

### 改进前（v1.0.0）

**问题**:
- `GetNativeHandle` 在核心接口中
- 所有后端必须实现（即使是纯 Pascal 后端）
- 暴露了实现细节

```pascal
ISSLContext = interface
  function GetNativeHandle: Pointer;  // ❌ 所有后端必须实现
end;

// 纯 Pascal 后端被迫返回 nil
function TPascalContext.GetNativeHandle: Pointer;
begin
  Result := nil;  // ❌ 无意义的实现
end;
```

### 改进后（v1.1.0）

**解决方案**:
- 移除核心接口中的 `GetNativeHandle`
- 创建可选接口 `ISSLNativeHandleAccess`
- C 库后端实现，纯 Pascal 后端忽略

```pascal
// 核心接口 - 清晰
ISSLContext = interface
  // 无 GetNativeHandle
end;

// 可选接口 - 明确
ISSLNativeHandleAccess = interface
  function GetNativeHandle: Pointer;
end;

// C 库后端实现
TOpenSSLContext = class(..., ISSLNativeHandleAccess)
  function GetNativeHandle: Pointer;  // ✅ 返回真实句柄
end;

// 纯 Pascal 后端忽略
TPascalContext = class(..., ISSLContext)
  // ✅ 无需实现 GetNativeHandle
end;
```

### 架构优势

| 方面 | v1.0.0 | v1.1.0 |
|------|--------|--------|
| **抽象清晰度** | ❌ 核心接口暴露实现细节 | ✅ 核心接口纯粹抽象 |
| **纯 Pascal 支持** | ❌ 被迫实现无意义方法 | ✅ 无需实现不相关接口 |
| **类型安全** | ⚠️ 用户可能误用 | ✅ Supports 查询强制检查 |
| **扩展性** | ⚠️ 添加新接口破坏所有后端 | ✅ 可选接口灵活扩展 |

---

## 设计模式总结

fafafa.ssl 使用的设计模式：

1. **工厂模式** - TSSLFactory 创建后端实例
2. **抽象工厂** - 每个后端是一个抽象工厂
3. **接口隔离** - 核心接口 + 可选接口
4. **依赖注入** - 通过接口参数传递依赖
5. **策略模式** - 运行时选择后端
6. **适配器模式** - 统一不同 C 库 API

---

## 未来架构演进

### 短期（v1.2-v1.3）

- [ ] 添加更多可选接口（PKCS#11, DANE）
- [ ] 增强能力矩阵系统
- [ ] 改进错误处理机制

### 中期（v2.0）

- [ ] 纯 FreePascal TLS 后端（Phase 1: 密码学原语）
- [ ] 异步 I/O 支持
- [ ] 内存池优化

### 长期（v3.0）

- [ ] 完整的纯 Pascal TLS 1.2/1.3 实现
- [ ] 零依赖、单二进制部署
- [ ] FIPS 140-2 认证支持

---

## 参考资料

- **接口设计**: [API_DESIGN_GUIDE.md](reference/API_DESIGN_GUIDE.md)
- **迁移指南**: [MIGRATION_GUIDE_V1.1.md](MIGRATION_GUIDE_V1.1.md)
- **当前路线图**: [ROADMAP.md](ROADMAP.md)
- **当前 completeness 主线**: [plans/2026-03-25-ssl-tls-backend-completeness-roadmap-and-freepascal-tls13-aes256-sha384-parity.md](plans/2026-03-25-ssl-tls-backend-completeness-roadmap-and-freepascal-tls13-aes256-sha384-parity.md)

---

**文档版本**: 1.0
**最后更新**: 2026-02-05
**作者**: fafafa.ssl 架构团队
