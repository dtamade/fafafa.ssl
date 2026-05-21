# fafafa.ssl 项目架构设计

> 当前路线图: [当前路线图](../ROADMAP.md)
> 说明: 本页保留架构与抽象层设计说明；当前执行顺序和阶段判断以 `docs/ROADMAP.md` 为准。
> 当前入口说明:
> - 普通新代码优先使用 `uses fafafa.ssl, fafafa.ssl.context.builder;`，然后通过 `TSSLContextBuilder` / `TSSLConnector` 建立 TLS
> - 需要 fixed-backend / core factory surface 时，再使用
>   `TSSLFactory.GetLibraryInstance(...)`
>   或
>   `TSSLFactory.CreateContext(...)`

## 1. 项目概览

### 1.1 目标

创建一个统一的 SSL/TLS 抽象层，支持 OpenSSL、WolfSSL、MbedTLS、WinSSL(Schannel)、FreePascal 五种后端实现，为 Pascal 生态提供简单易用的 SSL/TLS 解决方案。

### 1.2 核心原则

- **统一接口**：屏蔽不同库的 API 差异
- **动态选择**：运行时选择后端实现
- **错误透明**：统一的错误处理机制
- **性能优先**：最小化抽象层开销
- **内存安全**：严格的资源管理

## 2. 架构层次设计

```
┌─────────────────────────────────────────────────────┐
│                     用户应用层                       │
├─────────────────────────────────────────────────────┤
│                 fafafa.ssl 统一接口层               │
├─────────────────────────────────────────────────────┤
│              后端适配器层 (Adapter Layer)           │
├────────────┬──────────┬──────────┬──────────┬───────┤
│ OpenSSL    │ WolfSSL  │ MbedTLS  │ WinSSL   │ Free  │
│ Wrapper    │ Wrapper  │ Wrapper  │ Wrapper  │ Pascal│
└────────────┴──────────┴──────────┴──────────┴───────┘
```

## 3. 核心模块设计

### 3.1 基础类型模块 (`fafafa.ssl.base`)

**职责**：定义所有通用数据类型、枚举、常量、异常类

**关键类型**：

- `TSSLLibraryType`: 后端库类型枚举
- `TSSLProtocolVersion`: 协议版本枚举
- `TSSLVerifyMode`: 证书验证模式
- `TSSLContextType`: 上下文类型（客户端/服务端）
- `ESSLException`: 统一异常类
- `TSSLCertificateInfo`: 证书信息结构
- `TSSLConnectionInfo`: 连接信息结构

### 3.2 核心接口模块 (`fafafa.ssl.base`)

**职责**：定义所有抽象接口

**主要接口**：

```pascal
ISSLContext = interface
  // 上下文配置和管理
end;

ISSLConnection = interface
  // SSL连接的建立、数据传输、状态查询
end;

ISSLCertificate = interface
  // 证书加载、验证、信息获取
end;

ISSLLibrary = interface
  // 库的初始化、清理、版本信息
end;
```

### 3.3 工厂管理模块 (`fafafa.ssl.factory`)

**职责**：

- 后端库的动态加载和选择
- 实例创建和生命周期管理
- 库可用性检测

**核心类**：

```pascal
TSSLFactory = class
  class function GetLibraryInstance(
    ALibType: TSSLLibraryType = sslAutoDetect
  ): ISSLLibrary;
  class function CreateContext(AContextType: TSSLContextType;
    ALibType: TSSLLibraryType = sslAutoDetect): ISSLContext;
  class function GetAvailableLibraries: TSSLLibraryTypes;
  class function IsLibraryAvailable(ALibType: TSSLLibraryType): Boolean;
end;
```

> 注：
> - 上面保留 `CreateContext(...)` 是为了说明 current core/factory surface；
>   但普通新代码仍优先走 `TSSLContextBuilder` / `TSSLConnector`
> - reference 语境下，如果你要显式固定 backend，
>   当前更推荐先取库：
>   `Lib := TSSLFactory.GetLibraryInstance(sslOpenSSL);`
>   再走
>   `Lib.CreateContext(sslCtxClient);`

### 3.4 后端实现模块

当前 backend 的 shipped/runtime truth 不以本表的完成度措辞为准；请同时查看 `docs/ROADMAP.md`、`docs/BACKEND_CAPABILITY_MATRIX.md`、以及 WinSSL 相关状态报告。

| 模块                      | 实现说明                              | 启用方式                   | 状态                                                                   |
| ------------------------- | ------------------------------------- | -------------------------- | ---------------------------------------------------------------------- |
| `fafafa.ssl.openssl.*`    | OpenSSL 实现（Linux/macOS 默认）      | 默认启用                   | ✅ 当前默认 active backend                                             |
| `fafafa.ssl.winssl.*`     | Windows Schannel 实现（Windows 默认） | 默认启用（仅 Windows）     | ⚠️ Windows 零依赖客户端 baseline 已验证；更细 runtime truth 见状态报告 |
| `fafafa.ssl.freepascal.*` | 纯 FreePascal TLS 1.3 实现            | 默认可用                   | 🔄 当前主线                                                            |
| `fafafa.ssl.mbedtls.*`    | mbedTLS 实现（轻量 TLS）              | `{$DEFINE ENABLE_MBEDTLS}` | 🔄 可选                                                                |
| `fafafa.ssl.wolfssl.*`    | wolfSSL 实现（嵌入式/兼容性）         | `{$DEFINE ENABLE_WOLFSSL}` | 🔄 可选                                                                |

- **OpenSSL 后端**
  - 低层绑定：`fafafa.ssl.openssl.api.*.pas`（function pointer bindings）
  - Loader：`fafafa.ssl.openssl.loader.pas`（负责 `libcrypto`/`libssl` 动态加载与符号解析）
  - 高层实现：`fafafa.ssl.openssl.{context,connection,certificate,session,store}.*`

- **WinSSL（Schannel）后端**
  - 实现单元：`fafafa.ssl.winssl.*`
  - 非 Windows 环境应跳过 WinSSL 专用构建/测试

- **MbedTLS / WolfSSL 后端**
  - 默认不启用：如需编译并注册这些后端，请在工程/编译参数中定义 `ENABLE_MBEDTLS` / `ENABLE_WOLFSSL`

每个后端模块包含：

```pascal
TXXXSSLContext = class(TInterfacedObject, ISSLContext)
TXXXSSLConnection = class(TBaseSSLConnection)  // 继承自抽象基类
TXXXSSLCertificate = class(TInterfacedObject, ISSLCertificate)
TXXXSSLLibrary = class(TInterfacedObject, ISSLLibrary)
```

### 3.6 连接抽象基类 (`fafafa.ssl.connection.base`)

**职责**：为所有 SSL 连接实现提供共享的基础功能

**架构设计**：

```pascal
TBaseSSLConnection = class(TInterfacedObject, ISSLConnection)
protected
  { 21 个抽象方法 - 后端必须实现 }
  function DoRead(var ABuffer; ACount: Integer): Integer; virtual; abstract;
  function DoWrite(const ABuffer; ACount: Integer): Integer; virtual; abstract;
  function DoConnect: Boolean; virtual; abstract;
  function DoAccept: Boolean; virtual; abstract;
  function DoHandshakeInternal: TSSLHandshakeState; virtual; abstract;
  function DoShutdown: Boolean; virtual; abstract;
  procedure DoClose; virtual; abstract;
  function DoRenegotiate: Boolean; virtual; abstract;
  function DoGetError(ARet: Integer): TSSLErrorCode; virtual; abstract;
  function DoWantRead: Boolean; virtual; abstract;
  function DoWantWrite: Boolean; virtual; abstract;
  function DoGetProtocolVersion: TSSLProtocolVersion; virtual; abstract;
  function DoGetCipherName: string; virtual; abstract;
  function DoGetPeerCertificate: ISSLCertificate; virtual; abstract;
  function DoGetPeerCertificateChain: TSSLCertificateArray; virtual; abstract;
  function DoGetVerifyResult: Integer; virtual; abstract;
  function DoGetVerifyResultString: string; virtual; abstract;
  function DoGetSession: ISSLSession; virtual; abstract;
  procedure DoSetSession(ASession: ISSLSession); virtual; abstract;
  function DoIsSessionReused: Boolean; virtual; abstract;
  function DoGetSelectedALPNProtocol: string; virtual; abstract;
  function DoGetState: string; virtual; abstract;
  function DoGetNativeHandle: Pointer; virtual; abstract;
public
  { ~50 个 ISSLConnection 方法的统一实现 }
  function Connect: Boolean;          // 调用 DoConnect + 更新状态
  function Read(var ABuffer; ACount: Integer): Integer;  // 调用 DoRead + 统计
  function ReadString(out AStr: string): Boolean;        // 基于 Read 实现
  function GetHealthStatus: TSSLHealthStatus;            // 统一实现
  function GetPerformanceMetrics: TSSLPerformanceMetrics; // 统一实现
  // ... 其他方法
end;
```

**设计优势**：

- **代码复用**：通用逻辑只需实现一次，后端只实现 21 个抽象方法
- **一致性**：所有后端共享相同的状态管理、性能跟踪、错误处理逻辑
- **可维护性**：修改通用行为只需修改基类
- **代码减少**：总计减少约 800 行重复代码

**后端继承关系**：

```
TBaseSSLConnection (676 lines)
├── TOpenSSLConnection (1388 lines) - 保留 ValidatePostHandshake 等复杂逻辑
├── TWinSSLConnection (2169 lines) - 保留 Schannel 握手、会话管理
├── TWolfSSLConnection (641 lines) - 独立模块
└── TMbedTLSConnection (566 lines) - 最简洁的实现
```

### 3.5 证书时间与扩展解析策略

为保证不同后端在证书语义上的一致性，时间字段和常用扩展（尤其是 `subjectAltName`）做了统一约定：

- **有效期时间（NotBefore/NotAfter）**
  - OpenSSL 后端通过 `X509_get_notBefore/After` 取得 `PASN1_TIME`，再委托统一的 `ASN1TimeToDateTime` 工具函数完成解析，避免直接手写 `TM` 结构解析曾经引发的 AV 问题。
  - WinSSL 后端使用 `FileTimeToSystemTime` → `SystemTimeToDateTime` 解析 `CERT_INFO.NotBefore/NotAfter`，两端最终都返回正常的 `TDateTime`，并在 `TSSLCertificateInfo.NotBefore/NotAfter` 中对齐语义。

- **subjectAltName（SAN）扩展**
  - shared parser（`fafafa.ssl.x509`）会把 `subjectAltName` 统一投影成纯值数组，并覆盖：
  - `dNSName` / `rfc822Name` / `uniformResourceIdentifier`；
  - `iPAddress` 的 IPv4 与 IPv6 文本（例如 `127.0.0.1`、`2001:DB8:0:0:0:0:0:10`）。
  - OpenSSL 简化后端（`fafafa.ssl.openssl.certificate`）在 native `GENERAL_NAMES` helper 可用时，会直接枚举 `GENERAL_NAME` 条目：
  - `GEN_DNS`/`GEN_EMAIL`/`GEN_URI` → 使用 ASN1 字符串工具转换为纯文本域名、邮箱、URI；
  - `GEN_IPADD` → 按字节解析为 IPv4/IPv6 文本；
  - 若 native helper 不可用，则回退到 shared parser truth，而不是旧的 pretty-print 文本解析。
  - WinSSL 后端（`fafafa.ssl.winssl.certificate`）优先复用 shared parser truth；native `CERT_ALT_NAME_INFO` 解码只作为 fallback，输出仍保持同一组纯域名/IP/邮箱/URI 值。
- 抽象层约定：
  - `ISSLCertificate.GetSubjectAltNames` 始终返回 **不带前缀** 的主机名/IP/邮箱/URI 字符串（例如 `san-test.local`、`example.test`、`127.0.0.1`、`admin@example.test`、`spiffe://mesh/node`），不暴露 `DNS:` / `IP Address:` 等后端格式细节；
  - `TSSLCertificateInfo.SubjectAltNames` 为上述结果的只读快照，两后端语义保持一致，方便上层做主机名匹配或调试输出。

此外，主机名验证统一遵循以下策略：

- OpenSSL 后端直接调用 `X509_check_host`，自动处理 SAN/CN、通配符及 IP/DNS 区分；
- WinSSL 后端在 `VerifyHostname` 中复用 `TSSLUtils.IsIPAddress/IsValidHostname`，遍历 SAN 时将 IP 与域名通配逻辑分离，忽略 Email/URI 条目并在 SAN 未命中时回退到 CN，与 OpenSSL 语义对齐。

## 4. 错误处理架构

### 4.1 四层错误处理机制

1. **原生错误捕获**：捕获底层库的错误码和消息
2. **错误码映射**：将原生错误映射到统一的 `TSSLErrorCode`
3. **上下文信息**：添加操作上下文和堆栈信息
4. **用户友好消息**：提供可读的错误描述

### 4.2 错误信息结构

```pascal
ESSLException = class(Exception)
  ErrorCode: TSSLErrorCode;        // 统一错误码
  LibraryType: TSSLLibraryType;    // 源库类型
  NativeError: Integer;            // 原生错误码
  NativeMessage: string;           // 原生错误消息
  Context: string;                 // 操作上下文
end;
```

### 4.3 错误处理策略

- **即时转换**：底层错误立即转换为统一格式
- **上下文保留**：保持原始错误信息便于调试
- **分级处理**：区分致命错误和可恢复错误
- **日志集成**：自动记录错误详情

## 5. 内存管理策略

### 5.1 资源管理原则

- **RAII模式**：对象构造时获取资源，析构时释放
- **引用计数**：使用接口的自动引用计数
- **异常安全**：确保异常情况下资源正确释放

### 5.2 缓冲区管理

- **统一缓冲区大小**：默认 16KB，可配置
- **零拷贝优化**：尽可能避免数据拷贝
- **内存池**：考虑为频繁分配的小对象使用内存池

## 6. 性能优化设计

### 6.1 延迟加载

- 动态库延迟加载，只在需要时加载
- 上下文延迟初始化，减少启动开销

### 6.2 缓存策略

- 证书验证结果缓存
- DNS解析结果缓存（如果涉及）
- 会话复用支持

### 6.3 批量操作

- 支持批量证书验证
- 批量数据传输接口

## 7. 线程安全设计

### 7.1 线程安全等级

- **库级别**：确保库的初始化/清理线程安全
- **上下文级别**：上下文可在多线程间共享（只读配置）
- **连接级别**：单个连接不支持并发操作，需要外部同步

### 7.2 同步机制

- 使用 Pascal 的 `TCriticalSection` 保护共享资源
- 原子操作用于简单的计数器和状态标志

## 8. 配置管理

### 8.1 配置层次

`TSSLConfig` 并不是“所有字段都在同一层直接生效”的纯层级配置。当前 public truth 已经明确分成几类作用域：

- **library-scoped defaults**
  - `LogLevel`
  - `LogCallback`
  - 通过 `TSSLLibraryDefaults` + `GetLibraryDefaults(...)` / `ApplyLibraryDefaults(...)` 访问 library-owned defaults；底层仍分别落到 `SetDefaultConfig(...)` / `SetLogCallback(...)`；factory request path 不接受 request-local 覆盖。
- **context-scoped**
  - `ProtocolVersions` / `PreferredVersion`
  - `CertificateFile` / `PrivateKeyFile` / `CAFile` / `CAPath`
  - `UseSystemRoots`
  - `VerifyMode` / `VerifyDepth`
  - `CipherList` / `CipherSuites` / `Options`
  - `SessionCacheSize` / `SessionTimeout`
  - `ALPNProtocols`
  - `ClientEarlyDataEnabled`
  - `ServerEarlyDataPolicy` / `ServerMaxEarlyDataSize`
  - `ServerEarlyDataReplayStoreFile` / `ServerEarlyDataReplayStoreDirectory`
- **connection-scoped**
  - `HandshakeTimeout`
  - `BufferSize`
  - 这两个字段不属于 context/factory/direct-library config 主路径，应改走 `TSSLConnector.WithTimeout` / `ISSLConnection.SetTimeout` 或外围 IO/transport 配置。
- **compatibility-only**
  - `ServerName`
    - 当前是 deprecated context-level compatibility field；client 侧主路径应改走 per-connection SNI。
  - `EnableCompression`
  - `EnableSessionTickets`
  - `EnableOCSPStapling`
    - 这三个字段仍作为 option-bridge compatibility surface 保留，但新代码应优先直接写 `Options`。

更细的字段真相以 `src/fafafa.ssl.base.pas` 与 `docs/reference/API_REFERENCE.md` 为准。

### 8.2 配置来源优先级

1. 代码中显式设置的参数
2. 环境变量
3. 配置文件
4. 默认值

## 9. 测试策略

### 9.1 测试覆盖

- **单元测试**：每个接口方法的功能测试
- **集成测试**：不同后端的兼容性测试
- **压力测试**：并发连接和大数据量测试
- **安全测试**：恶意输入和边界条件测试

### 9.2 模拟测试

- Mock SSL服务器用于客户端测试
- 证书生成工具用于测试不同证书场景
- 网络条件模拟（延迟、丢包、中断）

## 10. 部署和分发

### 10.1 库文件组织

```
fafafa.ssl/
├── bin/           # 编译后的库文件
├── include/       # Pascal 单元文件
├── examples/      # 使用示例
└── docs/          # 文档
```

### 10.2 依赖管理

- 静态链接优先，减少运行时依赖
- 提供动态库版本支持更新
- 清晰的版本兼容性矩阵

## 11. 开发里程碑

### 阶段1：基础架构 (Week 1-2)

- [ ] 创建类型定义 (`fafafa.ssl.base`)
- [ ] 设计核心接口 (`fafafa.ssl.base`)
- [ ] 实现工厂模式 (`fafafa.ssl.factory`)
- [ ] 建立测试框架

### 阶段2：OpenSSL 后端 (Week 3-4)

- [ ] OpenSSL 绑定和封装
- [ ] 基本 SSL 上下文功能
- [ ] 客户端连接实现
- [ ] 单元测试完成

### 阶段3：WolfSSL 后端 (Week 5-6)

- [ ] WolfSSL 绑定和封装
- [ ] 接口实现和测试
- [ ] 兼容性验证

### 阶段4：MbedTLS 后端 (Week 7-8)

- [ ] MbedTLS 绑定和封装
- [ ] 接口实现和测试
- [ ] 性能对比分析

### 阶段5：WinSSL 后端 (Week 9-10)

- [ ] Windows Schannel API 封装
- [ ] 接口实现和测试
- [ ] Windows 平台优化

### 阶段6：完善和优化 (Week 11-12)

- [ ] 错误处理完善
- [ ] 性能优化和对比分析
- [ ] 文档完善
- [ ] 发布准备

## 12. 风险评估

### 12.1 技术风险

- **库版本兼容性**：不同版本 SSL 库的 API 差异
- **平台差异**：Windows/Linux/macOS 的实现差异
- **性能开销**：抽象层带来的性能损失

### 12.2 缓解策略

- 建立完整的测试矩阵覆盖主要版本组合
- 使用条件编译处理平台差异
- 基准测试持续监控性能影响

---

**文档版本**: 1.0  
**创建时间**: 2025-09-28  
**作者**: fafafa.ssl 开发团队
