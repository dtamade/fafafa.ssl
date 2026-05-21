# fafafa.ssl API 参考文档

> **版本**: rolling
> **最后更新**: 2026-05-18
> **当前路线图**: [当前路线图](../ROADMAP.md)
> **说明**: 当前接口真相源以 `src/fafafa.ssl.base.pas` 为准；本页优先收敛最常用的 public API。

## 目录

- [核心接口](#核心接口)
- [FreePascal early-data replay-store opt-in](#freepascal-early-data-replay-store-opt-in)
- [OpenSSL 后端](#openssl-后端)
- [WinSSL 后端](#winssl-后端)
- [数据类型](#数据类型)
- [错误处理](#错误处理)
- [工具函数](#工具函数)

---

## Client SNI Compatibility Note

- `TSSLConfig.ServerName` 仍然保留为向后兼容入口，但它表示的是 deprecated context-level SNI compatibility，不是推荐主路径。
- `TSSLContextBuilder.WithSNI(...)` 也仍然保留为 compatibility-only 入口；它现在已经是编译期 `deprecated`，且 `BuildClient` / `BuildServer` 都会发出 warning 并忽略它。
- `ISSLContext.SetServerName(...)` / `GetServerName(...)` 仍保留为 deprecated direct context compatibility API；普通 client 流不应再通过它们传递 SNI/hostname。
- `TSSLFactory.CreateContext(...)` 现在也不再把这个字段写进新建 context；若传入 `TSSLConfig.ServerName`，factory 会发出 warning 并忽略它。
- `ISSLLibrary.SetDefaultConfig(...)` + `ISSLLibrary.CreateContext(AType)` 这条 direct-library path 现在也已对齐：client default-config 会 warning + ignore，server default-config 会 reject。
- 新代码请优先使用 `TSSLConnectionBuilder.WithHostname(...)`、`ISSLClientConnection.SetServerName(...)`，或直接走 `TSSLConnector.Connect*(..., ServerName)`。

## Type-Safety Surface Note

`fafafa.ssl` 主门面当前也 re-export 这组 non-generic type-safety public surface（如 `TSSLVersion` / `TKeySize` / `TTimeoutDuration` / `TBufferSize`）；`TSecureData<T>` / `TResult<T, E>` 继续保留在 `fafafa.ssl.safety`。
如果你只想单独使用这套类型安全工具而不引入 TLS bootstrap facade，也可以窄用 `fafafa.ssl.safety`。

---

## TSSLConfig Scope Buckets

`TSSLConfig` 不是“所有字段都在同一层直接生效”的纯 context record。当前最容易反复失真的，是 performance / advanced / logging 这一段 mixed-scope 字段；它们的 public truth 已经收敛为下面几个 buckets。

- ordinary context/config fields
  - `LibraryType` / `ContextType` / `ProtocolVersions` / `PreferredVersion`
  - `CertificateFile` / `PrivateKeyFile` / `PrivateKeyPassword`
  - `CAFile` / `CAPath` / `VerifyMode` / `VerifyDepth`
  - `CipherList` / `CipherSuites` / `Options`
- library-scoped defaults
  - `LogLevel`
  - `LogCallback`
  - 通过 `ISSLLibrary.GetDefaultConfig(...)` / `SetDefaultConfig(...)` 调整 `LogLevel`，通过 `ISSLLibrary.SetLogCallback(...)` 安装回调；`SetDefaultConfig(...)` 不再安装或替换回调；fresh request config 仍会回到 `sslLogError` + `nil` baseline。
  - `TSSLFactory.CreateContext(const AConfig)` 会拒绝 request-local 覆盖。
- context-scoped
  - `SessionCacheSize`
  - `SessionTimeout`
  - `ALPNProtocols`
  - `ClientEarlyDataEnabled`
  - `ServerEarlyDataPolicy`
  - `ServerMaxEarlyDataSize`
  - `ServerEarlyDataReplayStoreFile`
  - `ServerEarlyDataReplayStoreDirectory`
  - 这些字段会被 `TSSLFactory.CreateContext(...)` 等 context 创建路径消费；其中 replay-store 两个字段还带有 server-only 约束。
- connection-scoped
  - `HandshakeTimeout`
  - `BufferSize`
  - factory request path 和 direct-library context path 都不接受它们的自定义值；请改走 `TSSLConnector.WithTimeout` / `TSSLAcceptor.WithTimeout` / `ISSLConnection.SetTimeout` 或外围 transport / IO 配置。
- compatibility-only
  - `ServerName`
  - 当前 client context 创建路径是 warning + ignore，server context 创建路径会 reject；新代码应改走 per-connection SNI。
- compatibility-only option-bridge flags
  - `EnableCompression`
  - `EnableSessionTickets`
  - `EnableOCSPStapling`
  - 这几个布尔字段当前仍保留在 `v1.x` public record 中，但它们只是历史 compatibility 写入口；新代码应优先直接写 `Options`。
  - factory 与 direct-library default-config path 会先把它们归一化进 `Options`，不应继续把它们扩成新的 backend-private 配置槽。
  - `ISSLLibrary.GetDefaultConfig(...)` / `CreateDefaultConfig(...)` 这类 fresh default-config surface 返回时，也必须保持这些 compatibility booleans 与最终 `Options` 真相一致。
  - 当调用方同时传入冲突的 `Options` 与 option-bridge booleans 时，当前冻结规则是：
    legacy boolean 赢，先回写对应 option bit，再把最终 `Options` 真相投影回这三个 boolean 字段。

---

## TSSLConfig Migration Targets

`TSSLConfig` 在 `v1.x` 仍然保留为 public record，但 mixed-scope / compatibility 字段已经不再适合作为未来主路径。当前推荐把迁移方向理解成下面这张 field-to-surface map。

- 继续保留在 context-safe `TSSLConfig` 主路径
  - `ProtocolVersions` / `PreferredVersion`
  - `CertificateFile` / `PrivateKeyFile` / `PrivateKeyPassword`
  - `CAFile` / `CAPath` / `VerifyMode` / `VerifyDepth`
  - `CipherList` / `CipherSuites` / `Options`
  - `SessionCacheSize` / `SessionTimeout`
  - `ALPNProtocols`
  - `ClientEarlyDataEnabled`
  - `ServerEarlyDataPolicy` / `ServerMaxEarlyDataSize`
  - `ServerEarlyDataReplayStoreFile` / `ServerEarlyDataReplayStoreDirectory`
- 迁移到 library defaults surface
  - `LogLevel`
    - 当前推荐入口：`ISSLLibrary.GetDefaultConfig(...)` / `SetDefaultConfig(...)`
  - `LogCallback`
    - 当前推荐入口：`ISSLLibrary.SetLogCallback(...)`（`SetDefaultConfig(...)` 不再安装或替换回调）
  - `v2` 方向：不再把 library defaults 混在 context/request config record 中。
- 迁移到 connection / transport surface
  - `HandshakeTimeout`
    - 当前推荐入口：`TSSLConnector.WithTimeout(...)` / `TSSLAcceptor.WithTimeout(...)`
    - 连接创建后若需要局部覆盖，`ISSLConnection.SetTimeout(...)` 仍作为 `v1.x` connection-adjacent convenience surface 保留
  - `BufferSize`
    - 当前推荐入口：外围 socket / stream / transport / app-level buffer policy
  - `v2` 方向：从 context factory record 中移出这类 connection-adjacent 字段。
- 迁移到 per-connection SNI surface
  - `ServerName`
    - 当前推荐入口：`TSSLConnectionBuilder.WithHostname(...)` / `ISSLClientConnection.SetServerName(...)` / `TSSLConnector.Connect*(..., ServerName)`
  - `v1.x` 状态：冻结为 deprecated compatibility-only context field
  - `v2` 方向：不再作为 context-level config field 继续主挂载。
- 迁移到 option-set surface
  - `EnableCompression`
  - `EnableSessionTickets`
  - `EnableOCSPStapling`
  - 当前推荐入口：直接写 `Options`，或 builder 的 `WithOption(...)` / option snapshot path
  - `v1.x` 状态：冻结为 compatibility-only option-bridge booleans
  - `v2` 方向：不再把这组三个 legacy booleans 当成正常首选写入口。

这个 migration map 的目的不是立即删字段，而是把后续 slimming / redesign 的执行顺序稳定下来：

1. 先停止把 mixed-scope / compatibility 字段继续当成主路径教给调用方。
2. 再决定 `v2` 是拆成独立 config type，还是改成更窄的 dedicated surface。
3. 最后才做真正的 public API removal / remount。

---

## Direct-Library Default Config Note

`ISSLLibrary.SetDefaultConfig(...)` + `ISSLLibrary.CreateContext(AType)` 这条 direct-library path 现在也按统一规则应用一组 context-safe 默认配置，不再只在 OpenSSL 上看起来“比较完整”。

- 当前对齐的字段：
  - `ProtocolVersions`
  - `PreferredVersion`
  - `VerifyMode`
  - `VerifyDepth`
  - `CipherList`
  - `CipherSuites`
  - `Options`
  - `SessionCacheSize`
  - `SessionTimeout`
  - `ALPNProtocols`
  - `ClientEarlyDataEnabled`
  - `ServerEarlyDataPolicy`
  - `ServerMaxEarlyDataSize`
  - `ServerEarlyDataReplayStoreFile`
  - `ServerEarlyDataReplayStoreDirectory`
- `SetDefaultConfig(...)` 会先归一化 `TSSLConfig`，所以 `EnableCompression` / `EnableSessionTickets` / `EnableOCSPStapling` 这类 compatibility-only option-bridge 字段也会先折叠进 `Options`，再由 direct-library `CreateContext(AType)` 应用到新 context。
- 同一条 direct-library path 现在也已对齐 deprecated `ServerName` compatibility 语义：
  - client default-config = warning + ignore
  - server default-config = reject
- `HandshakeTimeout` / `BufferSize` 仍保持 connection-scoped；如果 default-config 给它们写了自定义值，`CreateContext(AType)` 会 fail-fast reject，并把调用方导向 connector/connection timeout 或外围 transport/IO 配置。
- replay-store 仍保持 server-only 约束；若 backend 不实现 installer seam，则 direct-library server path 会 fail-fast。

---

## FreePascal early-data replay-store opt-in

FreePascal server-side `0-RTT / early data` 默认 shipped path 已经会把 replay truth 落到本地持久化 replay-store 路径。当前 capability 仍保持 `experimental`；如果默认路径不可用或不可写，恢复的 early data 会 fail-closed reject。public API 仍然开放 file / directory 两条 opt-in 路径，用来显式指定 replay-store 的落点。

### Use `TSSLConfig` with `TSSLFactory.CreateContext(...)`

`TSSLConfig` 目前提供两个 server-only replay-store 字段：

- `TSSLConfig.ServerEarlyDataReplayStoreFile`
- `TSSLConfig.ServerEarlyDataReplayStoreDirectory`

`TSSLConfig.ServerName` 不属于推荐的 client SNI 配置路径。当前 `TSSLFactory.CreateContext(...)` 对它的 client-side 行为是 warning + ignore；客户端请改用 `ISSLClientConnection.SetServerName(...)` 或 `TSSLConnector.Connect*(..., ServerName)`。

这两个字段属于 context-scoped、server-only opt-in，用于 `TSSLFactory.CreateContext(const AConfig)` 的 FreePascal server path。它们是 mutually exclusive 配置，不能同时设置；同时设置时，factory 会 fail fast。

```pascal
var
  LConfig: TSSLConfig;
begin
  LConfig := CreateDefaultConfig(sslCtxServer);
  LConfig.LibraryType := sslFreePascal;
  LConfig.ContextType := sslCtxServer;
  LConfig.PreferredVersion := sslProtocolTLS13;
  LConfig.ProtocolVersions := [sslProtocolTLS13];
  LConfig.VerifyMode := [];  // config/direct-context 当前 public no-verify 语义
  LConfig.CertificateFile := 'tests/certificate/test_certs/signer_cert.pem';
  LConfig.PrivateKeyFile := 'tests/certificate/test_certs/signer_key.pem';
  LConfig.SessionCacheSize := 8;
  LConfig.SessionTimeout := 7200;
  Include(LConfig.Options, ssoEnableSessionCache);
  LConfig.ServerEarlyDataPolicy := sslEarlyDataServerAccept;
  LConfig.ServerMaxEarlyDataSize := 8;
  LConfig.ServerEarlyDataReplayStoreDirectory := '/var/lib/fafafa/replay-store';

  Ctx := TSSLFactory.CreateContext(LConfig);
end;
```

当前口径：

- builder 上如果要禁用验证，请显式使用 `.WithVerifyNone`；
- config/direct-context 当前 public no-verify 语义是 `[]`；
- 这两条都会落成 no-verify runtime truth，但生产环境仍应优先启用验证。

### Use builder opt-ins for server contexts

`TSSLContextBuilder` 目前提供两个对应入口：

- `WithServerEarlyDataReplayStoreFile(...)`
- `WithServerEarlyDataReplayStoreDirectory(...)`

这两个 builder opt-in 也只适用于 FreePascal server context，同样 mutually exclusive；builder 在双配置时会 fail fast。

```pascal
Ctx := TSSLContextBuilder.Create
  .WithBackend(sslFreePascal)
  .WithTLS13
  .WithVerifyNone
  .WithCertificate('tests/certificate/test_certs/signer_cert.pem')
  .WithPrivateKey('tests/certificate/test_certs/signer_key.pem')
  .WithSessionCache(True)
  .WithSessionTimeout(TTimeoutDuration.Minutes(120))
  .WithServerEarlyDataPolicy(sslEarlyDataServerAccept)
  .WithServerMaxEarlyDataSize(8)
  .WithServerEarlyDataReplayStoreFile('/var/lib/fafafa/replay-store.bin')
  .BuildServer;
```

这条 public opt-in 只暴露单机可控的 replay-store seam；它解决的是 caller-controlled path placement，不表示 distributed readiness 已完成。

---

## 核心接口

### ISSLLibrary

SSL/TLS 库的主接口，提供库管理和实例创建功能。

当前 `v1.5.0` 活跃文档以 `src/fafafa.ssl.base.pas` 为准。下面代码块列的是当前 shipped source truth，而不是早期文档里保留下来的精简子集。

```pascal
ISSLLibrary = interface
  // 初始化与配置
  function Initialize: Boolean;
  procedure Finalize;
  function IsInitialized: Boolean;

  // 库信息
  function GetLibraryType: TSSLLibraryType;
  function GetVersionString: string;
  function GetVersionNumber: Cardinal;
  function GetCompileFlags: string;

  // 功能检测
  function IsProtocolSupported(aProtocol: TSSLProtocolVersion): Boolean;
  function IsCipherSupported(const aCipherName: string): Boolean;
  function IsFeatureSupported(aFeature: TSSLFeature): Boolean;
  function GetCapabilities: TSSLBackendCapabilities;

  // 默认配置
  procedure SetDefaultConfig(const aConfig: TSSLConfig);
  function GetDefaultConfig: TSSLConfig;

  // 错误处理
  function GetLastError: Integer;
  function GetLastErrorString: string;
  procedure ClearError;

  // 统计信息
  function GetStatistics: TSSLStatistics;
  procedure ResetStatistics;

  // 日志
  procedure SetLogCallback(aCallback: TSSLLogCallback);
  procedure Log(aLevel: TSSLLogLevel; const aMessage: string);

  // 工厂方法
  function CreateContext(aType: TSSLContextType): ISSLContext;
  function CreateCertificate: ISSLCertificate;
  function CreateCertificateStore: ISSLCertificateStore;
end;
```

**使用示例**:

```pascal
var
  LLib: ISSLLibrary;
begin
  LLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
  if LLib.Initialize then
  begin
    WriteLn('版本: ', LLib.GetVersionString);
    // 使用库...
    LLib.Finalize;
  end;
end;
```

---

### ISSLContext

SSL/TLS 上下文接口，管理连接配置。

当前 `v1.5.0` 活跃文档以 `src/fafafa.ssl.base.pas` 为准。下面代码块列的是当前 shipped source truth，而不是早期文档里保留下来的最小演示面。

```pascal
ISSLContext = interface
  // 上下文类型
  function GetContextType: TSSLContextType;

  // 协议版本
  procedure SetProtocolVersions(aVersions: TSSLProtocolVersions);
  function GetProtocolVersions: TSSLProtocolVersions;
  procedure SetPreferredVersion(aVersion: TSSLProtocolVersion);
  function GetPreferredVersion: TSSLProtocolVersion;

  // 证书与密钥
  procedure LoadCertificate(const aFileName: string); overload;
  procedure LoadCertificate(aStream: TStream); overload;
  procedure LoadCertificate(aCert: ISSLCertificate); overload;
  procedure LoadPrivateKey(const aFileName: string; const aPassword: string = ''); overload;
  procedure LoadPrivateKey(aStream: TStream; const aPassword: string = ''); overload;
  procedure LoadCertificatePEM(const aPEM: string);
  procedure LoadPrivateKeyPEM(const aPEM: string; const aPassword: string = '');

  // CA 证书
  procedure LoadCAFile(const aFileName: string);
  procedure LoadCAPath(const aPath: string);
  procedure SetCertificateStore(aStore: ISSLCertificateStore);

  // 验证配置
  procedure SetVerifyMode(aMode: TSSLVerifyModes);
  function GetVerifyMode: TSSLVerifyModes;
  procedure SetVerifyDepth(aDepth: Integer);
  function GetVerifyDepth: Integer;
  procedure SetVerifyCallback(aCallback: TSSLVerifyCallback);

  // 密码套件
  // 在调用 `SetCipherList(...)` / `SetCipherSuites(...)` 传入 custom non-default cipher override 前，先检查 `ISSLLibrary.GetCapabilities.SupportsCustomCipherSuites`；对 `SupportsCustomCipherSuites=False` 的 backend，custom non-default 赋值应抛出 `unsupported`，empty clear 与 shipped baseline defaults 仅作为 compatibility/default-context path。
  procedure SetCipherList(const aCipherList: string);
  function GetCipherList: string;
  procedure SetCipherSuites(const aCipherSuites: string);
  function GetCipherSuites: string;

  // 会话管理
  procedure SetSessionCacheMode(aEnabled: Boolean);
  function GetSessionCacheMode: Boolean;
  procedure SetSessionTimeout(aTimeout: Integer);
  function GetSessionTimeout: Integer;
  procedure SetSessionCacheSize(aSize: Integer);
  function GetSessionCacheSize: Integer;

  // 选项与兼容字段
  procedure SetOptions(const aOptions: TSSLOptions);
  function GetOptions: TSSLOptions;
  procedure SetServerName(const aServerName: string);
  function GetServerName: string;
  procedure SetALPNProtocols(const aProtocols: string);
  function GetALPNProtocols: string;
  procedure SetCertVerifyFlags(aFlags: TSSLCertVerifyFlags);
  function GetCertVerifyFlags: TSSLCertVerifyFlags;
  procedure SetPasswordCallback(aCallback: TSSLPasswordCallback);
  procedure SetInfoCallback(aCallback: TSSLInfoCallback);

  // 证书固定
  procedure AddCertificatePin(const aHash: TBytes; aPinType: Integer;
    const aDescription: string; aIsBackup: Boolean = False);
  procedure AddCertificatePinBase64(const aBase64Hash: string; aPinType: Integer;
    const aDescription: string; aIsBackup: Boolean = False);
  procedure SetCertificatePinningEnabled(aEnabled: Boolean);
  function GetCertificatePinningEnabled: Boolean;
  procedure ClearCertificatePins;

  // 连接创建
  function CreateConnection(aSocket: THandle): ISSLConnection; overload;
  function CreateConnection(aStream: TStream): ISSLConnection; overload;

  // 状态
  function IsValid: Boolean;
end;
```

原生句柄访问通过可选接口暴露：

```pascal
ISSLNativeHandleAccess = interface
  function GetNativeHandle: Pointer;
  function GetBackendType: TSSLLibraryType;
  function IsNativeHandleValid: Boolean;
end;
```

- `GetNativeHandle` 不属于 `ISSLContext` / `ISSLConnection` 核心接口；需要底层句柄时，请先通过 `ISSLNativeHandleAccess` 访问。
- 纯 `FreePascal` backend 不实现这个可选接口；`OpenSSL` / `WinSSL` / `MbedTLS` / `WolfSSL` 等 C-library backend 才会暴露它。

**使用示例**:

```pascal
var
  LContext: ISSLContext;
begin
  LContext := LLib.CreateContext(sslCtxClient);
  LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
  LContext.LoadCAFile('ca-bundle.crt');
  LContext.SetVerifyMode([sslVerifyPeer]);
  // 创建连接...
end;
```

#### 其他可选 public interfaces

`ISSLNativeHandleAccess` 之外，当前 shipped source 还公开了多组 optional public interfaces，分别承载：

- transport / HTTP hooks 注入
- server-side context 专属能力
- early-data context / connection owner surface
- `ISSLConnection` 上各组 compatibility-core mirror 的默认 owner surface

当前 public Pascal source 尚未声明 `ISSLServerConnection`；服务端特有能力主要通过可选 context 扩展接口暴露。

HTTP transport hooks 通过可选接口暴露：

```pascal
ISSLHttpHooksAccess = interface
  procedure SetHTTPGetCallback(ACallback: TSSLHTTPGetCallback);
  function GetHTTPGetCallback: TSSLHTTPGetCallback;
  procedure SetHTTPPostCallback(ACallback: TSSLHTTPPostCallback);
  function GetHTTPPostCallback: TSSLHTTPPostCallback;
end;
```

- `fafafa.ssl` 不实现网络通信；任何依赖 HTTP 的功能（例如 OCSP 在线检查、CT log list 下载）都需要调用方通过这个接口注入回调。

服务端 stapled OCSP response material 通过可选 context 接口暴露：

```pascal
ISSLServerOCSPStaplingContext = interface
  procedure ClearServerStapledOCSPResponse;
  procedure SetServerStapledOCSPResponse(const AResponseDER: TBytes);
  procedure LoadServerStapledOCSPResponseFile(const AFileName: string);
  function HasServerStapledOCSPResponse: Boolean;
  function GetServerStapledOCSPResponse: TBytes;
end;
```

- 这条 public surface 只负责 caller-provided DER bytes / file material，不负责 online fetch / refresh / responder 调度。

TLS 1.3 early-data public surface 分别挂在 context / connection 上：

```pascal
ISSLEarlyDataContext = interface
  procedure SetClientEarlyDataEnabled(AEnabled: Boolean);
  function GetClientEarlyDataEnabled: Boolean;
  procedure SetServerEarlyDataPolicy(APolicy: TSSLEarlyDataServerPolicy);
  function GetServerEarlyDataPolicy: TSSLEarlyDataServerPolicy;
  procedure SetServerMaxEarlyDataSize(ASize: Cardinal);
  function GetServerMaxEarlyDataSize: Cardinal;
end;

ISSLEarlyDataConnection = interface
  function SetEarlyData(const AData: TBytes): TSSLOperationResult;
  function GetEarlyDataStatus: TSSLEarlyDataStatus;
  function GetEarlyDataLimit: Cardinal;
end;
```

- `ISSLEarlyDataContext` 负责 context 级策略。
- `ISSLEarlyDataConnection` 负责客户端排队 early data 与连接级状态查询。

连接侧 owner surfaces 通过这几组可选接口暴露：

```pascal
ISSLConnectionInfo = interface
  function GetConnectionInfo: TSSLConnectionInfo;
  function GetContext: ISSLContext;
  function GetSelectedALPNProtocol: string;
  function GetStateString: string;
end;

ISSLDiagnostics = interface
  function GetHealthStatus: TSSLHealthStatus;
  function IsHealthy: Boolean;
  function GetPerformanceMetrics: TSSLPerformanceMetrics;
  function GetDiagnosticInfo: TSSLDiagnosticInfo;
end;

ISSLSessionResumption = interface
  function GetSession: ISSLSession;
  procedure SetSession(ASession: ISSLSession);
  function IsSessionReused: Boolean;
end;

ISSLCertificateVerification = interface
  function GetPeerCertificateChain: TSSLCertificateArray;
  function GetVerifyResult: Integer;
  function GetVerifyResultString: string;
end;

ISSLOCSPStapling = interface
  function GetOCSPStaplingEnabled: Boolean;
  function GetOCSPResponse: TBytes;
  function IsOCSPResponseVerified: Boolean;
  function GetOCSPResponseStatus: string;
end;
```

- 对 `GetConnectionInfo` / `GetContext` / `GetSelectedALPNProtocol` / `GetStateString` 这组连接信息 mirrors，新代码优先通过 `ISSLConnectionInfo` 获取。
- 对健康、性能、诊断信息，新代码优先通过 `ISSLDiagnostics` 获取。
- 对会话保存 / 注入 / 复用命中状态，新代码优先通过 `ISSLSessionResumption` 获取。
- 对证书链和验证结果，新代码优先通过 `ISSLCertificateVerification` 获取。
- 对 OCSP stapling runtime state，新代码优先通过 `ISSLOCSPStapling` 获取。

---

### ISSLCertificate

X.509 证书接口。

当前 `v1.5.0` 活跃文档以 `src/fafafa.ssl.base.pas` 为准。下面代码块列的是当前 shipped source truth，而不是早期文档里保留下来的窄化证书子集。

```pascal
ISSLCertificate = interface
  // 加载与保存
  function LoadFromFile(const aFileName: string): Boolean;
  function LoadFromStream(aStream: TStream): Boolean;
  function LoadFromMemory(const aData: Pointer; aSize: Integer): Boolean;
  function LoadFromPEM(const aPEM: string): Boolean;
  function LoadFromDER(const aDER: TBytes): Boolean;
  function SaveToFile(const aFileName: string): Boolean;
  function SaveToStream(aStream: TStream): Boolean;
  function SaveToPEM: string;
  function SaveToDER: TBytes;

  // 证书信息
  function GetInfo: TSSLCertificateInfo;
  function GetSubject: string;
  function GetIssuer: string;
  function GetSerialNumber: string;
  function GetNotBefore: TDateTime;
  function GetNotAfter: TDateTime;
  function GetPublicKey: string;
  function GetPublicKeyAlgorithm: string;
  function GetSignatureAlgorithm: string;
  function GetVersion: Integer;

  // 验证
  function Verify(aCAStore: ISSLCertificateStore): Boolean;
  function VerifyEx(aCAStore: ISSLCertificateStore;
    aFlags: TSSLCertVerifyFlags; out aResult: TSSLCertVerifyResult): Boolean;
  function VerifyHostname(const aHostname: string): Boolean;
  function IsExpired: Boolean;
  function IsSelfSigned: Boolean;
  function IsCA: Boolean;

  // 便利方法
  function GetDaysUntilExpiry: Integer;
  function GetSubjectCN: string;

  // 扩展
  function GetExtension(const aOID: string): string;
  function GetSubjectAltNames: TSSLStringArray;
  function GetKeyUsage: TSSLStringArray;
  function GetExtendedKeyUsage: TSSLStringArray;

  // 指纹
  function GetFingerprint(aHashType: TSSLHash): string;
  function GetFingerprintSHA1: string;
  function GetFingerprintSHA256: string;

  // 证书链 / 对象管理
  procedure SetIssuerCertificate(aCert: ISSLCertificate);
  function GetIssuerCertificate: ISSLCertificate;
  function Clone: ISSLCertificate;
end;
```

**使用示例**:

```pascal
var
  LCert: ISSLCertificate;
  LResult: TSSLCertVerifyResult;
begin
  LCert := LLib.CreateCertificate;
  if LCert.LoadFromFile('mycert.pem') then
  begin
    WriteLn('主题: ', LCert.GetSubject);
    WriteLn('有效期至: ', DateTimeToStr(LCert.GetNotAfter));

    // 增强验证
    if LCert.VerifyEx(LStore, [sslCertVerifyCheckRevocation], LResult) then
      WriteLn('验证成功')
    else
      WriteLn('验证失败: ', LResult.ErrorMessage);
  end;
end;
```

---

### ISSLCertificateStore

证书存储接口，用于加载、查找和验证根证书/中间证书集合。

当前 `v1.5.0` 活跃文档以 `src/fafafa.ssl.base.pas` 为准。下面代码块列的是当前 shipped source truth。

```pascal
ISSLCertificateStore = interface
  // 证书管理
  function AddCertificate(aCert: ISSLCertificate): Boolean;
  function RemoveCertificate(aCert: ISSLCertificate): Boolean;
  function Contains(aCert: ISSLCertificate): Boolean;
  procedure Clear;
  function GetCount: Integer;
  function GetCertificate(aIndex: Integer): ISSLCertificate;

  // 加载证书
  function LoadFromFile(const aFileName: string): Boolean;
  function LoadFromPath(const aPath: string): Boolean;
  function LoadSystemStore: Boolean;

  // 查找证书
  function FindBySubject(const aSubject: string): ISSLCertificate;
  function FindByIssuer(const aIssuer: string): ISSLCertificate;
  function FindBySerialNumber(const aSerialNumber: string): ISSLCertificate;
  function FindByFingerprint(const aFingerprint: string): ISSLCertificate;

  // 验证
  function VerifyCertificate(aCert: ISSLCertificate): Boolean;
  function BuildCertificateChain(aCert: ISSLCertificate): TSSLCertificateArray;
end;
```

最小用法可直接参考 [STORE_USAGE_GUIDE.md](../guides/STORE_USAGE_GUIDE.md)。

**使用示例**:

```pascal
var
  LStore: ISSLCertificateStore;
begin
  LStore := TSSLFactory.CreateCertificateStore;
  if not LStore.LoadSystemStore then
    WriteLn('加载系统证书库失败');
end;
```

---

### ISSLConnection

SSL/TLS 连接接口。

当前 `v1.5.0` 活跃文档以 `src/fafafa.ssl.base.pas` 为准。下面是当前 shipped source truth，而不是旧审查阶段留下的过时签名。

```pascal
ISSLConnection = interface
  function Connect: Boolean;
  function Accept: Boolean;
  function Shutdown: Boolean;
  procedure Close;

  function DoHandshake: TSSLHandshakeState;
  function IsHandshakeComplete: Boolean;
  function Renegotiate: Boolean;

  function Read(var ABuffer; ACount: Integer): Integer;
  function Write(const ABuffer; ACount: Integer): Integer;
  function ReadString(out AStr: string): Boolean;
  function WriteString(const AStr: string): Boolean;

  function WantRead: Boolean;
  function WantWrite: Boolean;
  function GetError(ARet: Integer): TSSLErrorCode;

  function GetConnectionInfo: TSSLConnectionInfo; // 编译期 deprecated，仅兼容保留；新代码优先走 ISSLConnectionInfo.GetConnectionInfo
  function GetProtocolVersion: TSSLProtocolVersion;
  function GetCipherName: string;
  function GetPeerCertificate: ISSLCertificate;
  function GetPeerCertificateChain: TSSLCertificateArray; // 编译期 deprecated，仅兼容保留；新代码优先走 ISSLCertificateVerification owner surface
  function GetVerifyResult: Integer; // 编译期 deprecated，仅兼容保留；新代码优先走 ISSLCertificateVerification owner surface
  function GetVerifyResultString: string; // 编译期 deprecated，仅兼容保留；新代码优先走 ISSLCertificateVerification owner surface

  function GetSession: ISSLSession; // 编译期 deprecated，仅兼容保留；新代码优先走 ISSLSessionResumption.GetSession
  procedure SetSession(ASession: ISSLSession); // 编译期 deprecated，仅兼容保留；新代码优先走 ISSLSessionResumption.SetSession
  function IsSessionReused: Boolean; // 编译期 deprecated，仅兼容保留；新代码优先走 ISSLSessionResumption.IsSessionReused
  function GetSelectedALPNProtocol: string; // 编译期 deprecated，仅兼容保留；新代码优先走 ISSLConnectionInfo.GetSelectedALPNProtocol

  function IsConnected: Boolean;
  function GetState: string;
  function GetStateString: string; // 编译期 deprecated，仅兼容保留；新代码优先走 ISSLConnectionInfo.GetStateString
  procedure SetTimeout(ATimeout: Integer);
  function GetTimeout: Integer;
  procedure SetBlocking(ABlocking: Boolean);
  function GetBlocking: Boolean;
  function GetContext: ISSLContext; // 编译期 deprecated，仅兼容保留；新代码优先走 ISSLConnectionInfo.GetContext

  function GetHealthStatus: TSSLHealthStatus; // 编译期 deprecated，仅兼容保留；新代码优先走 ISSLDiagnostics.GetHealthStatus
  function IsHealthy: Boolean; // 编译期 deprecated，仅兼容保留；新代码优先走 ISSLDiagnostics.IsHealthy
  function GetDiagnosticInfo: TSSLDiagnosticInfo; // 编译期 deprecated，仅兼容保留；新代码优先走 ISSLDiagnostics.GetDiagnosticInfo
  function GetPerformanceMetrics: TSSLPerformanceMetrics; // 编译期 deprecated，仅兼容保留；新代码优先走 ISSLDiagnostics.GetPerformanceMetrics

  function GetOCSPStaplingEnabled: Boolean; // 编译期 deprecated，仅兼容保留；新代码优先走 ISSLOCSPStapling.GetOCSPStaplingEnabled
  function GetOCSPResponse: TBytes; // 编译期 deprecated，仅兼容保留；新代码优先走 ISSLOCSPStapling.GetOCSPResponse
  function IsOCSPResponseVerified: Boolean; // 编译期 deprecated，仅兼容保留；新代码优先走 ISSLOCSPStapling.IsOCSPResponseVerified
  function GetOCSPResponseStatus: string; // 编译期 deprecated，仅兼容保留；新代码优先走 ISSLOCSPStapling.GetOCSPResponseStatus
end;
```

客户端特有的 per-connection SNI surface 仍通过扩展接口暴露：

```pascal
ISSLClientConnection = interface(ISSLConnection)
  procedure SetServerName(const AServerName: string);
  function GetServerName: string;
end;
```

#### `v1.x` compatibility-core note

- `ISSLConnection` 当前仍保留一批未来可能继续下沉到可选接口的能力面；当前文档只记录 **源码真相**，不等于推荐把更多能力继续塞回核心接口。
- `GetConnectionInfo` 在 `ISSLConnection` 上仅作为 `v1.x` compatibility-core mirror 保留；当前源码声明已经是编译期 `deprecated`，需要完整连接信息记录时，新代码优先通过 `ISSLConnectionInfo.GetConnectionInfo`。
- `GetContext` 在 `ISSLConnection` 上仅作为 `v1.x` compatibility-core mirror 保留；当前源码声明已经是编译期 `deprecated`，需要连接所属 context 引用时，新代码优先通过 `ISSLConnectionInfo.GetContext`。
- `GetSelectedALPNProtocol` 在 `ISSLConnection` 上仅作为 `v1.x` compatibility-core mirror 保留；当前源码声明已经是编译期 `deprecated`，需要当前连接的协商 ALPN 结果时，新代码优先通过 `ISSLConnectionInfo.GetSelectedALPNProtocol`。
- `GetStateString` 在 `ISSLConnection` 上仅作为 `v1.x` compatibility-core mirror 保留；当前源码声明已经是编译期 `deprecated`，需要后端相关状态描述时，新代码优先通过 `ISSLConnectionInfo.GetStateString`。
- `ReadString` / `WriteString` 继续作为 `v1.x` convenience-core 文本 helper 保留；框架/transport 集成优先使用 `Read` / `Write`。
- `SetTimeout` / `GetTimeout` 继续作为 `v1.x` connection-adjacent convenience surface 保留；新代码优先在构建阶段使用 `TSSLConnectionBuilder.WithTimeout(...)` / `TSSLConnector.WithTimeout(...)` / `TSSLAcceptor.WithTimeout(...)`。
- `SetBlocking` / `GetBlocking` 继续作为 `v1.x` connection-adjacent convenience surface 保留；新代码优先在构建阶段使用 `TSSLConnectionBuilder.WithBlocking(...)`。
- `GetHealthStatus` / `IsHealthy` / `GetDiagnosticInfo` / `GetPerformanceMetrics` 也由 `ISSLDiagnostics` 暴露。
- `GetHealthStatus` / `IsHealthy` / `GetDiagnosticInfo` / `GetPerformanceMetrics` 在 `ISSLConnection` 上当前也只作为 `v1.x` compatibility-core mirror 保留；当前源码声明已经是编译期 `deprecated`，需要诊断/健康/性能信息时，新代码优先通过 `ISSLDiagnostics` owner surface 访问。
- `GetSession` / `SetSession` / `IsSessionReused` 也由 `ISSLSessionResumption` 暴露。
- `GetSession` / `SetSession` / `IsSessionReused` 在 `ISSLConnection` 上当前也只作为 `v1.x` compatibility-core mirror 保留；当前源码声明已经是编译期 `deprecated`，需要保存/恢复会话或读取复用命中结果时，新代码优先通过 `ISSLSessionResumption` owner surface 访问。
- `GetPeerCertificateChain` / `GetVerifyResult` / `GetVerifyResultString` 也由 `ISSLCertificateVerification` 暴露。
- `GetPeerCertificateChain` / `GetVerifyResult` / `GetVerifyResultString` 在 `ISSLConnection` 上当前也只作为 `v1.x` compatibility-core mirror 保留；当前源码声明已经是编译期 `deprecated`，需要证书验证链或验证结果时，新代码优先通过 `ISSLCertificateVerification` owner surface 访问。
- `GetOCSPStaplingEnabled` / `GetOCSPResponse` / `IsOCSPResponseVerified` / `GetOCSPResponseStatus` 在 `ISSLConnection` 上当前也只作为 `v1.x` compatibility-core mirrors 保留；当前源码声明已经是编译期 `deprecated`，需要 stapled OCSP runtime state 时，新代码优先通过 `ISSLOCSPStapling` owner surface 访问。
- `GetNativeHandle` 不属于 `ISSLContext` / `ISSLConnection` 核心接口；当前应通过可选接口 `ISSLNativeHandleAccess` 访问。
- 下列旧名字不是当前活跃源码：`GetCipherBits`、`VerifyPeerCertificate`、`GetSessionID`、`IsSessionResumed`、`GetSessionData`、`SetSessionData`。

#### 基本用法

```pascal
var
  LConn: ISSLConnection;
  LData: string;
  LServerName: string;
begin
  LServerName := 'example.com';
  LConn := LContext.CreateConnection(MySocket);
  (LConn as ISSLClientConnection).SetServerName(LServerName);
  if LConn.Connect then
  begin
    // 发送 HTTP 请求
    if LConn.WriteString('GET / HTTP/1.1'#13#10#13#10) then
      WriteLn('请求发送成功')
    else
      WriteLn('请求发送失败');

    // 读取响应
    if LConn.ReadString(LData) then
      WriteLn('响应: ', LData)
    else
      WriteLn('读取响应失败');

    LConn.Shutdown;
  end;
end;
```

#### 读取大数据

```pascal
var
  LConn: ISSLConnection;
  LChunk: string;
  LFullData: string;
  LServerName: string;
begin
  LServerName := 'example.com';
  LConn := LContext.CreateConnection(MySocket);
  (LConn as ISSLClientConnection).SetServerName(LServerName);
  if LConn.Connect then
  begin
    LConn.WriteString('GET /large-file HTTP/1.1'#13#10#13#10);

    // 循环读取直到没有更多数据
    LFullData := '';
    while LConn.ReadString(LChunk) do
    begin
      LFullData := LFullData + LChunk;
      WriteLn('已读取: ', Length(LFullData), ' 字节');
    end;

    WriteLn('总共读取: ', Length(LFullData), ' 字节');
    LConn.Shutdown;
  end;
end;
```

#### 连接信息与状态

```pascal
var
  LConn: ISSLConnection;
  LConnInfoAccess: ISSLConnectionInfo;
  LInfo: TSSLConnectionInfo;
  LServerName: string;
begin
  LServerName := 'example.com';
  LConn := LContext.CreateConnection(MySocket);
  (LConn as ISSLClientConnection).SetServerName(LServerName);

  if LConn.Connect and Supports(LConn, ISSLConnectionInfo, LConnInfoAccess) then
  begin
    LInfo := LConnInfoAccess.GetConnectionInfo;
    WriteLn('协议版本: ', GetProtocolName(LInfo.ProtocolVersion));
    WriteLn('密码套件: ', LInfo.CipherSuite);
    WriteLn('ALPN: ', LConnInfoAccess.GetSelectedALPNProtocol);
    WriteLn('状态: ', LConnInfoAccess.GetStateString);
    WriteLn('连接信息里的 ServerName: ', LInfo.ServerName);
    LConn.Shutdown;
  end;
end;
```

如果你在写新代码，并且需要连接信息 / ALPN / 状态字符串这组 mirrors，优先通过 `ISSLConnectionInfo` 获取。
需要上下文引用时，也优先通过 `ISSLConnectionInfo.GetContext` 获取。
`GetConnectionInfo` 不再应被当作核心 `ISSLConnection` 的主入口；它当前在源码声明里也已经是编译期 `deprecated`。拿完整连接信息记录请优先通过 `ISSLConnectionInfo.GetConnectionInfo`。
`GetContext` 也不再应被当作核心 `ISSLConnection` 的主入口；它当前在源码声明里同样已经是编译期 `deprecated`。需要连接所属 context 引用时，请优先通过 `ISSLConnectionInfo.GetContext`。
`GetSelectedALPNProtocol` 也不再应被当作核心 `ISSLConnection` 的主入口；它当前在源码声明里同样已经是编译期 `deprecated`。需要当前连接的协商 ALPN 结果时，请优先通过 `ISSLConnectionInfo.GetSelectedALPNProtocol`。
`GetStateString` 也不再应被当作核心 `ISSLConnection` 的主入口；它当前在源码声明里同样已经是编译期 `deprecated`。需要后端相关状态描述时，请优先通过 `ISSLConnectionInfo.GetStateString`。

#### Session 复用

```pascal
var
  LConn1, LConn2: ISSLConnection;
  LResumption1, LResumption2: ISSLSessionResumption;
  LSession: ISSLSession;
begin
  LConn1 := LContext.CreateConnection(Socket1);
  (LConn1 as ISSLClientConnection).SetServerName('api.example.com');
  if LConn1.Connect and Supports(LConn1, ISSLSessionResumption, LResumption1) then
  begin
    LSession := LResumption1.GetSession;
    if Assigned(LSession) then
      WriteLn('Session ID: ', LSession.GetID);
    LConn1.Shutdown;
  end;

  LConn2 := LContext.CreateConnection(Socket2);
  (LConn2 as ISSLClientConnection).SetServerName('api.example.com');
  if Supports(LConn2, ISSLSessionResumption, LResumption2) and
     Assigned(LSession) and LSession.IsValid and LSession.IsResumable then
    LResumption2.SetSession(LSession);

  if LConn2.Connect then
  begin
    WriteLn('是否复用: ', BoolToStr(LResumption2.IsSessionReused, True));
    LConn2.Shutdown;
  end;
end;
```

如果你在写新代码，并且需要保存/恢复 TLS 会话，优先通过 `ISSLSessionResumption.GetSession` 获取会话对象。
需要在下一条连接上注入待恢复会话时，也优先通过 `ISSLSessionResumption.SetSession`。
检查当前握手是否实际命中了恢复路径时，也优先通过 `ISSLSessionResumption.IsSessionReused`。

> MbedTLS 当前边界：已发布 `GetSession / SetSession` 与 session serialize / deserialize path，但 local source/header truth 只有 `mbedtls_ssl_set_session` / `mbedtls_ssl_get_session` / `mbedtls_ssl_session_load/save`，没有像 `SSL_session_reused` / `wolfSSL_session_reused` 那样的 public reused getter。
> 因此当前 MbedTLS source/contract truth 只稳定证明“configured session 不会被误报成 observed resumed handshake”；不要把 `SetSession(...)` 自动读成 runtime reuse proof。

### WinSSL Session 管理

WinSSL 后端提供 `ISSLSession` 复用能力，但活跃公共接口以当前源码为准，不再使用旧的 `GetSessionID` / `GetSessionData` 风格。

> 当前 dedicated Windows CI runtime truth 已由 run `26037518301` 固定：
> `observed_reuse=false`，`session_configured=true`。
> 这意味着 WinSSL 的 session-resumption public surface 已可安全使用，但 native resumed-handshake 行为在 fafafa.ssl 中仍应视为实验性能力，而不是已稳定命中的 runtime 结论。
> 由于 canonical shared path 当前继续撤下 live `SECPKG_ATTR_SESSION_INFO` probe 以避免 Windows AV，`observed_reuse` 在 broader/shared lane 上应按 conservative public truth 理解；更深 native evidence 需要查看 opt-in isolated native probe 输出的 `native_observed_reuse` / `native_probe_succeeded`。
> 按当前 Schannel truth，client-side reconnect/cache lookup 仍主要取决于相同的 `target name` 与相同的 context-level `credential handle`；
> `ISSLSessionResumption.SetSession(...)` 在 WinSSL 上当前更接近 compatibility metadata surface，而不是 native session-handle injection 点。

#### 核心接口

```pascal
ISSLSession = interface
  function GetID: string;
  function GetCreationTime: TDateTime;
  function GetTimeout: Integer;
  procedure SetTimeout(ATimeout: Integer);
  function IsValid: Boolean;
  function IsResumable: Boolean;

  function GetProtocolVersion: TSSLProtocolVersion;
  function GetCipherName: string;
  function GetPeerCertificate: ISSLCertificate;

  function Serialize: TBytes;
  function Deserialize(const AData: TBytes): Boolean;

  function Clone: ISSLSession;
end;
```

#### WinSSL Session 实现

WinSSL 后端通过以下类实现 Session 管理：

- **TWinSSLSession**: 实现 ISSLSession 接口，封装 Schannel Session 数据
- **TWinSSLSessionManager**: Session 缓存管理器，支持自动过期和清理

#### 使用示例

**基本 Session 复用**:

```pascal
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LConn1, LConn2: ISSLConnection;
  LResumption1, LResumption2: ISSLSessionResumption;
  LSession: ISSLSession;
begin
  // 创建 WinSSL 库
  LLib := TSSLFactory.GetLibraryInstance(sslWinSSL);
  LLib.Initialize;

  LContext := LLib.CreateContext(sslCtxClient);

  // 第一次连接 - 完整握手
  LConn1 := LContext.CreateConnection(Socket1);
  (LConn1 as ISSLClientConnection).SetServerName('api.example.com');
  if LConn1.Connect and Supports(LConn1, ISSLSessionResumption, LResumption1) then
  begin
    WriteLn('第一次连接成功');

    LSession := LResumption1.GetSession;
    if Assigned(LSession) then
      WriteLn('Session ID: ', LSession.GetID);

    LConn1.Shutdown;
  end;

  LConn2 := LContext.CreateConnection(Socket2);
  (LConn2 as ISSLClientConnection).SetServerName('api.example.com');
  if Supports(LConn2, ISSLSessionResumption, LResumption2) and Assigned(LSession) then
    LResumption2.SetSession(LSession);  // 保存 metadata/compatibility surface；Schannel reconnect truth 仍以 target name + credential handle 为主

  if LConn2.Connect then
  begin
    WriteLn('第二次连接成功');

    if LResumption2.IsSessionReused then
      WriteLn('✓ 当前连接命中了 resumed handshake')
    else
      WriteLn('✗ 当前 dedicated Windows CI runtime truth 仍可能是 observed_reuse=false / session_configured=true');

    LConn2.Shutdown;
  end;
end;
```

**Session 序列化缓存**:

```pascal
var
  LConn: ISSLConnection;
  LResumption: ISSLSessionResumption;
  LSession: ISSLSession;
  LSerialized: TBytes;
begin
  LConn := LContext.CreateConnection(MySocket);
  (LConn as ISSLClientConnection).SetServerName('api.example.com');

  if LConn.Connect and Supports(LConn, ISSLSessionResumption, LResumption) then
  begin
    LSession := LResumption.GetSession;
    if Assigned(LSession) and LSession.IsResumable then
    begin
      LSerialized := LSession.Serialize;
      WriteLn('序列化后的 Session 字节数: ', Length(LSerialized));
    end;
    LConn.Shutdown;
  end;
end;
```

#### 性能优化建议

1. **长连接场景**: 对于需要频繁连接同一服务器的应用（如 REST API 客户端），可以先保留 Session capture/injection 路径；等 Windows runtime 真正证实 resumed handshake 命中后，再把它当作稳定优化项
2. **Session 有效期**: WinSSL Session 默认有效期由 Windows 系统策略控制，通常为 10 小时
3. **内存管理**: Session 数据较小（通常 < 1KB），可以安全缓存大量 Session
4. **线程安全**: TWinSSLSession 对象是线程安全的，可以在多线程环境中共享

#### 与 OpenSSL 的差异

| 特性               | WinSSL                                             | OpenSSL            |
| ------------------ | -------------------------------------------------- | ------------------ |
| Session 存储       | 自动（凭据句柄缓存）                               | 手动（需要序列化） |
| Session 有效期     | 系统策略控制                                       | 应用程序控制       |
| 跨进程共享         | 不支持                                             | 支持（通过序列化） |
| 当前 runtime truth | `observed_reuse=false` / `session_configured=true` | 常见场景可见收益   |

#### 错误处理

```pascal
var
  LConn: ISSLConnection;
  LResumption: ISSLSessionResumption;
  LSession: ISSLSession;
  LServerName: string;
begin
  LServerName := 'example.com';
  LConn := LContext.CreateConnection(MySocket);

  if Supports(LConn, ISSLSessionResumption, LResumption) and
     Assigned(LSession) and LSession.IsValid and LSession.IsResumable then
    LResumption.SetSession(LSession)
  else
    WriteLn('警告: Session 无效或已过期，将执行完整握手');

  (LConn as ISSLClientConnection).SetServerName(LServerName);

  if LConn.Connect and Supports(LConn, ISSLSessionResumption, LResumption) then
  begin
    if not LResumption.IsSessionReused then
    begin
      WriteLn('注意: Session 未复用，可能原因：');
      WriteLn('  - Session 已过期');
      WriteLn('  - 服务器不支持 Session 复用');
      WriteLn('  - 服务器要求重新验证');
    end;
  end;
end;
```

#### 调试和监控

```pascal
var
  LConn: ISSLConnection;
  LResumption: ISSLSessionResumption;
  LSession: ISSLSession;
  LServerName: string;
begin
  LServerName := 'example.com';
  LConn := LContext.CreateConnection(MySocket);
  (LConn as ISSLClientConnection).SetServerName(LServerName);

  if LConn.Connect and Supports(LConn, ISSLSessionResumption, LResumption) then
  begin
    LSession := LResumption.GetSession;

    if Assigned(LSession) then
    begin
      WriteLn('Session 信息:');
      WriteLn('  ID: ', LSession.GetID);
      WriteLn('  创建时间: ', DateTimeToStr(LSession.GetCreationTime));
      WriteLn('  超时(秒): ', LSession.GetTimeout);
      WriteLn('  是否可复用: ', BoolToStr(LSession.IsResumable, True));
      WriteLn('  是否已复用: ', BoolToStr(LResumption.IsSessionReused, True));
      WriteLn('  协议版本: ', GetProtocolName(LSession.GetProtocolVersion));
      WriteLn('  密码套件: ', LSession.GetCipherName);
    end;
  end;
end;
```

#### Phase 3.3: 监控和诊断示例

**健康检查**:

```pascal
var
  LConn: ISSLConnection;
  LDiag: ISSLDiagnostics;
  LHealth: TSSLHealthStatus;
  LServerName: string;
begin
  LServerName := 'example.com';
  LConn := LContext.CreateConnection(MySocket);
  (LConn as ISSLClientConnection).SetServerName(LServerName);

  if LConn.Connect and Supports(LConn, ISSLDiagnostics, LDiag) then
  begin
    // 快速健康检查
    if LDiag.IsHealthy then
      WriteLn('✓ 连接健康')
    else
      WriteLn('✗ 连接不健康');

    // 获取详细健康状态
    LHealth := LDiag.GetHealthStatus;
    WriteLn('健康状态详情:');
    WriteLn('  已连接: ', BoolToStr(LHealth.IsConnected, True));
    WriteLn('  握手完成: ', BoolToStr(LHealth.HandshakeComplete, True));
    WriteLn('  最后错误: ', GetErrorName(LHealth.LastError));
    WriteLn('  发送字节: ', LHealth.BytesSent);
    WriteLn('  接收字节: ', LHealth.BytesReceived);
    WriteLn('  连接时长: ', LHealth.ConnectionAge, ' 秒');
  end;
end;
```

**性能监控**:

```pascal
var
  LConn: ISSLConnection;
  LDiag: ISSLDiagnostics;
  LPerf: TSSLPerformanceMetrics;
  LServerName: string;
begin
  LServerName := 'example.com';
  LConn := LContext.CreateConnection(MySocket);
  (LConn as ISSLClientConnection).SetServerName(LServerName);

  if LConn.Connect and Supports(LConn, ISSLDiagnostics, LDiag) then
  begin
    // 获取性能指标
    LPerf := LDiag.GetPerformanceMetrics;

    WriteLn('性能指标:');
    WriteLn('  握手时间: ', LPerf.HandshakeTime, ' ms');
    WriteLn('  总传输: ', LPerf.TotalBytesTransferred, ' bytes');
    WriteLn('  Session 复用: ', BoolToStr(LPerf.SessionReused, True));

    // 性能分析
    if LPerf.HandshakeTime < 50 then
      WriteLn('✓ 握手性能优秀')
    else if LPerf.HandshakeTime < 100 then
      WriteLn('⚠ 握手性能良好')
    else
      WriteLn('✗ 握手性能需要优化');
  end;
end;
```

**完整诊断**:

```pascal
var
  LConn: ISSLConnection;
  LDiagExt: ISSLDiagnostics;
  LDiag: TSSLDiagnosticInfo;
  I: Integer;
  LServerName: string;
begin
  LServerName := 'example.com';
  LConn := LContext.CreateConnection(MySocket);
  (LConn as ISSLClientConnection).SetServerName(LServerName);

  if LConn.Connect and Supports(LConn, ISSLDiagnostics, LDiagExt) then
  begin
    // 获取完整诊断信息
    LDiag := LDiagExt.GetDiagnosticInfo;

    WriteLn('=== 完整诊断报告 ===');

    // 连接信息
    WriteLn('连接信息:');
    WriteLn('  协议: ', GetProtocolName(LDiag.ConnectionInfo.ProtocolVersion));
    WriteLn('  密码套件: ', LDiag.ConnectionInfo.CipherSuite);
    WriteLn('  密钥长度: ', LDiag.ConnectionInfo.KeySize, ' bits');

    // 健康状态
    WriteLn('健康状态:');
    WriteLn('  状态: ', BoolToStr(LDiag.HealthStatus.IsConnected, True));
    WriteLn('  连接时长: ', LDiag.HealthStatus.ConnectionAge, ' 秒');

    // 性能指标
    WriteLn('性能指标:');
    WriteLn('  握手时间: ', LDiag.PerformanceMetrics.HandshakeTime, ' ms');
    WriteLn('  总传输: ', LDiag.PerformanceMetrics.TotalBytesTransferred, ' bytes');

    // 错误历史
    if Length(LDiag.ErrorHistory) > 0 then
    begin
      WriteLn('错误历史:');
      for I := 0 to High(LDiag.ErrorHistory) do
        WriteLn('  [', DateTimeToStr(LDiag.ErrorHistory[I].Timestamp), '] ',
                GetErrorName(LDiag.ErrorHistory[I].ErrorCode), ': ',
                LDiag.ErrorHistory[I].ErrorMessage);
    end;
  end;
end;
```

**全局统计监控**:

```pascal
var
  LLib: ISSLLibrary;
  LStats: TSSLStatistics;
begin
  LLib := TSSLFactory.GetLibraryInstance(sslWinSSL);
  LLib.Initialize;

  // ... 执行多个连接 ...

  // 获取全局统计
  LStats := LLib.GetStatistics;

  WriteLn('=== 全局统计信息 ===');
  WriteLn('连接统计:');
  WriteLn('  总连接数: ', LStats.ConnectionsTotal);
  WriteLn('  活动连接: ', LStats.ConnectionsActive);
  WriteLn('  成功握手: ', LStats.HandshakesSuccessful);
  WriteLn('  失败握手: ', LStats.HandshakesFailed);

  WriteLn('性能统计:');
  WriteLn('  平均握手时间: ', LStats.HandshakeTimeAvg, ' ms');
  WriteLn('  最小握手时间: ', LStats.HandshakeTimeMin, ' ms');
  WriteLn('  最大握手时间: ', LStats.HandshakeTimeMax, ' ms');

  WriteLn('Session 复用统计:');
  WriteLn('  Session 创建: ', LStats.SessionsCreated);
  WriteLn('  Session 复用: ', LStats.SessionsReused);
  WriteLn('  复用率: ', Format('%.2f%%', [LStats.SessionReuseRate]));

  // 重置统计
  LLib.ResetStatistics;

  LLib.Finalize;
end;
```

**生产环境监控**:

```pascal
var
  LLib: ISSLLibrary;
  LConn: ISSLConnection;
  LStats: TSSLStatistics;
  LHealth: TSSLHealthStatus;
begin
  LLib := TSSLFactory.GetLibraryInstance(sslWinSSL);
  LLib.Initialize;

  // 定期监控循环
  while True do
  begin
    // 检查全局统计
    LStats := LLib.GetStatistics;

    // 告警：握手失败率过高
    if (LStats.HandshakesSuccessful + LStats.HandshakesFailed > 0) and
       (LStats.HandshakesFailed * 100 / (LStats.HandshakesSuccessful + LStats.HandshakesFailed) > 5) then
      WriteLn('⚠ 告警：握手失败率超过 5%');

    // 告警：平均握手时间过长
    if LStats.HandshakeTimeAvg > 200 then
      WriteLn('⚠ 告警：平均握手时间超过 200ms');

    // 告警：Session 复用率过低
    if (LStats.SessionsCreated + LStats.SessionsReused > 10) and
       (LStats.SessionReuseRate < 50) then
      WriteLn('⚠ 告警：Session 复用率低于 50%');

    // 检查活动连接健康状态
    for LConn in ActiveConnections do
    begin
      if Supports(LConn, ISSLDiagnostics, LDiag) and
         (not LDiag.IsHealthy) then
      begin
        LHealth := LDiag.GetHealthStatus;
        WriteLn('⚠ 不健康连接: 最后错误 ', GetErrorName(LHealth.LastError));
      end;
    end;

    Sleep(60000); // 每分钟检查一次
  end;
end;
```

---

## 数据类型

### TSSLLibraryType

```pascal
TSSLLibraryType = (
  sslAutoDetect,  // 自动检测可用库
  sslOpenSSL,     // OpenSSL 后端
  sslWolfSSL,     // WolfSSL 后端
  sslMbedTLS,     // MbedTLS 后端
  sslWinSSL,      // Windows Schannel 后端
  sslFreePascal   // 纯 FreePascal 实现
);
```

### TSSLProtocolVersion

```pascal
TSSLProtocolVersion = (
  sslProtocolSSL20,   // SSL 2.0 (已废弃)
  sslProtocolSSL30,   // SSL 3.0 (已废弃)
  sslProtocolTLS10,   // TLS 1.0
  sslProtocolTLS11,   // TLS 1.1
  sslProtocolTLS12,   // TLS 1.2
  sslProtocolTLS13    // TLS 1.3
);
TSSLProtocolVersions = set of TSSLProtocolVersion;
```

### TSSLContextType

```pascal
TSSLContextType = (
  sslCtxClient,  // 客户端上下文
  sslCtxServer   // 服务端上下文
);
```

### TSSLVerifyMode

```pascal
TSSLVerifyMode = (
  sslVerifyNone,       // 不验证
  sslVerifyPeer,       // 验证对等方
  sslVerifyFailIfNoPeerCert,  // 无证书时失败
  sslVerifyClientOnce  // 仅验证一次客户端
);
TSSLVerifyModes = set of TSSLVerifyMode;
```

### TSSLCertVerifyFlag

```pascal
TSSLCertVerifyFlag = (
  sslCertVerifyDefault,         // 默认验证
  sslCertVerifyCheckRevocation, // 检查吊销（CRL）
  sslCertVerifyCheckOCSP,       // 使用 OCSP
  sslCertVerifyIgnoreExpiry,    // 忽略过期
  sslCertVerifyIgnoreHostname,  // 忽略主机名
  sslCertVerifyAllowSelfSigned, // 允许自签名
  sslCertVerifyStrictChain,     // 严格证书链
  sslCertVerifyCheckCRL         // 检查 CRL 列表
);
TSSLCertVerifyFlags = set of TSSLCertVerifyFlag;
```

### TSSLCertVerifyResult

```pascal
TSSLCertVerifyResult = record
  Success: Boolean;         // 验证是否成功
  ErrorCode: Cardinal;      // 错误代码
  ErrorMessage: string;     // 友好的错误消息
  ChainStatus: Cardinal;    // 证书链状态
  RevocationStatus: Cardinal; // 吊销状态
  DetailedInfo: string;     // 详细信息
end;
```

`fafafa.ssl` 主门面当前也 re-export 证书 public surface 常用 supporting types（如 `TSSLStringArray` / `TSSLCertVerifyResult` / `TSSLCertVerifyFlags`）。

### TSSLConnectionInfo

```pascal
TSSLConnectionInfo = record
  ProtocolVersion: TSSLProtocolVersion;  // 协议版本
  CipherSuite: string;                   // 密码套件名称
  CipherSuiteId: Word;                   // 密码套件ID
  KeyExchange: TSSLKeyExchange;          // 密钥交换算法
  Cipher: TSSLCipher;                    // 加密算法
  Hash: TSSLHash;                        // 哈希算法
  KeySize: Integer;                      // 密钥长度（位）
  MacSize: Integer;                      // 认证/MAC/tag 长度（字节，best-effort）
  IsResumed: Boolean;                    // 是否为恢复的会话
  SessionId: string;                     // 会话ID
  CompressionMethod: string;             // 压缩方法
  ServerName: string;                    // SNI服务器名称
  ALPNProtocol: string;                  // ALPN协商的协议
  PeerCertificate: TSSLCertificateInfo;  // 对端证书信息
end;
```

**说明**:

- 新代码若要获取这份结构，优先通过 `ISSLConnectionInfo.GetConnectionInfo`；`ISSLConnection.GetConnectionInfo` 当前只作为 `v1.x` compatibility-core mirror 保留，且源码声明已经是编译期 `deprecated`
- `GetConnectionInfo` 方法返回此结构，包含连接的完整信息
- 用于监控、诊断和安全审计
- WinSSL 后端通过 `QueryContextAttributesW` API best-effort 获取这些信息；真实 cipher-suite id/name 会优先走 Schannel `SECPKG_ATTR_CIPHER_INFO`
- `ProtocolVersion` / `CipherSuite` / `IsResumed` / `ALPNProtocol` 这组通用字段由共享连接层保证最小可观测语义
- `ServerName` 在连接对象已持有该 metadata 时由共享连接层补齐
- `SessionId` 在连接/握手已经建立且后端可返回当前 session 时由共享连接层补齐
- `PeerCertificate` 在连接对象可暴露当前对端证书时由共享连接层补齐
- `CipherSuiteId` 对标准 TLS 1.3 suite name 会先由共享层做 best-effort 推导；OpenSSL / WinSSL 这类后端也可能直接提供 low-level truth
- `Cipher` / `Hash` / `KeySize` 这组字段会先由共享连接层基于 negotiated cipher-suite name 做 best-effort 推导
- `KeyExchange` 在 cipher-suite name 显式携带旧式密钥交换前缀时，也会由共享层做 best-effort 推导
- `MacSize` 现在会先由共享连接层对可识别的 AEAD suite name 做 best-effort 推导：
  - `...GCM` / `...POLY1305` / `...OCB` / `...CCM` -> `16`
  - `...CCM_8` -> `8`
- OpenSSL 在明确识别到 legacy/non-AEAD cipher 且底层 digest truth 可用时，也会补回真实 `MacSize`
- WinSSL 只有在共享层无法从 suite name 得到稳定值时，才回退到 `dwHashStrength div 8`
- legacy non-AEAD suites 以及后端未提供稳定 truth 的场景，`MacSize` 仍返回默认值 `0`

---

### Phase 3.3: 监控和诊断类型

#### TSSLStatistics

```pascal
TSSLStatistics = record
  // 连接统计
  ConnectionsTotal: Int64;
  ConnectionsActive: Integer;
  HandshakesSuccessful: Int64;
  HandshakesFailed: Int64;
  BytesSent: Int64;
  BytesReceived: Int64;
  SessionCacheHits: Int64;
  SessionCacheMisses: Int64;
  RenegotiationsCount: Int64;
  AlertsSent: Int64;
  AlertsReceived: Int64;

  // Phase 3.3: 性能统计
  HandshakeTimeTotal: Int64;      // 总握手时间（毫秒）
  HandshakeTimeMin: Integer;      // 最小握手时间（毫秒）
  HandshakeTimeMax: Integer;      // 最大握手时间（毫秒）
  HandshakeTimeAvg: Integer;      // 平均握手时间（毫秒）

  // Phase 3.3: Session 复用统计
  SessionsReused: Int64;          // Session 复用次数
  SessionsCreated: Int64;         // 新 Session 创建次数
  SessionReuseRate: Double;       // Session 复用率（百分比 0-100）
end;
```

**说明**:

- 通过 `ISSLLibrary.GetStatistics` 获取全局统计信息
- 性能统计使用高精度计时器（QueryPerformanceCounter）
- Session 复用率自动计算：`SessionsReused / (SessionsReused + SessionsCreated) * 100`
- 使用 `ISSLLibrary.ResetStatistics` 重置所有计数器

#### TSSLHealthStatus

```pascal
TSSLHealthStatus = record
  IsConnected: Boolean;           // 是否已连接
  HandshakeComplete: Boolean;     // 握手是否完成
  LastError: TSSLErrorCode;       // 最后一个错误码
  LastErrorTime: TDateTime;       // 最后一个错误的时间戳
  BytesSent: Int64;               // 已发送字节数
  BytesReceived: Int64;           // 已接收字节数
  ConnectionAge: Integer;         // 连接存活时间（秒）
end;
```

**说明**:

- 通过 `ISSLDiagnostics.GetHealthStatus` 获取连接健康状态
- 用于快速诊断连接问题和监控连接状态
- `ConnectionAge` 从连接创建时开始计算
- 在 `ISSLConnection` 上当前只是编译期 `deprecated` 的 compatibility mirror。
- 对新代码，优先通过 `ISSLDiagnostics.GetHealthStatus` 获取该结构，而不是直接走核心 `ISSLConnection` mirror。

#### TSSLPerformanceMetrics

```pascal
TSSLPerformanceMetrics = record
  HandshakeTime: Integer;         // 握手时间（毫秒）
  FirstByteTime: Integer;         // 首字节时间（毫秒）
  TotalBytesTransferred: Int64;   // 总传输字节数
  AverageLatency: Integer;        // 平均延迟（毫秒）
  SessionReused: Boolean;         // Session 是否复用
end;
```

**说明**:

- 通过 `ISSLDiagnostics.GetPerformanceMetrics` 获取性能指标
- 用于性能分析和优化
- `HandshakeTime` 使用高精度计时器测量
- 在 `ISSLConnection` 上当前只是编译期 `deprecated` 的 compatibility mirror。
- 对新代码，优先通过 `ISSLDiagnostics.GetPerformanceMetrics` 获取该结构，而不是直接走核心 `ISSLConnection` mirror。

#### TSSLErrorRecord

```pascal
TSSLErrorRecord = record
  ErrorCode: TSSLErrorCode;       // 错误码
  ErrorMessage: string;           // 错误消息
  Timestamp: TDateTime;           // 错误时间戳
end;
```

**说明**:

- 用于错误历史跟踪
- 连接维护最近 10 个错误的循环缓冲区

#### TSSLDiagnosticInfo

```pascal
TSSLDiagnosticInfo = record
  ConnectionInfo: TSSLConnectionInfo;      // 连接信息
  HealthStatus: TSSLHealthStatus;          // 健康状态
  PerformanceMetrics: TSSLPerformanceMetrics;  // 性能指标
  ErrorHistory: array of TSSLErrorRecord;  // 错误历史
end;
```

**说明**:

- 通过 `ISSLDiagnostics.GetDiagnosticInfo` 获取完整诊断信息
- 包含连接的所有监控和诊断数据
- 用于故障排查和性能分析
- 在 `ISSLConnection` 上当前只是编译期 `deprecated` 的 compatibility mirror。
- 对新代码，优先通过 `ISSLDiagnostics.GetDiagnosticInfo` 获取该结构，而不是直接走核心 `ISSLConnection` mirror。

---

## 错误处理

### TSSLErrorCode

```pascal
TSSLErrorCode = (
  sslErrNone,              // 无错误
  sslErrGeneral,           // 一般错误
  sslErrMemory,            // 内存分配错误
  sslErrInvalidParam,      // 无效参数
  sslErrNotInitialized,    // 未初始化
  sslErrProtocol,          // 协议错误
  sslErrHandshake,         // 握手错误
  sslErrCertificate,       // 证书错误
  sslErrCertificateExpired,// 证书过期
  sslErrCertificateRevoked,// 证书被撤销
  sslErrCertificateUnknown,// 未知证书
  sslErrCertificateUntrusted, // 证书不受信任
  sslErrHostnameMismatch,  // 主机名不匹配
  sslErrConnection,        // 连接错误
  sslErrTimeout,           // 超时
  sslErrIO,                // I/O错误
  sslErrWouldBlock,        // 非阻塞操作会阻塞
  sslErrWantRead,          // SSL需要读取
  sslErrWantWrite,         // SSL需要写入
  sslErrUnsupported,       // 不支持的功能
  sslErrLibraryNotFound,   // 库文件未找到
  sslErrFunctionNotFound,  // 函数未找到
  sslErrVersionMismatch,   // 版本不匹配
  sslErrConfiguration,     // 配置错误
  sslErrInvalidData,       // 数据格式错误
  sslErrDecryptionFailed,  // 解密失败
  sslErrEncryptionFailed,  // 加密失败
  sslErrParseFailed,       // 解析失败
  sslErrLoadFailed,        // 加载失败
  sslErrVerificationFailed,// 验证失败
  sslErrKeyDerivationFailed,// 密钥派生失败
  sslErrInvalidFormat,     // 格式无效
  sslErrBufferTooSmall,    // 缓冲区太小
  sslErrResourceExhausted, // 资源耗尽
  sslErrOther              // 其他错误
);
```

### 错误处理函数

```pascal
// OpenSSL
function GetOpenSSLError: Cardinal;
function GetOpenSSLErrorString(aError: Cardinal = 0): string;
procedure ClearOpenSSLErrors;
function ClassifyOpenSSLError(aError: Cardinal): TSSLErrorCode;
function GetFriendlyErrorMessage(aError: Cardinal): string;

// WinSSL
function GetFriendlyErrorMessageCN(aErrorCode: DWORD): string;
function GetFriendlyErrorMessageEN(aErrorCode: DWORD): string;
```

---

## 工具函数

### 高入口工厂与诊断工具

```pascal
class function TSSLFactory.IsLibraryAvailable(ALibType: TSSLLibraryType): Boolean;
class function TSSLFactory.GetLibraryInstance(ALibType: TSSLLibraryType = sslAutoDetect): ISSLLibrary;
class function TSSLFactory.CreateContext(
  AContextType: TSSLContextType;
  ALibType: TSSLLibraryType = sslAutoDetect
): ISSLContext;
function GetOpenSSLVersion: string;
```

高入口普通文档不再把手动 OpenSSL loader 当成应用入口步骤。
如需直接处理 OpenSSL API loader、`PX509` 或 `PEVP_PKEY`，请转到 backend-specific low-level units。

### PKCS#12 Helper

```pascal
function DefaultPKCS12Options: TPKCS12Options;

class function TPKCS12Manager.CreatePKCS12(
  const ACert: ICertificate;
  const AKey: IPrivateKey;
  const AOptions: TPKCS12Options
): TBytes;

class function TPKCS12Manager.CreatePKCS12ToFile(
  const ACert: ICertificate;
  const AKey: IPrivateKey;
  const AFile: string;
  const AOptions: TPKCS12Options
): Boolean;

class function TPKCS12Manager.LoadFromPKCS12(
  const APKCS12: TBytes;
  const APassword: string;
  out ACert: ICertificate;
  out AKey: IPrivateKey
): Boolean;

class function TPKCS12Manager.LoadFromPKCS12File(
  const AFile: string;
  const APassword: string;
  out ACert: ICertificate;
  out AKey: IPrivateKey
): Boolean;
```

这组 helper 当前对应 `OpenSSL` 的完整 PKCS#12 helper/API surface；`WinSSL` 仅发布 PFX/P12 import path，不提供这里的 helper 族。
如果你只需要普通高入口 PKCS#12 导入/导出，优先使用 `fafafa.ssl` 门面重导出的 `TPKCS12Manager` / `DefaultPKCS12Options`；只有在需要直接操作 `PPKCS12` / `PX509` / `PEVP_PKEY` 时，才转到 `fafafa.ssl.openssl.api.pkcs12` / `fafafa.ssl.openssl.api.pem`。

### WinSSL 企业工具

```pascal
var
  LConfig: TSSLEnterpriseConfig;
begin
  LConfig := TSSLEnterpriseConfig.Create;
  try
    LConfig.LoadFromSystem;
    if LConfig.IsFIPSEnabled then
      WriteLn('FIPS mode enabled');
    WriteLn('Trusted roots: ', Length(LConfig.GetTrustedRoots));
    WriteLn('Policies loaded: ', LConfig.GetAllPolicies.Count);
  finally
    LConfig.Free;
  end;
end;
```

`TSSLEnterpriseConfig` 当前 helper 主路径是 `IsFIPSEnabled` / `GetTrustedRoots` / `GetAllPolicies`。
这些 WinSSL 企业 helper 当前提供的是 Windows FIPS policy / 企业证书 / GPO 检测能力，不等于 `ISSLLibrary.GetCapabilities.SupportsFIPSMode=True`。
`IsFIPSModeEnabled(...)` / `GetEnterpriseTrustedRoots(...)` 仍然存在，但当前只应视为 legacy convenience wrappers。

---

## 工厂函数

### 创建后端实例

```pascal
class function TSSLFactory.GetLibraryInstance(ALibType: TSSLLibraryType = sslAutoDetect): ISSLLibrary;
```

`TSSLFactory.GetLibraryInstance(...)` 是当前高入口 public library-entrypoint。
`CreateOpenSSLLibrary` / `CreateWinSSLLibrary` 仍存在，但它们属于 backend-specific low-level creators，不应再作为普通 guide/reference 的默认入口。
`fafafa.ssl` 主门面当前也 re-export `ISSLNativeHandleAccess` 与 capability helper surface（如 `TSSLBackendCapabilities` / `IsFeatureStable(...)` / `GetCapabilitiesDescription(...)`）。
普通 capability / native-handle 查询不必再拆分回 `uses fafafa.ssl.base`。

### 门面便捷 helper（非 TLS bootstrap 主入口）

`TSSLFactory.GetLibraryInstance(...)` / `TSSLConnector` / `TSSLAcceptor` / `TSSLStream` 仍是当前 TLS bootstrap 主入口。

- `CreateDefaultConfig(...)` 当前只是 fresh default-config convenience helper。
  如果你需要 library-owned defaults、持续的 logging policy，或 direct-library default-config truth，
  优先通过 `ISSLLibrary.GetDefaultConfig(...)` / `SetDefaultConfig(...)` 访问。
- `TSSLHelper` 当前保留为证书文件检查 / 随机与摘要工具 / early-data optional-interface convenience helper。
  它不代替 `TSSLFactory` / `TSSLContextBuilder` / `TSSLConnector` 这条主入口。
- `QuickServer(...)` 当前只是 `TSSLFactory.CreateServerContext(...)` 的 convenience bootstrap。
  它只返回配置好的 `ISSLContext`，不负责 socket bind/listen/accept。
- `CreateOCSPClient(...)` / `CreateCRLManager(...)` 当前是证书工具 facade re-export，不是 TLS 连接/bootstrap 入口。
  只有在你显式需要 OCSP/CRL workflow 时，才直接走它们；普通 TLS client/server 建立流程仍然优先通过 context/builder/connector path。

---

## 私钥密码支持

当前 `SupportsPKCS12` 的 published truth 为：`OpenSSL`=完整 PKCS#12 helper/API，`WinSSL`=PFX/P12 bundle import，`FreePascal` / `MbedTLS` / `WolfSSL`=不发布 PKCS#12 bundle surface。
在向 LoadPrivateKey(..., APassword) / LoadPrivateKeyPEM(..., APassword) 传入非空密码前，先检查 `ISSLLibrary.GetCapabilities.SupportsPasswordProtectedKeys`；对 `SupportsPasswordProtectedKeys=False` 的 backend，non-empty `APassword` 应抛出 unsupported，而不是 silent ignore。
当前 WinSSL 仅发布 password-protected PFX/P12 import path；PEM private-key password path 仍为 unsupported。
当前 WinSSL 不发布 bare DER / PKCS#8 private-key load surface；如需这类输入，请改用 PFX/P12 或切换 OpenSSL backend。

---

## 回调类型

在安装非 nil 的 Verify/Password/Info callback 前，先检查 `ISSLLibrary.GetCapabilities.SupportsCallbacks`；对 `SupportsCallbacks=False` 的 backend，non-nil 赋值应抛出 unsupported，`nil` 仅用于清除并回到默认行为。
SupportsCallbacks=True 只表示至少一条 published context callback path 存在；具体 callback 种类仍可能 backend-specific。当前 WinSSL 仅发布 verify/info runtime path，password callback 仍为 unsupported。

```pascal
// 日志回调
TSSLLogLevel = (sslLogDebug, sslLogInfo, sslLogWarning, sslLogError);
TSSLLogCallback = procedure(aLevel: TSSLLogLevel; const aMessage: string) of object;

// 验证回调
TSSLVerifyCallback = function(const aCertificate: TSSLCertificateInfo;
                              const aErrorCode: Integer;
                              const aErrorMessage: string): Boolean of object;

// 密码回调
TSSLPasswordCallback = function(var aPassword: string;
                                const aIsRetry: Boolean): Boolean of object;

// 信息回调
TSSLInfoCallback = procedure(const aWhere: Integer;
                             const aRet: Integer;
                             const aState: string) of object;
```

---

## 常量

### OpenSSL 常量

```pascal
// 验证标志
X509_V_FLAG_CRL_CHECK = $00000004;
X509_V_FLAG_CRL_CHECK_ALL = $00000008;
X509_V_FLAG_NO_CHECK_TIME = $00000200;
X509_V_FLAG_PARTIAL_CHAIN = $00080000;

// SSL 选项
SSL_OP_NO_SSLv2 = $01000000;
SSL_OP_NO_SSLv3 = $02000000;
SSL_OP_NO_TLSv1 = $04000000;
SSL_OP_NO_TLSv1_1 = $10000000;
SSL_OP_NO_TLSv1_2 = $08000000;
SSL_OP_NO_TLSv1_3 = $20000000;
```

### WinSSL 常量

```pascal
// 证书错误
CERT_E_EXPIRED = LONG($800B0101);
CERT_E_UNTRUSTEDROOT = LONG($800B0109);
CERT_E_CN_NO_MATCH = LONG($800B010F);
CERT_E_REVOKED = LONG($800B010C);

// 吊销检查
CERT_CHAIN_REVOCATION_CHECK_END_CERT = $10000000;
CERT_CHAIN_REVOCATION_CHECK_CHAIN = $20000000;
```

---

## 使用示例

### 完整客户端示例

```pascal
program ssl_client;

uses
  fafafa.ssl;

var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LCert: ISSLCertificate;
  LServerName: string;
  LReply: string;
begin
  // 创建并初始化库
  LLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
  if not LLib.Initialize then
  begin
    WriteLn('初始化失败');
    Exit;
  end;

  try
    // 创建客户端上下文
    LContext := LLib.CreateContext(sslCtxClient);
    LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    LContext.LoadCAFile('/etc/ssl/certs/ca-bundle.crt');
    LContext.SetVerifyMode([sslVerifyPeer]);
    LServerName := 'example.com';

    // 创建连接
    LConn := LContext.CreateConnection(MySocket);
    (LConn as ISSLClientConnection).SetServerName(LServerName);
    if LConn.Connect then
    begin
      // 验证证书
      LCert := LConn.GetPeerCertificate;
      if LCert.VerifyHostname(LServerName) then
      begin
        // 发送和接收数据
        LConn.WriteString('Hello, SSL!');
        if LConn.ReadString(LReply) then
          WriteLn('收到: ', LReply);
      end;

      LConn.Shutdown;
    end;
  finally
    LLib.Finalize;
  end;
end.
```

---

## 能力矩阵 API (v1.2.0+)

### 概述

能力矩阵提供细粒度的后端能力查询，包括算法支持、功能成熟度、性能特性等。

### TSSLBackendCapabilities

后端能力信息记录。

```pascal
type
  TSSLBackendCapabilities = record
    // v1.1.0 字段（向后兼容）
    SupportsTLS13: Boolean;
    SupportsALPN: Boolean;
    SupportsSNI: Boolean;
    SupportsOCSPStapling: Boolean;
    SupportsCertificateTransparency: Boolean;
    SupportsSessionTickets: Boolean;
    SupportsECDHE: Boolean;
    SupportsChaChaPoly: Boolean;
    SupportsPEMPrivateKey: Boolean;
    MinTLSVersion: TSSLProtocolVersion;
    MaxTLSVersion: TSSLProtocolVersion;

    // v1.2.0 新增字段
    BackendType: TSSLLibraryType;
    BackendImplType: TSSLBackendImplType;
    BackendVersion: string;
    SupportsDTLS: Boolean;

    // 功能支持级别
    SNISupport: TSSLFeatureSupportLevel;
    ALPNSupport: TSSLFeatureSupportLevel;
    OCSPStaplingSupport: TSSLFeatureSupportLevel;
    CertTransparencySupport: TSSLFeatureSupportLevel;
    SessionTicketsSupport: TSSLFeatureSupportLevel;
    SessionCacheSupport: TSSLFeatureSupportLevel;
    // ... 其他功能级别字段

    // 算法支持
    SupportedCiphers: TSSLCipherSupport;
    SupportedHashes: TSSLHashSupport;
    SupportedKeyExchanges: TSSLKeyExchangeSupport;

    // 性能特性
    HasHardwareAcceleration: Boolean;
    HasSIMDOptimization: Boolean;
    HasAssemblyOptimization: Boolean;

    // 平台特性
    RequiresExternalLibrary: Boolean;
    SupportsSystemCertStore: Boolean;
    SupportsPKCS11: Boolean;
    SupportsTPM: Boolean;

    // 安全特性
    HasConstantTimeOperations: Boolean;
    SupportsFIPSMode: Boolean;
    HasSecureMemoryWipe: Boolean;

    // 兼容性
    CompatibilityLevel: Integer;  // 0-100
    KnownIssues: string;
  end;
```

读取优先级说明：

- 当 `SNISupport` / `ALPNSupport` / `OCSPStaplingSupport` / `CertTransparencySupport` / `SessionTicketsSupport` / `SessionCacheSupport` 出现时，它们是当前 source/runtime truth；legacy `SupportsSNI` / `SupportsALPN` / `SupportsOCSPStapling` / `SupportsCertificateTransparency` / `SupportsSessionTickets` 仅作为兼容投影。
- `SessionCacheSupport` 表示 context-scoped session cache/control surface 的 published support level；对 WinSSL 而言，它不等于当前已 runtime-proven 的 resumed handshake 结果。
- `SupportsTLS13` 仍是主 bool 字段，因为当前没有 `TLS13Support`。

### ISSLLibrary.GetCapabilities

获取后端能力矩阵。

```pascal
function GetCapabilities: TSSLBackendCapabilities;
```

**返回值**: 包含后端所有能力信息的记录。

**使用示例**:

```pascal
var
  Lib: ISSLLibrary;
  Caps: TSSLBackendCapabilities;
begin
  Lib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
  Caps := Lib.GetCapabilities;

  WriteLn('Backend: ', SSL_LIBRARY_NAMES[Caps.BackendType]);
  WriteLn('Version: ', Caps.BackendVersion);
  WriteLn('TLS 1.3: ', Caps.SupportsTLS13);
end;
```

---

### 算法查询函数

#### IsCipherSupported

检查是否支持指定的对称加密算法。

```pascal
function IsCipherSupported(const ACaps: TSSLBackendCapabilities;
                          ACipher: TSSLCipher): Boolean;
```

**参数**:

- `ACaps`: 后端能力矩阵
- `ACipher`: 要查询的加密算法

**返回值**: 如果支持返回 `True`，否则返回 `False`。

**使用示例**:

```pascal
if IsCipherSupported(Caps, sslCipherCHACHA20_POLY1305) then
  WriteLn('ChaCha20-Poly1305 is supported');
```

#### IsHashSupported

检查是否支持指定的哈希算法。

```pascal
function IsHashSupported(const ACaps: TSSLBackendCapabilities;
                        AHash: TSSLHash): Boolean;
```

**参数**:

- `ACaps`: 后端能力矩阵
- `AHash`: 要查询的哈希算法

**返回值**: 如果支持返回 `True`，否则返回 `False`。

**使用示例**:

```pascal
if IsHashSupported(Caps, sslHashSHA256) then
  WriteLn('SHA-256 is supported');
```

#### IsKeyExchangeSupported

检查是否支持指定的密钥交换算法。

```pascal
function IsKeyExchangeSupported(const ACaps: TSSLBackendCapabilities;
                               AKex: TSSLKeyExchange): Boolean;
```

**参数**:

- `ACaps`: 后端能力矩阵
- `AKex`: 要查询的密钥交换算法

**返回值**: 如果支持返回 `True`，否则返回 `False`。

**使用示例**:

```pascal
if IsKeyExchangeSupported(Caps, sslKexECDHE_RSA) then
  WriteLn('ECDHE-RSA is supported');
```

---

### 功能级别查询函数

#### IsFeatureStable

检查功能是否稳定（推荐生产使用）。

```pascal
function IsFeatureStable(ASupport: TSSLFeatureSupportLevel): Boolean;
```

**参数**:

- `ASupport`: 功能支持级别

**返回值**: 如果功能稳定返回 `True`。

**使用示例**:

```pascal
if IsFeatureStable(Caps.ALPNSupport) then
  WriteLn('ALPN is production-ready');
```

#### IsFeatureUsable

检查功能是否可用（包括实验性功能）。

```pascal
function IsFeatureUsable(ASupport: TSSLFeatureSupportLevel): Boolean;
```

**参数**:

- `ASupport`: 功能支持级别

**返回值**: 如果功能可用返回 `True`（包括实验性和稳定）。

#### IsFeatureDeprecated

检查功能是否已弃用。

```pascal
function IsFeatureDeprecated(ASupport: TSSLFeatureSupportLevel): Boolean;
```

**参数**:

- `ASupport`: 功能支持级别

**返回值**: 如果功能已弃用返回 `True`。

---

### 后端类型查询函数

#### IsNativeBackend

检查是否为纯 FreePascal 实现的后端。

```pascal
function IsNativeBackend(const ACaps: TSSLBackendCapabilities): Boolean;
```

**参数**:

- `ACaps`: 后端能力矩阵

**返回值**: 如果是纯 Pascal 实现返回 `True`。

#### IsCLibraryBackend

检查是否为 C 库绑定后端。

```pascal
function IsCLibraryBackend(const ACaps: TSSLBackendCapabilities): Boolean;
```

**参数**:

- `ACaps`: 后端能力矩阵

**返回值**: 如果是 C 库绑定返回 `True`。

#### RequiresExternalDependencies

检查是否需要外部依赖（库文件）。

```pascal
function RequiresExternalDependencies(const ACaps: TSSLBackendCapabilities): Boolean;
```

**参数**:

- `ACaps`: 后端能力矩阵

**返回值**: 如果需要外部库返回 `True`。

---

### 评分函数

#### GetSecurityScore

获取后端的安全评分（0-100）。

```pascal
function GetSecurityScore(const ACaps: TSSLBackendCapabilities): Integer;
```

**参数**:

- `ACaps`: 后端能力矩阵

**返回值**: 安全评分（0-100），分数越高越安全。

**评分因素**:

- TLS 1.3 支持 (+20)
- 恒定时间操作 (+20)
- FIPS 模式 (+15)
- 安全内存擦除 (+15)
- 现代算法支持 (+30)

**使用示例**:

```pascal
var
  Score: Integer;
begin
  Score := GetSecurityScore(Caps);
  WriteLn('Security Score: ', Score, '/100');

  if Score >= 90 then
    WriteLn('Excellent security')
  else if Score >= 70 then
    WriteLn('Good security')
  else
    WriteLn('Consider using a more secure backend');
end;
```

#### GetPerformanceScore

获取后端的性能评分（0-100）。

```pascal
function GetPerformanceScore(const ACaps: TSSLBackendCapabilities): Integer;
```

**参数**:

- `ACaps`: 后端能力矩阵

**返回值**: 性能评分（0-100），分数越高性能越好。

**评分因素**:

- 硬件加速 (+40)
- SIMD 优化 (+30)
- 汇编优化 (+30)

**使用示例**:

```pascal
var
  Score: Integer;
begin
  Score := GetPerformanceScore(Caps);
  WriteLn('Performance Score: ', Score, '/100');

  if Score >= 90 then
    WriteLn('High performance backend')
  else if Score >= 70 then
    WriteLn('Good performance')
  else
    WriteLn('Consider performance optimizations');
end;
```

---

### 描述生成函数

#### GetCapabilitiesDescription

生成后端能力的完整文本描述。

```pascal
function GetCapabilitiesDescription(const ACaps: TSSLBackendCapabilities): string;
```

**参数**:

- `ACaps`: 后端能力矩阵

**返回值**: 包含后端所有能力信息的多行文本。

**使用示例**:

```pascal
var
  Desc: string;
begin
  Desc := GetCapabilitiesDescription(Caps);
  WriteLn(Desc);

  // 输出示例:
  // Backend: OpenSSL
  // Version: OpenSSL 3.5.4 30 Sep 2025
  // Implementation: C Library Binding
  // TLS Versions: TLS 1.0 - TLS 1.3
  // DTLS: Supported
  // Dependencies: External library required
  // Platform Features:
  //   - PKCS#11 hardware tokens
  // Security Score: 90/100
  // Performance Score: 100/100
end;
```

---

### 完整使用示例

```pascal
program capability_example;

uses
  SysUtils, fafafa.ssl.base, fafafa.ssl.factory;

procedure PrintBackendInfo(ABackend: TSSLLibraryType);
var
  Lib: ISSLLibrary;
  Caps: TSSLBackendCapabilities;
begin
  try
    Lib := TSSLFactory.GetLibraryInstance(ABackend);
    if not Assigned(Lib) then
    begin
      WriteLn('Backend not available: ', SSL_LIBRARY_NAMES[ABackend]);
      Exit;
    end;

    Caps := Lib.GetCapabilities;

    WriteLn('========================================');
    WriteLn('Backend: ', SSL_LIBRARY_NAMES[Caps.BackendType]);
    WriteLn('========================================');
    WriteLn;

    // 基础信息
    WriteLn('Version: ', Caps.BackendVersion);
    WriteLn('Implementation Type: ', Ord(Caps.BackendImplType));
    WriteLn;

    // 协议支持
    WriteLn('TLS Support:');
    WriteLn('  TLS 1.3: ', Caps.SupportsTLS13);
    WriteLn('  DTLS: ', Caps.SupportsDTLS);
    WriteLn('  Min Version: TLS ', Ord(Caps.MinTLSVersion) - Ord(sslProtocolTLS10) + 1, '.0');
    WriteLn('  Max Version: TLS ', Ord(Caps.MaxTLSVersion) - Ord(sslProtocolTLS10) + 1, '.0');
    WriteLn;

    // 算法支持
    WriteLn('Algorithm Support:');
    WriteLn('  AES-256-GCM: ', IsCipherSupported(Caps, sslCipherAES256GCM));
    WriteLn('  ChaCha20-Poly1305: ', IsCipherSupported(Caps, sslCipherCHACHA20_POLY1305));
    WriteLn('  SHA-256: ', IsHashSupported(Caps, sslHashSHA256));
    WriteLn('  ECDHE-RSA: ', IsKeyExchangeSupported(Caps, sslKexECDHE_RSA));
    WriteLn;

    // 功能成熟度
    WriteLn('Feature Maturity:');
    WriteLn('  ALPN: ', IsFeatureStable(Caps.ALPNSupport));
    WriteLn('  SNI: ', IsFeatureStable(Caps.SNISupport));
    WriteLn;

    // 性能和安全
    WriteLn('Performance & Security:');
    WriteLn('  Security Score: ', GetSecurityScore(Caps), '/100');
    WriteLn('  Performance Score: ', GetPerformanceScore(Caps), '/100');
    WriteLn('  Hardware Acceleration: ', Caps.HasHardwareAcceleration);
    WriteLn('  SIMD Optimization: ', Caps.HasSIMDOptimization);
    WriteLn;

    // 平台特性
    WriteLn('Platform Features:');
    WriteLn('  External Library Required: ', Caps.RequiresExternalLibrary);
    WriteLn('  System Certificate Store: ', Caps.SupportsSystemCertStore);
    WriteLn('  PKCS#11: ', Caps.SupportsPKCS11);
    WriteLn('  TPM: ', Caps.SupportsTPM);
    WriteLn;

  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end;

begin
  PrintBackendInfo(sslOpenSSL);
  PrintBackendInfo(sslWolfSSL);
  PrintBackendInfo(sslMbedTLS);
  PrintBackendInfo(sslWinSSL);
end.
```

---

## 参考资源

- **OpenSSL 文档**: https://www.openssl.org/docs/
- **Windows Schannel**: https://docs.microsoft.com/en-us/windows/win32/secauthn/schannel
- **RFC 5280** (X.509): https://tools.ietf.org/html/rfc5280
- **RFC 8446** (TLS 1.3): https://tools.ietf.org/html/rfc8446
- **能力矩阵指南**: `docs/CAPABILITY_MATRIX_GUIDE.md` - 详细使用指南

---

**版本历史**:

- v1.2 (2026-02-05): 添加能力矩阵 API（v1.2.0）
- v0.8 (2025-10-24): 添加 VerifyEx 方法和 WinSSL 企业功能
- v0.7 (2025-10-01): 初始 API 文档
