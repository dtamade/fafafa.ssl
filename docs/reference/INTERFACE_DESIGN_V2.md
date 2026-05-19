# ISSLConnection 接口设计 v2.0

## 设计目标

1. **最小化核心接口** - 便于第三方框架集成
2. **分层扩展** - 高级功能通过扩展接口提供
3. **清晰的错误状态** - 支持非阻塞模式集成

> 这份文档描述的是 **v2 最小 core 目标**，不是 `v1.5.0` 当前 shipped source 的逐行镜像。
> 当前 shipped source truth 以 `src/fafafa.ssl.base.pas` 与 `docs/reference/API_REFERENCE.md` 为准；
> `ReadString` / `WriteString` / timeout / blocking 这组方法在 `v1.x` 仍保留为 convenience-core / connection-adjacent surface。

---

## 接口层次结构

```
ISSLConnection (v2 目标核心 - 17 个方法)
├── ISSLClientConnection (客户端扩展 - SNI)
├── ISSLNativeHandleAccess (原生句柄访问)
├── ISSLConnectionInfo (连接信息 mirrors)
├── ISSLDiagnostics (诊断扩展)
├── ISSLSessionResumption (会话扩展)
├── ISSLCertificateVerification (证书验证扩展)
└── ISSLOCSPStapling (OCSP 扩展)
```

> 注：当前 public Pascal source 尚未声明 `ISSLServerConnection`。
> 现阶段服务端特有能力主要通过 `ISSLContext` 的可选扩展接口暴露，
> 例如 `ISSLServerOCSPStaplingContext` 和 early-data 相关 server-side context surface。

---

## 核心接口 ISSLConnection

仅包含框架集成必需的方法：

```pascal
ISSLConnection = interface
  ['{...}']

  // === 连接生命周期 (4) ===
  function Connect: Boolean;           // 客户端连接
  function Accept: Boolean;            // 服务端接受
  function Shutdown: Boolean;          // 优雅关闭
  procedure Close;                     // 强制关闭

  // === 握手控制 (3) ===
  function DoHandshake: TSSLHandshakeState;  // 非阻塞握手
  function IsHandshakeComplete: Boolean;
  function Renegotiate: Boolean;

  // === 数据传输 (2) ===
  function Read(var ABuffer; ACount: Integer): Integer;
  function Write(const ABuffer; ACount: Integer): Integer;

  // === 非阻塞状态 (3) ===
  function WantRead: Boolean;
  function WantWrite: Boolean;
  function GetError(ARet: Integer): TSSLErrorCode;

  // === 连接状态 (2) ===
  function IsConnected: Boolean;
  function GetState: string;

  // === 协商结果 (3) ===
  function GetProtocolVersion: TSSLProtocolVersion;
  function GetCipherName: string;
  function GetPeerCertificate: ISSLCertificate;
end;
```

**总计: 17 个方法**（从原来的 ~50 个精简）

---

## 扩展接口

### ISSLClientConnection (客户端特有)

```pascal
ISSLClientConnection = interface(ISSLConnection)
  procedure SetServerName(const AServerName: string);  // SNI
  function GetServerName: string;
end;
```

### ISSLNativeHandleAccess (原生句柄访问)

```pascal
ISSLNativeHandleAccess = interface
  function GetNativeHandle: Pointer;
  function GetBackendType: TSSLLibraryType;
  function IsNativeHandleValid: Boolean;
end;
```

### ISSLDiagnostics (诊断功能)

```pascal
ISSLDiagnostics = interface
  function GetHealthStatus: TSSLHealthStatus;
  function GetPerformanceMetrics: TSSLPerformanceMetrics;
  function GetDiagnosticInfo: TSSLDiagnosticInfo;
  function IsHealthy: Boolean;
end;
```

### ISSLConnectionInfo (连接信息 mirrors)

```pascal
ISSLConnectionInfo = interface
  function GetConnectionInfo: TSSLConnectionInfo;
  function GetContext: ISSLContext;
  function GetSelectedALPNProtocol: string;
  function GetStateString: string;
end;
```

### ISSLSessionResumption (会话复用)

```pascal
ISSLSessionResumption = interface
  function GetSession: ISSLSession;
  procedure SetSession(ASession: ISSLSession);
  function IsSessionReused: Boolean;
end;
```

### ISSLCertificateVerification (证书验证详情)

```pascal
ISSLCertificateVerification = interface
  function GetPeerCertificateChain: TSSLCertificateArray;
  function GetVerifyResult: Integer;
  function GetVerifyResultString: string;
end;
```

### ISSLOCSPStapling (OCSP 装订)

```pascal
ISSLOCSPStapling = interface
  function GetOCSPStaplingEnabled: Boolean;
  function GetOCSPResponse: TBytes;
  function IsOCSPResponseVerified: Boolean;
  function GetOCSPResponseStatus: string;
end;
```

---

## 迁移策略

### 向后兼容

```pascal
// 旧代码仍然有效
var LConn: ISSLConnection;
LConn.GetConnectionInfo;  // 仅兼容保留，源码声明已是编译期 deprecated

// Stage A demotion target
var LInfoExt: ISSLConnectionInfo;
if Supports(LConn, ISSLConnectionInfo, LInfoExt) then
  LInfoExt.GetConnectionInfo;

// diagnostics 仍走自己的扩展接口
var LDiag: ISSLDiagnostics;
if Supports(LConn, ISSLDiagnostics, LDiag) then
  LDiag.GetHealthStatus;
```

### 实现类

```pascal
TBaseSSLConnection = class(TInterfacedObject,
  ISSLConnection,
  ISSLClientConnection,
  ISSLConnectionInfo,
  ISSLDiagnostics,
  ISSLSessionResumption,
  ISSLCertificateVerification,
  ISSLOCSPStapling)
```

---

## 框架集成示例

### 最小集成（只需 ISSLConnection）

```pascal
type
  TMySSLSocket = class
  private
    FSSLConn: ISSLConnection;
  public
    function DoRead(var Buf; Count: Integer): Integer;
    function DoWrite(const Buf; Count: Integer): Integer;
  end;

function TMySSLSocket.DoRead(var Buf; Count: Integer): Integer;
begin
  Result := FSSLConn.Read(Buf, Count);
  if Result < 0 then
  begin
    case FSSLConn.GetError(Result) of
      sslErrWantRead: Result := 0;   // 稍后重试
      sslErrWantWrite: Result := 0;  // 等待可写
      else raise Exception.Create('SSL error');
    end;
  end;
end;
```

### 高级集成（按需使用扩展接口）

```pascal
// 只有需要诊断时才获取
var LDiag: ISSLDiagnostics;
if Supports(FSSLConn, ISSLDiagnostics, LDiag) then
  LogMetrics(LDiag.GetPerformanceMetrics);

// 只有需要会话复用时才获取
var LSession: ISSLSessionResumption;
if Supports(FSSLConn, ISSLSessionResumption, LSession) then
  SaveSession(LSession.GetSession);
```

---

## 方法迁移对照表

| 原方法 | 新位置 | 说明 |
|--------|--------|------|
| Connect, Accept, Shutdown, Close | ISSLConnection | 保留 |
| Read, Write | ISSLConnection | 保留 |
| DoHandshake, IsHandshakeComplete | ISSLConnection | 保留 |
| WantRead, WantWrite, GetError | ISSLConnection | 保留 |
| IsConnected, GetState | ISSLConnection | 保留 |
| GetProtocolVersion, GetCipherName | ISSLConnection | 保留 |
| GetPeerCertificate | ISSLConnection | 保留 |
| GetNativeHandle | ISSLNativeHandleAccess | 不属于核心 ISSLConnection；通过可选 native-handle 接口访问 |
| ReadString, WriteString | ISSLConnection | `v1.x` convenience-core 文本 helper；框架/transport 集成优先使用 `Read` / `Write` |
| GetConnectionInfo | ISSLConnectionInfo | 默认 owner 已切到 ISSLConnectionInfo；core 侧仅兼容保留，源码声明已是编译期 deprecated |
| GetStateString | ISSLConnectionInfo | 默认 owner 已切到 ISSLConnectionInfo；core 侧仅兼容保留，源码声明已是编译期 deprecated |
| SetTimeout, GetTimeout | ISSLConnection | `v1.x` connection-adjacent convenience surface；builder-first，连接侧保留 override |
| SetBlocking, GetBlocking | ISSLConnection | `v1.x` connection-adjacent convenience surface；builder-first，连接侧保留 override |
| GetContext | ISSLConnectionInfo | 默认 owner 已切到 ISSLConnectionInfo；core 侧仅兼容保留，源码声明已是编译期 deprecated |
| SetServerName, GetServerName | ISSLClientConnection | 客户端特有 |
| GetSelectedALPNProtocol | ISSLConnectionInfo | 默认 owner 已切到 ISSLConnectionInfo；core 侧仅兼容保留，源码声明已是编译期 deprecated |
| GetHealthStatus, IsHealthy | ISSLDiagnostics | 诊断扩展 |
| GetPerformanceMetrics | ISSLDiagnostics | 诊断扩展 |
| GetDiagnosticInfo | ISSLDiagnostics | 诊断扩展 |
| GetSession, SetSession | ISSLSessionResumption | 会话扩展 |
| IsSessionReused | ISSLSessionResumption | 会话扩展 |
| GetPeerCertificateChain | ISSLCertificateVerification | 证书扩展 |
| GetVerifyResult, GetVerifyResultString | ISSLCertificateVerification | 默认 owner 已切到 ISSLCertificateVerification；core 侧仅兼容保留，源码声明已是编译期 deprecated |
| GetOCSP* | ISSLOCSPStapling | OCSP 扩展 |

---

## 实施计划

1. **Phase 1**: 创建新的扩展接口定义
2. **Phase 2**: 更新 TBaseSSLConnection 实现所有接口
3. **Phase 3**: 更新所有后端实现
4. **Phase 4**: 更新测试和文档
5. **Phase 5**: 标记旧方法为 deprecated（保持兼容）

---

## Stage-A Note

当前 `v1.x` source truth 里，`GetConnectionInfo` / `GetContext` / `GetSelectedALPNProtocol` / `GetStateString`
既存在于核心 `ISSLConnection`，也存在于 `ISSLConnectionInfo`。

这份 v2 设计文档当前只冻结 **第一步 demotion 路线**：

1. 先把这 4 个 mirrors 的默认 owner 统一成 `ISSLConnectionInfo`
2. 只在这一步稳定后，再决定：
   - `GetSelectedALPNProtocol` 是否进一步收窄到 `ISSLClientConnection`
   - `GetStateString` 是否并入 `GetState`
   - `GetContext` 是否最终彻底退出 public surface

其中 `GetSelectedALPNProtocol` 在核心 `ISSLConnection` 上当前也只保留为 compatibility mirror，源码声明已经进入编译期 `deprecated`；后续仍可再评估是否进一步收窄到 `ISSLClientConnection`。

其中 `GetStateString` 在核心 `ISSLConnection` 上当前也只保留为 compatibility mirror，源码声明已经进入编译期 `deprecated`；后续仍可再评估是否并入 `GetState`。

其中 `GetContext` 在核心 `ISSLConnection` 上当前也只保留为 compatibility mirror，源码声明已经进入编译期 `deprecated`。

换句话说，`GetConnectionInfo` 在 `ISSLConnection` core 上虽然仍存在，但这里只把它视为 compatibility mirror，不再把它当作新代码默认入口；当前源码声明也已经进入编译期 `deprecated`。

其中 `GetVerifyResult` / `GetVerifyResultString` 在核心 `ISSLConnection` 上当前也只保留为 compatibility mirror，源码声明已经进入编译期 `deprecated`；后续仍可再评估是否把这组结果 surface 进一步完全收窄到 `ISSLCertificateVerification`。

也就是说，这一版文档不再提前把 Stage-B/Stage-C 的选择写死。

*设计版本: 2.0*
*创建日期: 2026-02-05*
