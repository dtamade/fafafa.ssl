# ISSLConnection 接口设计 v2.0

## 设计目标

1. **最小化核心接口** - 便于第三方框架集成
2. **分层扩展** - 高级功能通过扩展接口提供
3. **清晰的错误状态** - 支持非阻塞模式集成

---

## 接口层次结构

```
ISSLConnection (核心 - 18 个方法)
├── ISSLClientConnection (客户端扩展 - SNI)
├── ISSLServerConnection (服务端扩展 - 未来)
├── ISSLDiagnostics (诊断扩展)
└── ISSLAdvanced (高级功能扩展)
```

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

  // === 原生访问 (1) ===
  function GetNativeHandle: Pointer;
end;
```

**总计: 18 个方法**（从原来的 ~50 个精简）

---

## 扩展接口

### ISSLClientConnection (客户端特有)

```pascal
ISSLClientConnection = interface(ISSLConnection)
  procedure SetServerName(const AServerName: string);  // SNI
  function GetServerName: string;
  function GetSelectedALPNProtocol: string;
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
LConn.GetConnectionInfo;  // 仍然存在

// 新代码推荐
var LDiag: ISSLDiagnostics;
if Supports(LConn, ISSLDiagnostics, LDiag) then
  LDiag.GetHealthStatus;
```

### 实现类

```pascal
TBaseSSLConnection = class(TInterfacedObject,
  ISSLConnection,
  ISSLClientConnection,
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
| GetNativeHandle | ISSLConnection | 保留 |
| ReadString, WriteString | **移除** | 使用 Read/Write |
| GetConnectionInfo | **移除** | 使用 ISSLDiagnostics |
| GetStateString | **移除** | 合并到 GetState |
| SetTimeout, GetTimeout | **移除** | 由外部框架控制 |
| SetBlocking, GetBlocking | **移除** | 由外部框架控制 |
| GetContext | **移除** | 通常不需要 |
| SetServerName, GetServerName | ISSLClientConnection | 客户端特有 |
| GetSelectedALPNProtocol | ISSLClientConnection | 客户端特有 |
| GetHealthStatus, IsHealthy | ISSLDiagnostics | 诊断扩展 |
| GetPerformanceMetrics | ISSLDiagnostics | 诊断扩展 |
| GetDiagnosticInfo | ISSLDiagnostics | 诊断扩展 |
| GetSession, SetSession | ISSLSessionResumption | 会话扩展 |
| IsSessionReused | ISSLSessionResumption | 会话扩展 |
| GetPeerCertificateChain | ISSLCertificateVerification | 证书扩展 |
| GetVerifyResult, GetVerifyResultString | ISSLCertificateVerification | 证书扩展 |
| GetOCSP* | ISSLOCSPStapling | OCSP 扩展 |

---

## 实施计划

1. **Phase 1**: 创建新的扩展接口定义
2. **Phase 2**: 更新 TBaseSSLConnection 实现所有接口
3. **Phase 3**: 更新所有后端实现
4. **Phase 4**: 更新测试和文档
5. **Phase 5**: 标记旧方法为 deprecated（保持兼容）

---

*设计版本: 2.0*
*创建日期: 2026-02-05*
