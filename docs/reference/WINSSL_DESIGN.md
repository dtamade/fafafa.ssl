# Windows SSL (Schannel) 后端设计

**状态**: Windows runtime partially proven（公共 handshake / gate 路径已有 GitHub Windows runner 证据；native session-info probe 仍留在 opt-in 隔离调查 lane）
**最后更新**: 2026-05-19

## 1. 概述

Windows SSL 后端基于 Windows 内置的 Schannel (Security Channel) API 实现，提供原生的 SSL/TLS 支持。

**当前证据分层**:

- **已确认的源码与编译面**:
  - `src/fafafa.ssl.winssl.connection.pas` 是当前 canonical connection truth source
  - `src/fafafa.ssl.winssl.session.pas` 只保留兼容 shim
  - Linux 侧 source contract 已锁住 session/native-handle truth 和 context/library access seam
  - 选定 WinSSL 用例与 `backend_comparison` 路径已能继续做 Win64 交叉编译
- **已确认的仓库门禁**:
  - `python3 scripts/compile_all_modules.py`：`185/185`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`：`[PASS]`
- **已确认的 Windows runtime truth**:
  - GitHub Windows runner 已证明常规 WinSSL gate / broader suite 可以跑通到 public truth 输出
  - dedicated session-resumption bridge truth 当前固定为：
    - `observed_reuse=false`
    - `session_configured=true`
  - opt-in native probe 仍只允许留在 isolated worker / experimental evidence lane
- **仍未确认的区域**:
  - fafafa.ssl 是否已经在 Windows 主机上真实观测到 resumed handshake
  - `SECPKG_ATTR_SESSION_INFO` 是否存在安全、可复用、可共享的 production seam
  - 当前 Linux 主机不能把 `wine` 或交叉编译结果当成 runtime proof

这意味着 WinSSL 的**代码结构、compile surface 与一部分 Windows runtime truth 已持续收口**，但本文档不再把它表述成“session resumption 已在 fafafa.ssl 中完整 runtime-proven”。

## 2. 优势

### 2.1 系统集成

- **无额外依赖**：使用 Windows 内置 API，无需分发额外的 DLL
- **自动更新**：通过 Windows Update 自动获取安全补丁
- **证书存储**：直接访问 Windows 证书存储
- **企业策略**：自动遵循企业 SSL/TLS 策略配置

### 2.2 性能优势

- **原生优化**：针对 Windows 平台优化的实现
- **硬件加速**：自动利用系统可用的加密硬件
- **内存效率**：与系统内存管理器集成

### 2.3 兼容性

- **协议支持**：支持 Windows 系统支持的所有 TLS 版本
- **密码套件**：使用系统配置的密码套件
- **FIPS policy 检测**：可检测 Windows 是否启用 FIPS policy，但这不是当前公开 `SupportsFIPSMode` capability

## 3. 技术实现

### 3.1 核心 Windows API

- **SSPI (Security Support Provider Interface)**：主要接口
- **Schannel.dll**：SSL/TLS 实现
- **Crypt32.dll**：证书处理
- **Secur32.dll**：安全上下文管理

### 3.2 关键结构和函数

```pascal
// 主要 Windows API 函数
AcquireCredentialsHandle()     // 获取凭据句柄
InitializeSecurityContext()   // 初始化安全上下文
AcceptSecurityContext()       // 接受安全上下文 (服务端)
QueryContextAttributes()      // 查询上下文属性
EncryptMessage()              // 加密数据
DecryptMessage()              // 解密数据
FreeCredentialsHandle()       // 释放凭据句柄
DeleteSecurityContext()       // 删除安全上下文
```

### 3.3 数据结构映射

```pascal
type
  // Schannel 特定配置
  TSchannelConfig = record
    EnabledProtocols: DWORD;           // 启用的协议版本
    CertContext: PCCERT_CONTEXT;       // 证书上下文
    CertStore: HCERTSTORE;             // 证书存储句柄
    ClientAuthMode: DWORD;             // 客户端认证模式
    CipherSuites: array of ALG_ID;     // 指定的密码套件
  end;

  // 安全上下文包装
  TWinSSLContext = class(TInterfacedObject, ISSLContext, ISSLNativeHandleAccess,
    IWinSSLContextAccess)
  private
    FCredHandle: CredHandle;           // 凭据句柄
    FCtxtHandle: CtxtHandle;           // 上下文句柄
    FConfig: TSchannelConfig;          // 配置信息
    FServerName: string;               // 目标服务器名称
  end;
```

说明:

- `ISSLNativeHandleAccess` 是 public optional interface
- `IWinSSLContextAccess` 是 WinSSL 内部 seam，只用于 connection 与 context 协作，不扩张 public `ISSLContext`
- `SECPKG_ATTR_CONNECTION_INFO`
  - 提供的是算法级字段，如 `aiCipher` / `aiHash` / `dwCipherStrength`
  - 适合回填 `Cipher` / `Hash` / `KeySize` 这类算法 detail
- `SECPKG_ATTR_CIPHER_INFO`
  - 才是 WinSSL 侧真实 cipher-suite id/name 的首选 truth source
  - `CipherSuiteId` 不应继续从 `ConnectionInfo.aiCipher` 这种算法 ID 直接推回
  - `MacSize` 也不应无条件把 `dwHashStrength` 当作统一 truth；当前应先吃共享层基于 suite name 的 AEAD tag-length 推导，只在缺值时再回退到 `dwHashStrength div 8`

## 4. 实现细节

### 4.1 握手流程

1. **凭据获取**：调用 `AcquireCredentialsHandle` 获取凭据
2. **上下文初始化**：客户端调用 `InitializeSecurityContext`
3. **握手数据交换**：处理握手数据包
4. **上下文完成**：握手完成，建立安全连接

### 4.2 数据传输

```pascal
// 加密发送数据
function EncryptData(const AData: TBytes): TBytes;
begin
  // 准备安全缓冲区
  SetLength(LSecBuffers, 4);
  // SECBUFFER_STREAM_HEADER
  // SECBUFFER_DATA
  // SECBUFFER_STREAM_TRAILER
  // SECBUFFER_EMPTY

  // 调用 EncryptMessage
  LStatus := EncryptMessage(@FCtxtHandle, 0, @LSecBufferDesc, 0);
  // 处理结果...
end;

// 解密接收数据
function DecryptData(const AData: TBytes): TBytes;
begin
  // 准备安全缓冲区
  // 调用 DecryptMessage
  LStatus := DecryptMessage(@FCtxtHandle, @LSecBufferDesc, 0, nil);
  // 处理结果...
end;
```

### 4.3 证书处理

```pascal
// 加载证书
function LoadCertificate(const ACertPath: string): PCCERT_CONTEXT;
var
  LStore: HCERTSTORE;
  LCert: PCCERT_CONTEXT;
begin
  // 打开证书存储
  LStore := CertOpenStore(CERT_STORE_PROV_SYSTEM, 0, 0,
    CERT_SYSTEM_STORE_CURRENT_USER, 'MY');

  // 查找证书
  LCert := CertFindCertificateInStore(LStore, X509_ASN_ENCODING, 0,
    CERT_FIND_SUBJECT_STR, PWideChar(ACertPath), nil);

  CertCloseStore(LStore, 0);
  Result := LCert;
end;
```

### 4.4 Session 管理架构

WinSSL 后端的 Session 复用依赖 Windows Schannel 的凭据句柄缓存机制。当前真实实现已收敛到 `src/fafafa.ssl.winssl.connection.pas`；`src/fafafa.ssl.winssl.session.pas` 只保留兼容 shim，不再维护平行实现。

这里的 session 流程描述反映的是**当前源码结构和设计意图**，不是 Linux 主机上已经拿到的 Windows runtime proof。

#### 4.4.1 核心组件

```pascal
// Session 接口定义
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

// Canonical WinSSL session implementation
TWinSSLSession = class(TInterfacedObject, ISSLSession)
private
  FID: string;
  FCreationTime: TDateTime;
  FTimeout: Integer;
  FProtocolVersion: TSSLProtocolVersion;
  FCipherName: string;
  FSessionData: TBytes;
  FPeerCertificate: ISSLCertificate;
  FIsResumed: Boolean;
public
  constructor Create;
  procedure SetSessionMetadata(const AID: string;
    AProtocol: TSSLProtocolVersion; const ACipher: string; AResumed: Boolean);
  function WasResumed: Boolean;
end;

// Session 缓存管理器
TWinSSLSessionManager = class
private
  FSessions: TStringList;
  FLock: TCriticalSection;
  FMaxSessions: Integer;
public
  constructor Create;
  destructor Destroy; override;

  procedure AddSession(const AID: string; ASession: ISSLSession);
  function GetSession(const AID: string): ISSLSession;
  procedure RemoveSession(const AID: string);
  procedure CleanupExpired;
  procedure SetMaxSessions(AMax: Integer);
end;
```

当前边界：

- `TWinSSLSession` 不暴露 `ISSLNativeHandleAccess`
- Schannel session 仍由系统管理，不提供稳定的独立原生 session 句柄
- Windows runtime proof 必须以 Windows 主机/GitHub Windows runner 为准

#### 4.4.2 Session 复用流程

**第一次连接（完整握手）**:

```
1. 客户端调用 `AcquireCredentialsHandle` 获取凭据句柄
2. 调用 `InitializeSecurityContext` 开始握手
3. 与服务器交换握手数据包（多次往返）
4. 握手完成，建立安全连接
5. 通过 canonical connection path 记录保守的 session metadata / compatibility surface
6. 若调用方需要 `ISSLSessionResumption`，则返回当前 public session object
7. dedicated runtime truth 继续以 `IsSessionReused` / runtime transcript 为准，而不是在 shared path 里直接做 native session-info query
```

**后续连接（Session 复用）**:

```
1. 调用方可通过 `ISSLSessionResumption.SetSession(...)` 传回 compatibility metadata
2. 连接继续复用相同 target name / credential handle
3. Schannel 可能尝试命中系统侧 session cache / ticket
4. public truth 仍以当前握手后的 `IsSessionReused` 和 Windows transcript 为准
5. 现有 dedicated Windows CI truth 仍可能是 `observed_reuse=false` / `session_configured=true`
```

#### 4.4.3 Schannel 凭据句柄缓存机制

Windows Schannel 通过凭据句柄（CredHandle）实现 Session 缓存：

```pascal
// 凭据句柄结构
type
  TCredentialCache = record
    CredHandle: CredHandle;           // 凭据句柄
    ServerName: string;               // 目标服务器名称
    CachedSessions: TList;            // 缓存的 Session 列表
    LastUsed: TDateTime;              // 最后使用时间
  end;

// 凭据句柄管理
function GetOrCreateCredHandle(const AServerName: string): CredHandle;
begin
  // 1. 检查是否已有该服务器的凭据句柄
  if FCredHandleCache.ContainsKey(AServerName) then
  begin
    Result := FCredHandleCache[AServerName].CredHandle;
    FCredHandleCache[AServerName].LastUsed := Now;
    Exit;
  end;

  // 2. 创建新的凭据句柄
  LStatus := AcquireCredentialsHandle(
    nil,                              // 主体名称
    UNISP_NAME,                       // 包名称（Schannel）
    SECPKG_CRED_OUTBOUND,            // 凭据使用方式
    nil,                              // LUID
    @FSchannelCred,                   // 认证数据
    nil,                              // GetKey 函数
    nil,                              // GetKey 参数
    @Result,                          // 凭据句柄输出
    @LExpiry                          // 过期时间
  );

  // 3. 缓存凭据句柄
  FCredHandleCache.Add(AServerName, TCredentialCache.Create(Result));
end;
```

**关键特性**:

- **自动缓存**: Schannel 在凭据句柄内部自动缓存 Session
- **自动尝试**: 使用相同凭据句柄连接时，Schannel 可能自动尝试 Session 复用
- **系统级缓存**: Session 数据存储在系统内存中，进程间不共享
- **有效期管理**: Session 有效期由 Windows 系统策略控制（默认 10 小时）

#### 4.4.4 Session 数据结构

```pascal
// Schannel Session 信息
type
  SecPkgContext_SessionInfo = record
    dwFlags: DWORD;                   // 标志位
    cbSessionId: DWORD;               // Session ID 长度
    rgbSessionId: array[0..31] of Byte; // Session ID 数据
  end;

// 注意：
// 这类 native session-info 结构当前不再作为 shared production path 的直接 truth source。
// 当前仓库只允许在 opt-in isolated worker / experimental evidence lane 中调查
// SECPKG_ATTR_SESSION_INFO，并且 latest Windows evidence 仍显示该 query 可能直接打死 worker。
```

#### 4.4.5 性能优化策略

**1. 凭据句柄池化**:

```pascal
// 为不同服务器维护独立的凭据句柄
// 避免频繁创建和销毁凭据句柄
FCredHandlePool: TDictionary<string, CredHandle>;
```

**2. Session 预热**:

```pascal
// 在应用启动时预先建立连接，缓存 Session
procedure WarmupSessions(const AHosts: TStringList);
var
  LHost: string;
  LConn: ISSLConnection;
begin
  for LHost in AHosts do
  begin
    LConn := CreateConnection(LHost, 443);
    if LConn.Connect then
    begin
      // 保存 Session 供后续使用
      FSessionManager.AddSession(LHost, LConn.GetSession);
      LConn.Shutdown;
    end;
  end;
end;
```

**3. 智能过期清理**:

```pascal
// 定期清理过期 Session，避免内存泄漏
procedure TWinSSLSessionManager.ClearExpiredSessions;
var
  LPair: TPair<string, ISSLSession>;
  LExpiredKeys: TStringList;
begin
  FLock.Enter;
  try
    LExpiredKeys := TStringList.Create;
    try
      // 查找过期 Session
      for LPair in FSessions do
      begin
        if not LPair.Value.IsValid or
           (SecondsBetween(Now, LPair.Value.GetLastAccessTime) > FDefaultTimeout) then
          LExpiredKeys.Add(LPair.Key);
      end;

      // 删除过期 Session
      for var LKey in LExpiredKeys do
        FSessions.Remove(LKey);
    finally
      LExpiredKeys.Free;
    end;
  finally
    FLock.Leave;
  end;
end;
```

**4. LRU 缓存策略**:

```pascal
// 当缓存达到上限时，移除最少使用的 Session
procedure TWinSSLSessionManager.EnforceCacheLimit;
var
  LOldestKey: string;
  LOldestTime: TDateTime;
begin
  if FSessions.Count <= FMaxCacheSize then
    Exit;

  // 查找最旧的 Session
  LOldestTime := Now;
  for var LPair in FSessions do
  begin
    if LPair.Value.GetLastAccessTime < LOldestTime then
    begin
      LOldestTime := LPair.Value.GetLastAccessTime;
      LOldestKey := LPair.Key;
    end;
  end;

  // 移除最旧的 Session
  FSessions.Remove(LOldestKey);
end;
```

#### 4.4.6 线程安全

Session 管理器使用临界区（Critical Section）保证线程安全：

```pascal
procedure TWinSSLSessionManager.AddSession(const AHostName: string;
  ASession: ISSLSession);
begin
  FLock.Enter;
  try
    // 检查缓存限制
    EnforceCacheLimit;

    // 添加或更新 Session
    FSessions.AddOrSetValue(AHostName, ASession);
  finally
    FLock.Leave;
  end;
end;

function TWinSSLSessionManager.GetSession(const AHostName: string): ISSLSession;
begin
  FLock.Enter;
  try
    if FSessions.TryGetValue(AHostName, Result) then
    begin
      // 更新最后访问时间
      if Result.IsValid then
        Exit;
    end;

    Result := nil;
  finally
    FLock.Leave;
  end;
end;
```

#### 4.4.7 与 OpenSSL 的差异

| 特性             | WinSSL (Schannel)    | OpenSSL              |
| ---------------- | -------------------- | -------------------- |
| **Session 存储** | 凭据句柄内部自动缓存 | 需要手动序列化和存储 |
| **复用机制**     | 系统自动尝试；当前 fafafa.ssl public truth 仍偏保守 | 需要显式设置 Session |
| **跨进程共享**   | 不支持（进程隔离）   | 支持（通过序列化）   |
| **有效期控制**   | 系统策略控制         | 应用程序控制         |
| **内存管理**     | 系统自动管理         | 应用程序负责         |
| **性能收益**     | 当前未在 fafafa.ssl Windows CI 中 runtime-proven | 需按具体场景实测      |

#### 4.4.8 已知限制

1. **进程隔离**: Session 缓存仅在单个进程内有效，无法跨进程共享
2. **系统策略依赖**: Session 有效期受 Windows 组策略控制，应用程序无法直接修改
3. **服务器支持**: Session 复用需要服务器端支持，某些服务器可能拒绝复用
4. **TLS 1.3 差异**: TLS 1.3 的 Session 复用机制与 TLS 1.2 不同（使用 PSK）
5. **native probe 隔离**: `SECPKG_ATTR_SESSION_INFO` 当前只允许留在 opt-in isolated worker / experimental evidence lane，不能回流 shared production path

---

## 5. 错误处理

### 5.1 Windows 错误码映射

```pascal
function MapSchannelError(AWinError: DWORD): TSSLErrorCode;
begin
  case AWinError of
    SEC_E_INVALID_HANDLE: Result := sslErrGeneral;
    SEC_E_INVALID_TOKEN: Result := sslErrProtocol;
    SEC_E_CERT_EXPIRED: Result := sslErrCertificate;
    SEC_E_CERT_UNKNOWN: Result := sslErrCertificate;
    SEC_E_INCOMPLETE_MESSAGE: Result := sslErrIO;
    SEC_I_CONTINUE_NEEDED: Result := sslErrNone; // 继续握手
    // ... 更多映射
  end;
end;
```

### 5.2 错误上下文信息

- 保留原始 Windows 错误码和消息
- 提供操作上下文（握手、数据传输等）
- 包含证书验证详情

## 6. 配置选项

### 6.1 协议版本控制

```pascal
// 设置支持的 TLS 版本
procedure SetProtocolVersions(AVersions: TSSLProtocolVersionSet);
var
  LProtocols: DWORD;
begin
  LProtocols := 0;
  if sslProtocolTLS10 in AVersions then
    LProtocols := LProtocols or SP_PROT_TLS1_0;
  if sslProtocolTLS11 in AVersions then
    LProtocols := LProtocols or SP_PROT_TLS1_1;
  if sslProtocolTLS12 in AVersions then
    LProtocols := LProtocols or SP_PROT_TLS1_2;
  if sslProtocolTLS13 in AVersions then
    LProtocols := LProtocols or SP_PROT_TLS1_3;

  FConfig.EnabledProtocols := LProtocols;
end;
```

### 6.2 证书验证配置

- 使用系统证书存储
- 支持自定义证书验证回调
- 支持 info/state callback
- 当前 published callback surface = verify/info；password callback 仍未接入 WinSSL runtime，non-nil assignment 应 fail-closed 为 unsupported
- 当前 password-protected private key surface = PFX/P12 import；PEM private-key password path 仍未发布
- 当前 bare DER / PKCS#8 private-key load surface 仍未发布；如需裸私钥导入，请改用 OpenSSL backend 或先封装为 PFX/P12
- 企业证书策略集成

## 7. 平台兼容性

### 7.1 Windows 版本支持

- **Windows 7/Server 2008 R2**：TLS 1.0, 1.1, 1.2
- **Windows 8/Server 2012**：增强的 TLS 1.2 支持
- **Windows 10 1903+ / Server 2022+**：public capability 会发布 TLS 1.3 支持
- **更细的可用性差异**：仍受具体 Windows build 和系统策略影响，需 Windows 主机 runtime 验证

### 7.2 运行时检测

```pascal
function GetSystemTLSSupport: TSSLProtocolVersionSet;
var
  LVersionInfo: OSVERSIONINFO;
begin
  Result := [];
  LVersionInfo.dwOSVersionInfoSize := SizeOf(OSVERSIONINFO);
  GetVersionEx(LVersionInfo);

  // 基于 Windows 版本确定支持的 TLS 版本
  if (LVersionInfo.dwMajorVersion >= 6) then
  begin
    Result := Result + [sslProtocolTLS10, sslProtocolTLS11, sslProtocolTLS12];

    // TLS 1.3 仅在 Windows 10 build 18362+ / Server 2022+ 发布为 supported
    if (LVersionInfo.dwMajorVersion >= 10) and (LVersionInfo.dwBuildNumber >= 18362) then
      Result := Result + [sslProtocolTLS13];
  end;
end;
```

## 8. 性能优化

### 8.1 缓冲区管理

- 重用安全缓冲区减少分配
- 优化数据拷贝操作
- 批量处理多个数据包

### 8.2 上下文缓存

- 缓存握手结果用于会话复用
- 延迟初始化非关键组件

## 9. 安全考虑

### 9.1 内存安全

- 及时清零敏感数据
- 正确释放 Windows 句柄和上下文
- 防止敏感信息泄漏

### 9.2 企业集成

- 遵循组策略设置
- 支持域证书自动配置
- FIPS 140-2 合规模式

## 10. 测试策略

### 10.1 兼容性测试

- 不同 Windows 版本的功能验证
- 与其他后端的行为一致性测试
- 企业环境下的策略遵循测试

### 10.2 性能基准

- 与 OpenSSL 性能对比
- 内存使用效率测试
- 大量并发连接测试

---

**文档版本**: 1.0  
**创建时间**: 2025-09-28  
**更新历史**: 初始版本
