# fafafa.ssl API 参考文档

> **版本**: v0.8  
> **最后更新**: 2025-10-24

## 目录

- [核心接口](#核心接口)
- [OpenSSL 后端](#openssl-后端)
- [WinSSL 后端](#winssl-后端)
- [数据类型](#数据类型)
- [错误处理](#错误处理)
- [工具函数](#工具函数)

---

## 核心接口

### ISSLLibrary

SSL/TLS 库的主接口，提供库管理和实例创建功能。

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
  
  // 功能检测
  function IsProtocolSupported(aProtocol: TSSLProtocolVersion): Boolean;
  function IsCipherSupported(const aCipherName: string): Boolean;
  function IsFeatureSupported(const aFeatureName: string): Boolean;
  
  // 错误处理
  function GetLastError: Integer;
  function GetLastErrorString: string;
  procedure ClearError;
  
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
  LLib := CreateOpenSSLLibrary;
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

```pascal
ISSLContext = interface
  // 上下文类型
  function GetContextType: TSSLContextType;
  
  // 协议版本
  procedure SetProtocolVersions(aVersions: TSSLProtocolVersions);
  function GetProtocolVersions: TSSLProtocolVersions;
  
  // 证书与密钥
  procedure LoadCertificate(const aFileName: string); overload;
  procedure LoadCertificate(aStream: TStream); overload;
  procedure LoadCertificate(aCert: ISSLCertificate); overload;
  procedure LoadPrivateKey(const aFileName: string; const aPassword: string = ''); overload;
  procedure LoadPrivateKey(aStream: TStream; const aPassword: string = ''); overload;
  
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
  procedure SetCipherList(const aCipherList: string);
  function GetCipherList: string;
  procedure SetCipherSuites(const aCipherSuites: string);
  function GetCipherSuites: string;
  
  // 会话管理
  procedure SetSessionCacheMode(aEnabled: Boolean);
  function GetSessionCacheMode: Boolean;
  procedure SetSessionTimeout(aTimeout: Integer);
  function GetSessionTimeout: Integer;
  
  // 连接创建
  function CreateConnection(aSocket: THandle): ISSLConnection; overload;
  function CreateConnection(aStream: TStream): ISSLConnection; overload;
  
  // 状态
  function IsValid: Boolean;
  function GetNativeHandle: Pointer;
end;
```

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

---

### ISSLCertificate

X.509 证书接口。

```pascal
ISSLCertificate = interface
  // 加载与保存
  function LoadFromFile(const aFileName: string): Boolean;
  function LoadFromStream(aStream: TStream): Boolean;
  function LoadFromPEM(const aPEM: string): Boolean;
  function LoadFromDER(const aDER: TBytes): Boolean;
  function SaveToFile(const aFileName: string): Boolean;
  function SaveToPEM: string;
  function SaveToDER: TBytes;
  
  // 证书信息
  function GetSubject: string;
  function GetIssuer: string;
  function GetSerialNumber: string;
  function GetNotBefore: TDateTime;
  function GetNotAfter: TDateTime;
  function GetPublicKey: string;
  function GetVersion: Integer;
  
  // 验证
  function Verify(aCAStore: ISSLCertificateStore): Boolean;
  function VerifyEx(aCAStore: ISSLCertificateStore; 
    aFlags: TSSLCertVerifyFlags; out aResult: TSSLCertVerifyResult): Boolean;
  function VerifyHostname(const aHostname: string): Boolean;
  function IsExpired: Boolean;
  function IsSelfSigned: Boolean;
  function IsCA: Boolean;
  
  // 扩展
  function GetSubjectAltNames: TStringList;
  function GetKeyUsage: TStringList;
  function GetExtendedKeyUsage: TStringList;
  
  // 指纹
  function GetFingerprintSHA1: string;
  function GetFingerprintSHA256: string;
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

### ISSLConnection

SSL/TLS 连接接口。

```pascal
ISSLConnection = interface
  // 连接管理
  function Connect: Boolean;
  function Accept: Boolean;
  procedure Shutdown;
  procedure Close;
  
  // 数据传输
  function Read(var aBuffer; aCount: Integer): Integer;
  function Write(const aBuffer; aCount: Integer): Integer;

  // 字符串便捷方法
  // ReadString: 从 SSL 连接读取数据到字符串
  //   - AStr: 输出参数，接收读取的数据
  //   - 返回: True 表示成功读取数据，False 表示无数据或错误
  //   - 缓冲区大小: 4096 字节
  //   - 编码: 支持 UTF-8，使用 SetString 直接转换
  //   - 注意: 对于大于 4096 字节的数据，需要多次调用
  function ReadString(out AStr: string): Boolean;

  // WriteString: 将字符串写入 SSL 连接
  //   - AStr: 要写入的字符串数据
  //   - 返回: True 表示完整写入成功，False 表示部分写入或失败
  //   - 编码: 直接发送字符串字节，支持 UTF-8
  //   - 注意: 确保完整写入，部分写入视为失败
  function WriteString(const AStr: string): Boolean;

  // 状态查询
  function IsConnected: Boolean;
  function GetState: TSSLConnectionState;
  function GetConnectionInfo: TSSLConnectionInfo;
  function GetProtocolVersion: TSSLProtocolVersion;
  function GetCipherName: string;
  function GetCipherBits: Integer;
  
  // 证书信息
  function GetPeerCertificate: ISSLCertificate;
  function GetPeerCertificateChain: TSSLCertificateArray;
  function VerifyPeerCertificate: Boolean;
  
  // 会话信息
  function GetSessionID: string;
  function IsSessionReused: Boolean;

  // Phase 3.3: 监控和诊断
  function GetHealthStatus: TSSLHealthStatus;
  function IsHealthy: Boolean;
  function GetDiagnosticInfo: TSSLDiagnosticInfo;
  function GetPerformanceMetrics: TSSLPerformanceMetrics;
end;
```

**使用示例**:

**基本用法**:
```pascal
var
  LConn: ISSLConnection;
  LData: string;
begin
  LConn := LContext.CreateConnection(MySocket);
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

**读取大数据**:
```pascal
var
  LConn: ISSLConnection;
  LChunk: string;
  LFullData: string;
begin
  LConn := LContext.CreateConnection(MySocket);
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

**错误处理**:
```pascal
var
  LConn: ISSLConnection;
  LRequest, LResponse: string;
begin
  LConn := LContext.CreateConnection(MySocket);

  try
    if not LConn.Connect then
      raise Exception.Create('连接失败');

    LRequest := 'GET / HTTP/1.1'#13#10 +
                'Host: example.com'#13#10 +
                'Connection: close'#13#10#13#10;

    if not LConn.WriteString(LRequest) then
      raise Exception.Create('发送请求失败');

    if not LConn.ReadString(LResponse) then
      raise Exception.Create('读取响应失败');

    WriteLn('响应: ', LResponse);

  finally
    LConn.Shutdown;
  end;
end;
```

**获取连接详细信息**:
```pascal
var
  LConn: ISSLConnection;
  LInfo: TSSLConnectionInfo;
begin
  LConn := LContext.CreateConnection(MySocket);

  if LConn.Connect then
  begin
    // 获取完整的连接信息
    LInfo := LConn.GetConnectionInfo;

    WriteLn('=== SSL/TLS 连接信息 ===');
    WriteLn('协议版本: ', GetProtocolName(LInfo.ProtocolVersion));
    WriteLn('密码套件: ', LInfo.CipherSuite);
    WriteLn('密钥长度: ', LInfo.KeySize, ' bits');
    WriteLn('MAC 长度: ', LInfo.MacSize, ' bytes');
    WriteLn('Session 复用: ', BoolToStr(LInfo.IsResumed, True));
    WriteLn('服务器名称: ', LInfo.ServerName);

    if LInfo.ALPNProtocol <> '' then
      WriteLn('ALPN 协议: ', LInfo.ALPNProtocol);

    LConn.Shutdown;
  end;
end;
```

**监控和诊断**:
```pascal
var
  LConn: ISSLConnection;
  LInfo: TSSLConnectionInfo;
  LStartTime: TDateTime;
begin
  LStartTime := Now;
  LConn := LContext.CreateConnection(MySocket);

  if LConn.Connect then
  begin
    LInfo := LConn.GetConnectionInfo;

    // 记录连接性能指标
    WriteLn('=== 连接性能指标 ===');
    WriteLn('连接时间: ', MilliSecondsBetween(Now, LStartTime), ' ms');
    WriteLn('协议: ', GetProtocolName(LInfo.ProtocolVersion));
    WriteLn('密码套件: ', LInfo.CipherSuite);
    WriteLn('密钥强度: ', LInfo.KeySize, ' bits');

    // 检查是否使用了强加密
    if LInfo.KeySize >= 256 then
      WriteLn('✓ 使用强加密')
    else if LInfo.KeySize >= 128 then
      WriteLn('⚠ 使用中等强度加密')
    else
      WriteLn('✗ 加密强度不足');

    // 检查协议版本
    if LInfo.ProtocolVersion in [sslProtocolTLS12, sslProtocolTLS13] then
      WriteLn('✓ 使用现代 TLS 协议')
    else
      WriteLn('⚠ 使用旧版 TLS 协议');

    LConn.Shutdown;
  end;
end;
```

### WinSSL Session 管理

WinSSL 后端提供完整的 TLS Session 复用功能，通过 Windows Schannel 的凭据句柄缓存机制实现。Session 复用可以显著提升 HTTPS 连接性能（预期提升 70-90%），特别适合需要频繁建立连接的场景。

#### 核心接口

```pascal
ISSLSession = interface
  function GetSessionData: TBytes;
  procedure SetSessionData(const aData: TBytes);
  function IsValid: Boolean;
  function GetCreationTime: TDateTime;
  function GetLastAccessTime: TDateTime;
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
  LSession: ISSLSession;
begin
  // 创建 WinSSL 库
  LLib := CreateWinSSLLibrary;
  LLib.Initialize;

  LContext := LLib.CreateContext(sslCtxClient);

  // 第一次连接 - 完整握手
  LConn1 := LContext.CreateConnection(Socket1);
  if LConn1.Connect then
  begin
    WriteLn('第一次连接成功');

    // 获取 Session
    LSession := LConn1.GetSession;
    WriteLn('Session ID: ', LConn1.GetSessionID);

    LConn1.Shutdown;
  end;

  // 第二次连接 - 复用 Session
  LConn2 := LContext.CreateConnection(Socket2);
  LConn2.SetSession(LSession);  // 设置之前保存的 Session

  if LConn2.Connect then
  begin
    WriteLn('第二次连接成功');

    // 检查是否复用了 Session
    if LConn2.IsSessionResumed then
      WriteLn('✓ Session 复用成功 - 握手时间大幅减少')
    else
      WriteLn('✗ Session 未复用 - 执行了完整握手');

    LConn2.Shutdown;
  end;
end;
```

**多连接 Session 缓存**:
```pascal
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LSessionCache: TDictionary<string, ISSLSession>;
  LConn: ISSLConnection;
  LHost: string;
begin
  LLib := CreateWinSSLLibrary;
  LLib.Initialize;
  LContext := LLib.CreateContext(sslCtxClient);

  // 创建 Session 缓存
  LSessionCache := TDictionary<string, ISSLSession>.Create;
  try
    // 连接到多个主机
    for LHost in ['api.example.com', 'cdn.example.com', 'www.example.com'] do
    begin
      LConn := LContext.CreateConnection(ConnectToHost(LHost, 443));

      // 尝试复用缓存的 Session
      if LSessionCache.ContainsKey(LHost) then
        LConn.SetSession(LSessionCache[LHost]);

      if LConn.Connect then
      begin
        WriteLn(Format('连接到 %s: Session %s',
          [LHost,
           IfThen(LConn.IsSessionResumed, '复用', '新建')]));

        // 保存 Session 供后续使用
        LSessionCache.AddOrSetValue(LHost, LConn.GetSession);

        // 执行业务逻辑
        LConn.WriteString('GET / HTTP/1.1'#13#10 +
                         'Host: ' + LHost + #13#10#13#10);
        WriteLn(LConn.ReadString);

        LConn.Shutdown;
      end;
    end;
  finally
    LSessionCache.Free;
  end;
end;
```

#### 性能优化建议

1. **长连接场景**: 对于需要频繁连接同一服务器的应用（如 REST API 客户端），始终保存和复用 Session
2. **Session 有效期**: WinSSL Session 默认有效期由 Windows 系统策略控制，通常为 10 小时
3. **内存管理**: Session 数据较小（通常 < 1KB），可以安全缓存大量 Session
4. **线程安全**: TWinSSLSession 对象是线程安全的，可以在多线程环境中共享

#### 与 OpenSSL 的差异

| 特性 | WinSSL | OpenSSL |
|------|--------|---------|
| Session 存储 | 自动（凭据句柄缓存） | 手动（需要序列化） |
| Session 有效期 | 系统策略控制 | 应用程序控制 |
| 跨进程共享 | 不支持 | 支持（通过序列化） |
| 性能提升 | 70-90% | 70-90% |

#### 错误处理

```pascal
var
  LConn: ISSLConnection;
  LSession: ISSLSession;
begin
  LConn := LContext.CreateConnection(MySocket);

  // 尝试设置 Session
  if Assigned(LSession) and LSession.IsValid then
    LConn.SetSession(LSession)
  else
    WriteLn('警告: Session 无效或已过期，将执行完整握手');

  if LConn.Connect then
  begin
    // 连接成功
    if not LConn.IsSessionResumed then
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
  LSession: ISSLSession;
begin
  LConn := LContext.CreateConnection(MySocket);

  if LConn.Connect then
  begin
    LSession := LConn.GetSession;

    // 输出 Session 信息
    WriteLn('Session 信息:');
    WriteLn('  ID: ', LConn.GetSessionID);
    WriteLn('  创建时间: ', DateTimeToStr(LSession.GetCreationTime));
    WriteLn('  最后访问: ', DateTimeToStr(LSession.GetLastAccessTime));
    WriteLn('  是否复用: ', BoolToStr(LConn.IsSessionResumed, True));
    WriteLn('  协议版本: ', GetProtocolName(LConn.GetProtocolVersion));
    WriteLn('  密码套件: ', LConn.GetCipherName);
  end;
end;
```

#### Phase 3.3: 监控和诊断示例

**健康检查**:
```pascal
var
  LConn: ISSLConnection;
  LHealth: TSSLHealthStatus;
begin
  LConn := LContext.CreateConnection(MySocket);

  if LConn.Connect then
  begin
    // 快速健康检查
    if LConn.IsHealthy then
      WriteLn('✓ 连接健康')
    else
      WriteLn('✗ 连接不健康');

    // 获取详细健康状态
    LHealth := LConn.GetHealthStatus;
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
  LPerf: TSSLPerformanceMetrics;
begin
  LConn := LContext.CreateConnection(MySocket);

  if LConn.Connect then
  begin
    // 获取性能指标
    LPerf := LConn.GetPerformanceMetrics;

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
  LDiag: TSSLDiagnosticInfo;
  I: Integer;
begin
  LConn := LContext.CreateConnection(MySocket);

  if LConn.Connect then
  begin
    // 获取完整诊断信息
    LDiag := LConn.GetDiagnosticInfo;

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
  LLib := CreateWinSSLLibrary;
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
  LLib := CreateWinSSLLibrary;
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
      if not LConn.IsHealthy then
      begin
        LHealth := LConn.GetHealthStatus;
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
  sslOpenSSL,  // OpenSSL 后端
  sslWinSSL,   // Windows Schannel 后端
  sslMbedTLS   // MbedTLS 后端（计划中）
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
  MacSize: Integer;                      // MAC长度（字节）
  IsResumed: Boolean;                    // 是否为恢复的会话
  SessionId: string;                     // 会话ID
  CompressionMethod: string;             // 压缩方法
  ServerName: string;                    // SNI服务器名称
  ALPNProtocol: string;                  // ALPN协商的协议
  PeerCertificate: TSSLCertificateInfo;  // 对端证书信息
end;
```

**说明**:
- `GetConnectionInfo` 方法返回此结构，包含连接的完整信息
- 用于监控、诊断和安全审计
- WinSSL 后端通过 `QueryContextAttributesW` API 获取这些信息
- 所有字段在连接建立后填充，未连接时返回默认值

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
- 通过 `ISSLConnection.GetHealthStatus` 获取连接健康状态
- 用于快速诊断连接问题和监控连接状态
- `ConnectionAge` 从连接创建时开始计算

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
- 通过 `ISSLConnection.GetPerformanceMetrics` 获取性能指标
- 用于性能分析和优化
- `HandshakeTime` 使用高精度计时器测量

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
- 通过 `ISSLConnection.GetDiagnosticInfo` 获取完整诊断信息
- 包含连接的所有监控和诊断数据
- 用于故障排查和性能分析

---

## 错误处理

### TSSLErrorCode
```pascal
TSSLErrorCode = (
  sslErrNone,              // 无错误
  sslErrGeneral,           // 一般错误
  sslErrNotInitialized,    // 未初始化
  sslErrInvalidParameter,  // 无效参数
  sslErrOutOfMemory,       // 内存不足
  sslErrTimeout,           // 超时
  sslErrConnectionClosed,  // 连接关闭
  sslErrHandshakeFailed,   // 握手失败
  sslErrCertificateVerifyFailed, // 证书验证失败
  sslErrCipherNotSupported,      // 不支持的密码
  sslErrProtocolNotSupported     // 不支持的协议
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
function GetWinSSLErrorMessageCN(aErrorCode: DWORD): string;
function GetWinSSLErrorMessageEN(aErrorCode: DWORD): string;
```

---

## 工具函数

### OpenSSL 工具
```pascal
// 库管理
function OpenSSLAvailable: Boolean;
function LoadOpenSSL(const aLibraryPath: string = ''): Boolean;
procedure UnloadOpenSSL;
function GetOpenSSLVersion: string;
function GetOpenSSLVersionNumber: Cardinal;

// 证书工具
function LoadCertificateFromFile(const aFileName: string): PX509;
function LoadPrivateKeyFromFile(const aFileName: string; const aPassword: string = ''): PEVP_PKEY;
function VerifyCertificate(aCert: PX509; aCAStore: PX509_STORE): Boolean;

// 协议工具
function ProtocolToOpenSSL(aProtocol: TSSLProtocolVersion): Integer;
function GetProtocolName(aProtocol: TSSLProtocolVersion): string;
```

### WinSSL 工具
```pascal
// 企业功能
function IsFipsModeEnabled: Boolean;
function GetEnterpriseTrustedRoots: TStringList;
function GetGroupPolicies: TStringList;
```

---

## 工厂函数

### 创建后端实例
```pascal
// OpenSSL
function CreateOpenSSLLibrary: ISSLLibrary;

// WinSSL
function CreateWinSSLLibrary: ISSLLibrary;

// 自动选择
function CreateSSLLibrary(aType: TSSLLibraryType = sslOpenSSL): ISSLLibrary;
```

---

## 回调类型

```pascal
// 日志回调
TSSLLogLevel = (sslLogDebug, sslLogInfo, sslLogWarning, sslLogError);
TSSLLogCallback = procedure(aLevel: TSSLLogLevel; const aMessage: string) of object;

// 验证回调
TSSLVerifyCallback = function(aPreverified: Boolean; aCert: ISSLCertificate): Boolean of object;

// 密码回调
TSSLPasswordCallback = function(const aHint: string; aMaxLen: Integer): string of object;

// 信息回调
TSSLInfoCallback = procedure(const aInfo: string) of object;
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
  fafafa.ssl.openssl,
  fafafa.ssl.abstract.intf;

var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LCert: ISSLCertificate;
begin
  // 创建并初始化库
  LLib := CreateOpenSSLLibrary;
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
    
    // 创建连接
    LConn := LContext.CreateConnection(MySocket);
    if LConn.Connect then
    begin
      // 验证证书
      LCert := LConn.GetPeerCertificate;
      if LCert.VerifyHostname('example.com') then
      begin
        // 发送和接收数据
        LConn.WriteString('Hello, SSL!');
        WriteLn('收到: ', LConn.ReadString);
      end;
      
      LConn.Shutdown;
    end;
  finally
    LLib.Finalize;
  end;
end.
```

---

## 参考资源

- **OpenSSL 文档**: https://www.openssl.org/docs/
- **Windows Schannel**: https://docs.microsoft.com/en-us/windows/win32/secauthn/schannel
- **RFC 5280** (X.509): https://tools.ietf.org/html/rfc5280
- **RFC 8446** (TLS 1.3): https://tools.ietf.org/html/rfc8446

---

**版本历史**:
- v0.8 (2025-10-24): 添加 VerifyEx 方法和 WinSSL 企业功能
- v0.7 (2025-10-01): 初始 API 文档

