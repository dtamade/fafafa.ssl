# WinSSL 最佳实践指南

> **版本**: v1.0
> **最后更新**: 2026-01-18

本指南提供 fafafa.ssl WinSSL 后端的最佳实践建议，帮助你构建安全、高性能、可维护的 TLS/SSL 应用程序。

## 目录

- [安全最佳实践](#安全最佳实践)
- [性能最佳实践](#性能最佳实践)
- [错误处理最佳实践](#错误处理最佳实践)
- [资源管理最佳实践](#资源管理最佳实践)
- [测试最佳实践](#测试最佳实践)
- [生产部署最佳实践](#生产部署最佳实践)

---

## 安全最佳实践

### 1. 始终验证证书

**❌ 错误做法**:
```pascal
// 禁用证书验证（仅用于测试！）
LContext.SetVerifyMode([]);
```

**✅ 正确做法**:
```pascal
// 启用完整的证书验证
LContext.SetVerifyMode([sslVerifyPeer, sslVerifyFailIfNoPeerCert]);

// 加载系统 CA 证书
LContext.LoadCAFile('C:\Windows\curl-ca-bundle.crt');  // Windows
// 或
LContext.LoadCAPath('/etc/ssl/certs');  // Linux
```

### 2. 使用强密码套件

**❌ 错误做法**:
```pascal
// 允许弱密码套件
LContext.SetCipherSuites('ALL');
```

**✅ 正确做法**:
```pascal
// 只使用强密码套件
LContext.SetCipherSuites(
  'TLS_AES_128_GCM_SHA256:' +
  'TLS_AES_256_GCM_SHA384:' +
  'ECDHE-RSA-AES128-GCM-SHA256:' +
  'ECDHE-RSA-AES256-GCM-SHA384'
);
```

### 3. 使用最新的 TLS 版本

**❌ 错误做法**:
```pascal
// 允许旧版本 TLS
LContext.SetProtocolVersions([sslProtocolTLS10, sslProtocolTLS11]);
```

**✅ 正确做法**:
```pascal
// 只使用 TLS 1.2 和 TLS 1.3
LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
```

### 4. 验证主机名

**❌ 错误做法**:
```pascal
// 不验证主机名
LConn.Connect;
```

**✅ 正确做法**:
```pascal
// 设置服务器名称（SNI）并验证
LConn.SetServerName('example.com');
if LConn.Connect then
begin
  // 验证证书主机名
  LCert := LConn.GetPeerCertificate;
  if not LCert.VerifyHostname('example.com') then
    raise Exception.Create('主机名不匹配');
end;
```

### 5. 保护私钥

**❌ 错误做法**:
```pascal
// 私钥明文存储
LKey := '-----BEGIN PRIVATE KEY-----...';
```

**✅ 正确做法**:
```pascal
// 使用加密的私钥文件
LContext.LoadPrivateKey('server.key', 'password');

// 或使用 Windows 证书存储
LStore := LLib.CreateCertificateStore;
LStore.Open(SSL_STORE_MY);  // 个人证书存储
LCert := LStore.FindBySubject('CN=example.com');
```

### 6. 实施证书固定（高安全场景）

```pascal
type
  TCertificatePinner = class
  private
    FPinnedFingerprints: TStringList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddPin(const AFingerprint: string);
    function Verify(ACert: ISSLCertificate): Boolean;
  end;

// 使用示例
var
  LPinner: TCertificatePinner;
  LCert: ISSLCertificate;
begin
  LPinner := TCertificatePinner.Create;
  try
    // 添加预期的证书指纹
    LPinner.AddPin('SHA256:1234567890abcdef...');

    if LConn.Connect then
    begin
      LCert := LConn.GetPeerCertificate;
      if not LPinner.Verify(LCert) then
        raise Exception.Create('证书固定验证失败');
    end;
  finally
    LPinner.Free;
  end;
end;
```

---

## 性能最佳实践

### 1. 复用 Context 对象

**❌ 错误做法**:
```pascal
// 每次连接创建新 Context
for i := 1 to 100 do
begin
  LContext := LLib.CreateContext(sslCtxClient);
  LConn := LContext.CreateConnection(Socket);
  LConn.Connect;
end;
```

**✅ 正确做法**:
```pascal
// 复用 Context 对象
LContext := LLib.CreateContext(sslCtxClient);
for i := 1 to 100 do
begin
  LConn := LContext.CreateConnection(Socket);
  LConn.Connect;
end;
```

### 2. 启用 Session 复用

**❌ 错误做法**:
```pascal
// 每次完整握手
for i := 1 to 100 do
begin
  LConn := LContext.CreateConnection(Socket);
  LConn.Connect;  // 完整握手
  LConn.Shutdown;
end;
```

**✅ 正确做法**:
```pascal
// 使用 Session 复用
LConn1 := LContext.CreateConnection(Socket1);
if LConn1.Connect then
begin
  LSession := LConn1.GetSession;
  LConn1.Shutdown;
end;

// 后续连接复用 Session
for i := 1 to 100 do
begin
  LConn := LContext.CreateConnection(Socket);
  LConn.SetSession(LSession);
  LConn.Connect;  // 快速握手
  LConn.Shutdown;
end;
```

### 3. 使用合适的缓冲区大小

**❌ 错误做法**:
```pascal
// 缓冲区太小
const BUFFER_SIZE = 512;  // 太小，频繁系统调用
```

**✅ 正确做法**:
```pascal
// 根据数据大小选择合适的缓冲区
const BUFFER_SIZE = 8192;  // 8 KB，适合大多数场景

// 大文件传输
const LARGE_BUFFER_SIZE = 65536;  // 64 KB
```

### 4. 批量读写数据

**❌ 错误做法**:
```pascal
// 逐字节读取
for i := 1 to FileSize do
  LConn.Read(LByte, 1);
```

**✅ 正确做法**:
```pascal
// 批量读取
var
  LBuffer: array[0..8191] of Byte;
  LRead: Integer;
begin
  repeat
    LRead := LConn.Read(LBuffer, SizeOf(LBuffer));
    if LRead > 0 then
      ProcessData(@LBuffer, LRead);
  until LRead <= 0;
end;
```

---

## 错误处理最佳实践

### 1. 使用结构化错误处理

**❌ 错误做法**:
```pascal
// 忽略错误
LConn.Connect;
LConn.WriteString('data');
```

**✅ 正确做法**:
```pascal
// 完整的错误处理
try
  if not LConn.Connect then
  begin
    var LErr := GetLastError;
    LogError(sslErrorError, LErr,
      GetSchannelErrorMessageCN(LErr),
      'Connection.Connect');
    raise EWinSSLException.Create('连接失败', LErr);
  end;

  if not LConn.WriteString('data') then
  begin
    var LErr := GetLastError;
    LogError(sslErrorError, LErr,
      GetSchannelErrorMessageCN(LErr),
      'Connection.WriteString');
    raise EWinSSLException.Create('写入失败', LErr);
  end;
except
  on E: EWinSSLException do
  begin
    WriteLn('WinSSL 错误: ', E.Message);
    WriteLn('错误码: 0x', IntToHex(E.ErrorCode, 8));
    WriteLn('分类: ', GetWinSSLErrorCategory(E.ErrorCode));
  end;
end;
```

### 2. 记录详细的错误上下文

**❌ 错误做法**:
```pascal
// 只记录错误码
WriteLn('Error: ', LErr);
```

**✅ 正确做法**:
```pascal
// 记录完整的错误上下文
procedure LogDetailedError(const AOperation: string; AErrorCode: DWORD);
var
  LErrorInfo: TSSLErrorInfo;
begin
  LErrorInfo.Level := sslErrorError;
  LErrorInfo.Code := AErrorCode;
  LErrorInfo.Message := GetSchannelErrorMessageCN(AErrorCode);
  LErrorInfo.Context := AOperation;
  LErrorInfo.Timestamp := Now;

  WriteLn(FormatErrorInfo(LErrorInfo));
  WriteLn('错误分类: ', GetWinSSLErrorCategory(AErrorCode));
  WriteLn('系统消息: ', GetSystemErrorMessage(AErrorCode));
end;
```

### 3. 实现重试逻辑

**❌ 错误做法**:
```pascal
// 连接失败就放弃
if not LConn.Connect then
  raise Exception.Create('连接失败');
```

**✅ 正确做法**:
```pascal
// 实现指数退避重试
function ConnectWithRetry(AConn: ISSLConnection;
  AMaxRetries: Integer = 3): Boolean;
var
  LRetry: Integer;
  LDelay: Integer;
begin
  Result := False;
  LDelay := 1000;  // 初始延迟 1 秒

  for LRetry := 1 to AMaxRetries do
  begin
    try
      if AConn.Connect then
      begin
        Result := True;
        Exit;
      end;
    except
      on E: Exception do
        WriteLn('重试 ', LRetry, '/', AMaxRetries, ' 失败: ', E.Message);
    end;

    if LRetry < AMaxRetries then
    begin
      Sleep(LDelay);
      LDelay := LDelay * 2;  // 指数退避
    end;
  end;
end;
```

---

## 资源管理最佳实践

### 1. 正确释放连接

**❌ 错误做法**:
```pascal
// 未调用 Shutdown
LConn := LContext.CreateConnection(Socket);
LConn.Connect;
// 连接未正确关闭
```

**✅ 正确做法**:
```pascal
// 使用 try-finally 确保释放
LConn := LContext.CreateConnection(Socket);
try
  if LConn.Connect then
  begin
    // 执行操作...
  end;
finally
  LConn.Shutdown;  // 正确关闭连接
end;
```

### 2. 管理 Session 缓存

**❌ 错误做法**:
```pascal
// Session 缓存无限增长
FSessionCache.Add(Host, Session);  // 永不清理
```

**✅ 正确做法**:
```pascal
// 定期清理过期 Session
procedure TSessionCache.CleanExpired;
var
  LPair: TPair<string, ISSLSession>;
  LExpiredKeys: TStringList;
begin
  LExpiredKeys := TStringList.Create;
  try
    for LPair in FCache do
    begin
      if not LPair.Value.IsValid or
         (SecondsBetween(Now, LPair.Value.GetLastAccessTime) > FMaxAge) then
        LExpiredKeys.Add(LPair.Key);
    end;

    for var LKey in LExpiredKeys do
      FCache.Remove(LKey);
  finally
    LExpiredKeys.Free;
  end;
end;

// 定期调用清理
if SecondsBetween(Now, FLastCleanup) > 300 then  // 每 5 分钟
begin
  CleanExpired;
  FLastCleanup := Now;
end;
```

### 3. 使用连接池

**❌ 错误做法**:
```pascal
// 每次请求创建新连接
for i := 1 to 1000 do
begin
  LConn := LContext.CreateConnection(CreateSocket(Host, Port));
  LConn.Connect;
  // 执行请求...
  LConn.Shutdown;
end;
```

**✅ 正确做法**:
```pascal
// 使用连接池复用连接
LPool := TSSLConnectionPool.Create(LContext, Host, Port, 10);
try
  for i := 1 to 1000 do
  begin
    LConn := LPool.Acquire;
    try
      if LConn.IsConnected or LConn.Connect then
      begin
        // 执行请求...
      end;
    finally
      LPool.Release(LConn);
    end;
  end;
finally
  LPool.Free;
end;
```

---

## 测试最佳实践

### 1. 使用本地测试服务器

**❌ 错误做法**:
```pascal
// 依赖外部服务器测试
LConn.Connect('example.com', 443);
```

**✅ 正确做法**:
```pascal
// 使用本地测试服务器
procedure SetupTestServer;
begin
  // 启动本地 HTTPS 服务器
  LServer := TSSLTestServer.Create('localhost', 8443);
  LServer.LoadCertificate('test-cert.pem', 'test-key.pem');
  LServer.Start;
end;

// 测试
LConn.Connect('localhost', 8443);
```

### 2. 测试错误场景

**❌ 错误做法**:
```pascal
// 只测试成功场景
procedure TestConnect;
begin
  Assert(LConn.Connect);
end;
```

**✅ 正确做法**:
```pascal
// 测试各种错误场景
procedure TestConnectErrors;
begin
  // 测试证书过期
  TestCertificateExpired;

  // 测试主机名不匹配
  TestHostnameMismatch;

  // 测试不受信任的根证书
  TestUntrustedRoot;

  // 测试连接超时
  TestConnectionTimeout;

  // 测试协议不匹配
  TestProtocolMismatch;
end;
```

### 3. 使用 Mock 对象

```pascal
type
  TMockSSLConnection = class(TInterfacedObject, ISSLConnection)
  private
    FConnectResult: Boolean;
    FReadData: TBytes;
    FWriteData: TBytes;

  public
    property ConnectResult: Boolean read FConnectResult write FConnectResult;
    property ReadData: TBytes read FReadData write FReadData;
    property WriteData: TBytes read FWriteData;

    function Connect: Boolean;
    function Read(var aBuffer; aCount: Integer): Integer;
    function Write(const aBuffer; aCount: Integer): Integer;
    // ... 其他方法 ...
  end;

// 使用 Mock 测试
procedure TestWithMock;
var
  LMock: TMockSSLConnection;
begin
  LMock := TMockSSLConnection.Create;
  try
    LMock.ConnectResult := True;
    LMock.ReadData := TEncoding.UTF8.GetBytes('HTTP/1.1 200 OK');

    // 测试代码...
    Assert(LMock.Connect);
    Assert(Length(LMock.WriteData) > 0);
  finally
    LMock.Free;
  end;
end;
```

---

## 生产部署最佳实践

### 1. 配置日志记录

**❌ 错误做法**:
```pascal
// 不记录日志
LConn.Connect;
```

**✅ 正确做法**:
```pascal
// 配置结构化日志
var
  LErrorHandler: ISSLErrorHandler;
begin
  LErrorHandler := TSSLFileErrorHandler.Create('winssl.log');
  SetGlobalErrorHandler(LErrorHandler);
  EnableErrorLogging(True);

  // 记录关键操作
  LogError(sslErrorInfo, 0, '应用程序启动', 'Main');

  try
    if LConn.Connect then
      LogError(sslErrorInfo, 0, '连接成功', 'Connection.Connect')
    else
      LogError(sslErrorError, GetLastError,
        '连接失败', 'Connection.Connect');
  except
    on E: Exception do
      LogError(sslErrorFatal, 0, E.Message, 'Connection.Connect');
  end;
end;
```

### 2. 实施健康检查

```pascal
type
  TSSLHealthChecker = class
  public
    function CheckConnection(const AHost: string; APort: Word): Boolean;
    function CheckCertificate(ACert: ISSLCertificate): Boolean;
    function CheckSessionCache: Boolean;
    function GetHealthReport: string;
  end;

function TSSLHealthChecker.CheckConnection(const AHost: string;
  APort: Word): Boolean;
var
  LConn: ISSLConnection;
  LStartTime: TDateTime;
begin
  Result := False;
  LStartTime := Now;

  try
    LConn := LContext.CreateConnection(CreateSocket(AHost, APort));
    try
      if LConn.Connect then
      begin
        Result := True;
        WriteLn('健康检查: 连接成功 (',
          MilliSecondsBetween(Now, LStartTime), ' ms)');
      end;
    finally
      LConn.Shutdown;
    end;
  except
    on E: Exception do
      WriteLn('健康检查失败: ', E.Message);
  end;
end;
```

### 3. 监控性能指标

```pascal
type
  TSSLMetrics = class
  private
    FTotalConnections: Int64;
    FSuccessfulConnections: Int64;
    FFailedConnections: Int64;
    FSessionReuses: Int64;
    FTotalHandshakeTime: Int64;
    FTotalDataSent: Int64;
    FTotalDataReceived: Int64;

  public
    procedure RecordConnection(ASuccess: Boolean;
      AHandshakeTime: Int64; ASessionReused: Boolean);
    procedure RecordDataTransfer(ASent, AReceived: Int64);
    function GetReport: string;
  end;

procedure TSSLMetrics.RecordConnection(ASuccess: Boolean;
  AHandshakeTime: Int64; ASessionReused: Boolean);
begin
  Inc(FTotalConnections);
  if ASuccess then
    Inc(FSuccessfulConnections)
  else
    Inc(FFailedConnections);

  if ASessionReused then
    Inc(FSessionReuses);

  Inc(FTotalHandshakeTime, AHandshakeTime);
end;

function TSSLMetrics.GetReport: string;
var
  LAvgHandshakeTime: Int64;
  LSuccessRate: Double;
  LSessionReuseRate: Double;
begin
  if FTotalConnections > 0 then
  begin
    LAvgHandshakeTime := FTotalHandshakeTime div FTotalConnections;
    LSuccessRate := (FSuccessfulConnections / FTotalConnections) * 100;
    LSessionReuseRate := (FSessionReuses / FTotalConnections) * 100;
  end
  else
  begin
    LAvgHandshakeTime := 0;
    LSuccessRate := 0;
    LSessionReuseRate := 0;
  end;

  Result := Format(
    '=== SSL 性能指标 ===' + #13#10 +
    '总连接数: %d' + #13#10 +
    '成功连接: %d (%.1f%%)' + #13#10 +
    '失败连接: %d' + #13#10 +
    'Session 复用: %d (%.1f%%)' + #13#10 +
    '平均握手时间: %d ms' + #13#10 +
    '总发送: %d bytes' + #13#10 +
    '总接收: %d bytes',
    [FTotalConnections,
     FSuccessfulConnections, LSuccessRate,
     FFailedConnections,
     FSessionReuses, LSessionReuseRate,
     LAvgHandshakeTime,
     FTotalDataSent,
     FTotalDataReceived]
  );
end;
```

### 4. 实施优雅关闭

```pascal
type
  TSSLApplication = class
  private
    FShutdownRequested: Boolean;
    FActiveConnections: TThreadList<ISSLConnection>;

  public
    procedure RequestShutdown;
    procedure WaitForShutdown;
  end;

procedure TSSLApplication.RequestShutdown;
var
  LList: TList<ISSLConnection>;
  LConn: ISSLConnection;
begin
  FShutdownRequested := True;

  // 关闭所有活动连接
  LList := FActiveConnections.LockList;
  try
    for LConn in LList do
    begin
      try
        LConn.Shutdown;
      except
        on E: Exception do
          WriteLn('关闭连接失败: ', E.Message);
      end;
    end;
    LList.Clear;
  finally
    FActiveConnections.UnlockList;
  end;
end;

procedure TSSLApplication.WaitForShutdown;
var
  LTimeout: Integer;
begin
  LTimeout := 0;
  while (FActiveConnections.LockList.Count > 0) and (LTimeout < 30) do
  begin
    FActiveConnections.UnlockList;
    Sleep(1000);
    Inc(LTimeout);
  end;

  if FActiveConnections.LockList.Count > 0 then
    WriteLn('警告: ', FActiveConnections.LockList.Count, ' 个连接未正常关闭');
  FActiveConnections.UnlockList;
end;
```

### 5. 配置证书自动续期

```pascal
type
  TCertificateRenewalManager = class
  private
    FCertPath: string;
    FKeyPath: string;
    FRenewalDays: Integer;  // 提前多少天续期

  public
    constructor Create(const ACertPath, AKeyPath: string;
      ARenewalDays: Integer = 30);

    function NeedsRenewal: Boolean;
    procedure RenewCertificate;
    procedure CheckAndRenew;
  end;

function TCertificateRenewalManager.NeedsRenewal: Boolean;
var
  LCert: ISSLCertificate;
  LDaysUntilExpiry: Integer;
begin
  LCert := LLib.CreateCertificate;
  if LCert.LoadFromFile(FCertPath) then
  begin
    LDaysUntilExpiry := DaysBetween(LCert.GetNotAfter, Now);
    Result := LDaysUntilExpiry <= FRenewalDays;
  end
  else
    Result := True;  // 证书加载失败，需要续期
end;

procedure TCertificateRenewalManager.CheckAndRenew;
begin
  if NeedsRenewal then
  begin
    WriteLn('证书即将过期，开始续期...');
    try
      RenewCertificate;
      WriteLn('证书续期成功');
    except
      on E: Exception do
      begin
        WriteLn('证书续期失败: ', E.Message);
        // 发送告警通知
      end;
    end;
  end;
end;
```

---

## 检查清单

### 安全检查清单
- [ ] 启用证书验证
- [ ] 使用强密码套件
- [ ] 使用 TLS 1.2+
- [ ] 验证主机名
- [ ] 保护私钥
- [ ] 实施证书固定（高安全场景）

### 性能检查清单
- [ ] 复用 Context 对象
- [ ] 启用 Session 复用
- [ ] 使用合适的缓冲区大小
- [ ] 批量读写数据
- [ ] 使用连接池
- [ ] 监控性能指标

### 错误处理检查清单
- [ ] 使用结构化错误处理
- [ ] 记录详细的错误上下文
- [ ] 实现重试逻辑
- [ ] 处理所有错误场景
- [ ] 提供友好的错误消息

### 资源管理检查清单
- [ ] 正确释放连接
- [ ] 管理 Session 缓存
- [ ] 使用连接池
- [ ] 监控内存使用
- [ ] 实施优雅关闭

### 测试检查清单
- [ ] 使用本地测试服务器
- [ ] 测试错误场景
- [ ] 使用 Mock 对象
- [ ] 测试覆盖率 > 80%
- [ ] 性能基准测试

### 生产部署检查清单
- [ ] 配置日志记录
- [ ] 实施健康检查
- [ ] 监控性能指标
- [ ] 实施优雅关闭
- [ ] 配置证书自动续期
- [ ] 设置告警通知

---

**持续更新中** - 如有最佳实践建议，请提交 Issue。
