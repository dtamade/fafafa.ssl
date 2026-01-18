# WinSSL 性能调优指南

> **版本**: v1.0
> **最后更新**: 2026-01-18

本指南帮助你优化 fafafa.ssl WinSSL 后端的性能，实现最佳的 TLS/SSL 连接速度和资源利用率。

## 目录

- [Session 复用优化](#session-复用优化)
- [连接池管理](#连接池管理)
- [缓冲区优化](#缓冲区优化)
- [密码套件选择](#密码套件选择)
- [证书优化](#证书优化)
- [内存管理](#内存管理)
- [性能监控](#性能监控)

---

## Session 复用优化

### 为什么重要

TLS 握手是连接建立中最耗时的部分。Session 复用可以：
- **减少握手时间 70-90%**
- **降低 CPU 使用率**
- **减少网络往返次数**

### 实现方式

**基本 Session 复用**:
```pascal
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LConn1, LConn2: ISSLConnection;
  LSession: ISSLSession;
begin
  LLib := CreateWinSSLLibrary;
  LLib.Initialize;

  // 创建 Context（复用凭据句柄）
  LContext := LLib.CreateContext(sslCtxClient);

  // 第一次连接 - 完整握手
  LConn1 := LContext.CreateConnection(Socket1);
  if LConn1.Connect then
  begin
    WriteLn('第一次连接: ', MilliSecondsBetween(Now, LStartTime), ' ms');

    // 获取 Session
    LSession := LConn1.GetSession;
    LConn1.Shutdown;
  end;

  // 第二次连接 - 复用 Session（快速握手）
  LConn2 := LContext.CreateConnection(Socket2);
  LConn2.SetSession(LSession);  // 设置 Session
  if LConn2.Connect then
  begin
    WriteLn('第二次连接: ', MilliSecondsBetween(Now, LStartTime), ' ms');
    WriteLn('Session 复用: ', LConn2.IsSessionResumed);
    LConn2.Shutdown;
  end;
end;
```

**Session 缓存管理器**:
```pascal
type
  TSessionCache = class
  private
    FCache: TDictionary<string, ISSLSession>;
    FMaxAge: Integer;  // 秒

  public
    constructor Create(AMaxAge: Integer = 36000);  // 默认 10 小时
    destructor Destroy; override;

    procedure Add(const AHost: string; ASession: ISSLSession);
    function Get(const AHost: string): ISSLSession;
    procedure CleanExpired;
  end;

// 使用示例
var
  LCache: TSessionCache;
  LSession: ISSLSession;
begin
  LCache := TSessionCache.Create(36000);  // 10 小时
  try
    // 首次连接
    if LConn1.Connect then
      LCache.Add('example.com', LConn1.GetSession);

    // 后续连接 - 从缓存获取
    LSession := LCache.Get('example.com');
    if Assigned(LSession) and LSession.IsValid then
    begin
      LConn2.SetSession(LSession);
      LConn2.Connect;  // 快速握手
    end;

    // 定期清理过期 Session
    LCache.CleanExpired;
  finally
    LCache.Free;
  end;
end;
```

### 性能对比

| 场景 | 首次连接 | Session 复用 | 性能提升 |
|------|---------|-------------|---------|
| 本地网络 | 50-100ms | 10-20ms | 70-80% |
| 互联网 | 200-500ms | 30-80ms | 80-90% |
| 高延迟 | 1000ms+ | 100-200ms | 85-90% |

---

## 连接池管理

### 为什么需要连接池

- **减少连接建立开销**
- **复用 TCP 连接和 TLS Session**
- **控制并发连接数**

### 实现连接池

```pascal
type
  TSSLConnectionPool = class
  private
    FContext: ISSLContext;
    FAvailable: TThreadList<ISSLConnection>;
    FInUse: TThreadList<ISSLConnection>;
    FMaxSize: Integer;
    FHost: string;
    FPort: Word;

  public
    constructor Create(AContext: ISSLContext; const AHost: string;
      APort: Word; AMaxSize: Integer = 10);
    destructor Destroy; override;

    function Acquire: ISSLConnection;
    procedure Release(AConn: ISSLConnection);
    procedure CloseAll;
  end;

// 使用示例
var
  LPool: TSSLConnectionPool;
  LConn: ISSLConnection;
begin
  LPool := TSSLConnectionPool.Create(LContext, 'api.example.com', 443, 10);
  try
    // 从池中获取连接
    LConn := LPool.Acquire;
    try
      if LConn.IsConnected or LConn.Connect then
      begin
        LConn.WriteString('GET /api/data HTTP/1.1'#13#10#13#10);
        // 处理响应...
      end;
    finally
      LPool.Release(LConn);  // 归还到池中
    end;
  finally
    LPool.Free;
  end;
end;
```

### 连接池配置建议

| 场景 | 池大小 | 空闲超时 | 说明 |
|------|--------|---------|------|
| 低频请求 | 2-5 | 30秒 | 减少资源占用 |
| 中频请求 | 5-10 | 60秒 | 平衡性能和资源 |
| 高频请求 | 10-20 | 120秒 | 最大化性能 |
| 批量处理 | 20-50 | 300秒 | 大量并发 |

---

## 缓冲区优化

### 读写缓冲区大小

**默认缓冲区**:
```pascal
const
  DEFAULT_BUFFER_SIZE = 4096;  // 4 KB
```

**优化建议**:

1. **小数据传输**（< 4KB）:
```pascal
const BUFFER_SIZE = 2048;  // 2 KB
```

2. **中等数据传输**（4KB - 64KB）:
```pascal
const BUFFER_SIZE = 8192;  // 8 KB（推荐）
```

3. **大数据传输**（> 64KB）:
```pascal
const BUFFER_SIZE = 65536;  // 64 KB
```

4. **文件传输**:
```pascal
const BUFFER_SIZE = 131072;  // 128 KB
```

### 批量读写

**低效方式**:
```pascal
// ❌ 每次读取 1 字节
for i := 1 to FileSize do
  LConn.Read(LByte, 1);
```

**高效方式**:
```pascal
// ✓ 批量读取
var
  LBuffer: array[0..65535] of Byte;
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

## 密码套件选择

### 性能对比

| 密码套件 | 握手速度 | 加密速度 | 硬件加速 | 推荐 |
|---------|---------|---------|---------|------|
| TLS_AES_128_GCM_SHA256 | 快 | 非常快 | ✅ | ⭐⭐⭐⭐⭐ |
| TLS_AES_256_GCM_SHA384 | 快 | 快 | ✅ | ⭐⭐⭐⭐ |
| TLS_CHACHA20_POLY1305_SHA256 | 快 | 快 | ❌ | ⭐⭐⭐ |
| ECDHE-RSA-AES128-GCM-SHA256 | 中等 | 快 | ✅ | ⭐⭐⭐⭐ |
| ECDHE-RSA-AES256-GCM-SHA384 | 中等 | 快 | ✅ | ⭐⭐⭐ |

### 配置建议

**高性能配置**（优先 AES-GCM）:
```pascal
LContext.SetCipherSuites('TLS_AES_128_GCM_SHA256:TLS_AES_256_GCM_SHA384');
```

**平衡配置**（兼容性 + 性能）:
```pascal
LContext.SetCipherSuites(
  'TLS_AES_128_GCM_SHA256:' +
  'TLS_AES_256_GCM_SHA384:' +
  'ECDHE-RSA-AES128-GCM-SHA256:' +
  'ECDHE-RSA-AES256-GCM-SHA384'
);
```

**移动设备配置**（ChaCha20 优先）:
```pascal
// 移动设备通常没有 AES 硬件加速
LContext.SetCipherSuites(
  'TLS_CHACHA20_POLY1305_SHA256:' +
  'TLS_AES_128_GCM_SHA256'
);
```

---

## 证书优化

### 证书链长度

**影响**:
- 每增加一个中间证书，握手时间增加 10-50ms
- 证书链越长，验证时间越长

**优化建议**:
```pascal
// ❌ 避免过长的证书链
// Root CA -> Intermediate CA 1 -> Intermediate CA 2 -> Leaf Cert (4 级)

// ✓ 推荐证书链
// Root CA -> Intermediate CA -> Leaf Cert (3 级)
```

### 证书类型选择

| 证书类型 | 密钥大小 | 生成时间 | 握手时间 | 推荐 |
|---------|---------|---------|---------|------|
| RSA 2048 | 2048 bit | 100-500ms | 50-100ms | ⭐⭐⭐ |
| RSA 4096 | 4096 bit | 1-3秒 | 100-200ms | ⭐⭐ |
| ECDSA P-256 | 256 bit | 10-50ms | 20-40ms | ⭐⭐⭐⭐⭐ |
| ECDSA P-384 | 384 bit | 20-80ms | 30-60ms | ⭐⭐⭐⭐ |

**推荐配置**:
```pascal
// 使用 ECDSA P-256 证书（最佳性能）
LOptions.KeyType := ktECDSA;
LOptions.ECCurve := 'prime256v1';  // = secp256r1
```

### 证书缓存

```pascal
type
  TCertificateCache = class
  private
    FCache: TDictionary<string, ISSLCertificate>;

  public
    function GetOrLoad(const APath: string): ISSLCertificate;
  end;

function TCertificateCache.GetOrLoad(const APath: string): ISSLCertificate;
begin
  if not FCache.TryGetValue(APath, Result) then
  begin
    Result := LLib.CreateCertificate;
    Result.LoadFromFile(APath);
    FCache.Add(APath, Result);
  end;
end;
```

---

## 内存管理

### 避免内存泄漏

**常见泄漏场景**:

1. **未释放 Session 对象**:
```pascal
// ❌ 错误
for i := 1 to 1000 do
begin
  LConn := LContext.CreateConnection(Socket);
  LConn.Connect;
  // 未调用 Shutdown，Session 未释放
end;

// ✓ 正确
for i := 1 to 1000 do
begin
  LConn := LContext.CreateConnection(Socket);
  try
    if LConn.Connect then
    begin
      // 执行操作...
    end;
  finally
    LConn.Shutdown;  // 正确释放
  end;
end;
```

2. **Session 缓存未清理**:
```pascal
// ✓ 定期清理过期 Session
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
```

### 内存使用监控

```pascal
procedure MonitorMemoryUsage;
var
  LMemStatus: TMemoryManagerState;
  LSmallBlockStatus: TSmallBlockTypeState;
begin
  GetMemoryManagerState(LMemStatus);

  WriteLn('总分配内存: ', LMemStatus.TotalAllocatedMediumBlockSize div 1024, ' KB');
  WriteLn('总可用内存: ', LMemStatus.TotalAvailableMediumBlockSize div 1024, ' KB');

  // 检查小块内存使用
  for var i := 0 to High(LMemStatus.SmallBlockTypeStates) do
  begin
    LSmallBlockStatus := LMemStatus.SmallBlockTypeStates[i];
    if LSmallBlockStatus.AllocatedBlockCount > 0 then
      WriteLn(Format('块大小 %d: %d 个已分配',
        [LSmallBlockStatus.BlockSize, LSmallBlockStatus.AllocatedBlockCount]));
  end;
end;
```

---

## 性能监控

### 连接性能指标

```pascal
type
  TConnectionMetrics = record
    ConnectTime: Int64;      // 连接建立时间（ms）
    HandshakeTime: Int64;    // TLS 握手时间（ms）
    FirstByteTime: Int64;    // 首字节时间（ms）
    TotalTime: Int64;        // 总时间（ms）
    BytesSent: Int64;        // 发送字节数
    BytesReceived: Int64;    // 接收字节数
    SessionReused: Boolean;  // Session 是否复用
  end;

function MeasureConnection(AConn: ISSLConnection): TConnectionMetrics;
var
  LStart, LConnected, LHandshake, LFirstByte: TDateTime;
begin
  LStart := Now;

  // 连接
  if AConn.Connect then
  begin
    LConnected := Now;
    Result.ConnectTime := MilliSecondsBetween(LConnected, LStart);

    // 握手
    LHandshake := Now;
    Result.HandshakeTime := MilliSecondsBetween(LHandshake, LConnected);

    // 发送请求
    AConn.WriteString('GET / HTTP/1.1'#13#10#13#10);

    // 接收首字节
    var LData: string;
    if AConn.ReadString(LData) then
    begin
      LFirstByte := Now;
      Result.FirstByteTime := MilliSecondsBetween(LFirstByte, LHandshake);
    end;

    Result.TotalTime := MilliSecondsBetween(Now, LStart);
    Result.SessionReused := AConn.IsSessionResumed;
  end;
end;
```

### 性能日志

```pascal
procedure LogPerformanceMetrics(const AMetrics: TConnectionMetrics);
begin
  WriteLn('=== 连接性能指标 ===');
  WriteLn('连接时间: ', AMetrics.ConnectTime, ' ms');
  WriteLn('握手时间: ', AMetrics.HandshakeTime, ' ms');
  WriteLn('首字节时间: ', AMetrics.FirstByteTime, ' ms');
  WriteLn('总时间: ', AMetrics.TotalTime, ' ms');
  WriteLn('Session 复用: ', BoolToStr(AMetrics.SessionReused, True));
  WriteLn('发送: ', AMetrics.BytesSent, ' bytes');
  WriteLn('接收: ', AMetrics.BytesReceived, ' bytes');
  WriteLn('');
end;
```

---

## 性能优化检查清单

### 连接优化
- [ ] 启用 Session 复用
- [ ] 使用连接池
- [ ] 复用 Context 对象
- [ ] 配置合适的超时时间

### 数据传输优化
- [ ] 使用合适的缓冲区大小
- [ ] 批量读写数据
- [ ] 避免频繁的小数据传输

### 密码套件优化
- [ ] 优先使用 AES-GCM（硬件加速）
- [ ] 移动设备使用 ChaCha20
- [ ] 禁用弱密码套件

### 证书优化
- [ ] 使用 ECDSA 证书
- [ ] 减少证书链长度
- [ ] 缓存证书对象

### 内存优化
- [ ] 正确释放连接对象
- [ ] 定期清理过期 Session
- [ ] 监控内存使用

### 监控和诊断
- [ ] 记录性能指标
- [ ] 监控 Session 复用率
- [ ] 分析慢连接原因

---

## 性能基准测试

### 测试场景

```pascal
program winssl_performance_benchmark;

procedure BenchmarkSessionReuse;
var
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LSession: ISSLSession;
  LMetrics: array[0..99] of TConnectionMetrics;
  LAvgWithSession, LAvgWithoutSession: Int64;
begin
  WriteLn('=== Session 复用性能测试 ===');

  LContext := CreateWinSSLLibrary.CreateContext(sslCtxClient);

  // 测试 1: 无 Session 复用
  WriteLn('测试 1: 无 Session 复用（100 次连接）');
  for var i := 0 to 99 do
  begin
    LConn := LContext.CreateConnection(CreateSocket('example.com', 443));
    LMetrics[i] := MeasureConnection(LConn);
    LConn.Shutdown;
  end;

  LAvgWithoutSession := 0;
  for var i := 0 to 99 do
    Inc(LAvgWithoutSession, LMetrics[i].TotalTime);
  LAvgWithoutSession := LAvgWithoutSession div 100;

  WriteLn('平均连接时间: ', LAvgWithoutSession, ' ms');

  // 测试 2: 有 Session 复用
  WriteLn('');
  WriteLn('测试 2: 有 Session 复用（100 次连接）');

  // 首次连接获取 Session
  LConn := LContext.CreateConnection(CreateSocket('example.com', 443));
  LConn.Connect;
  LSession := LConn.GetSession;
  LConn.Shutdown;

  for var i := 0 to 99 do
  begin
    LConn := LContext.CreateConnection(CreateSocket('example.com', 443));
    LConn.SetSession(LSession);
    LMetrics[i] := MeasureConnection(LConn);
    LConn.Shutdown;
  end;

  LAvgWithSession := 0;
  for var i := 0 to 99 do
    Inc(LAvgWithSession, LMetrics[i].TotalTime);
  LAvgWithSession := LAvgWithSession div 100;

  WriteLn('平均连接时间: ', LAvgWithSession, ' ms');
  WriteLn('');
  WriteLn('性能提升: ',
    Round((1 - LAvgWithSession / LAvgWithoutSession) * 100), '%');
end;

begin
  BenchmarkSessionReuse;
end.
```

---

## 故障排查

### 性能问题诊断

**问题**: Session 复用不工作

**检查**:
```pascal
if not LConn.IsSessionResumed then
begin
  WriteLn('Session 未复用，可能原因：');
  WriteLn('  - 服务器不支持 Session 复用');
  WriteLn('  - Session 已过期（默认 10 小时）');
  WriteLn('  - 服务器要求重新验证');
  WriteLn('  - 未使用相同的 Context 对象');
end;
```

**问题**: 连接速度慢

**诊断步骤**:
1. 测量各阶段时间
2. 检查 Session 复用率
3. 检查密码套件
4. 检查证书链长度
5. 检查网络延迟

---

**持续更新中** - 如有性能优化建议，请提交 Issue。
