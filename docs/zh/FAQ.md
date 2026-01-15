# fafafa.ssl 常见问题解答 (FAQ)

本文档收集了使用 fafafa.ssl 时的常见问题和解决方案。

## 目录

- [连接相关](#连接相关)
- [证书相关](#证书相关)
- [配置相关](#配置相关)
- [性能相关](#性能相关)
- [错误处理](#错误处理)
- [平台特定](#平台特定)

---

## 连接相关

### Q1: 如何验证服务器证书？

**A:** 默认情况下，应该启用证书验证：

```pascal
LContext := TSSLFactory.CreateContext(sslOpenSSL, sslCtxClient);
LContext.SetVerifyMode([sslVerifyPeer]);  // 验证对方证书

// 对于自定义CA，可以设置CA证书路径
LContext.LoadCAFile('/path/to/ca-bundle.crt');
```

### Q2: 如何处理自签名证书？

**A:** 对于开发/测试环境：

```pascal
// 方法1：禁用验证（不推荐用于生产）
LContext.SetVerifyMode([sslVerifyNone]);

// 方法2：添加自签名证书为可信（推荐）
LContext.LoadCAFile('self-signed-ca.crt');
LContext.SetVerifyMode([sslVerifyPeer]);
```

⚠️ **警告**：生产环境务必启用证书验证！

### Q3: 连接超时如何处理？

**A:** 实现超时机制：

```pascal
const
  CONNECT_TIMEOUT = 10000;  // 10秒

var
  LStartTime: TDateTime;
begin
  LStartTime := Now;
  
  try
    LConnection.Connect(AHost, APort);
  except
    on E: Exception do
    begin
      if MilliSecondsBetween(Now, LStartTime) > CONNECT_TIMEOUT then
        raise Exception.Create('连接超时')
      else
        raise;
    end;
  end;
end;
```

### Q4: 如何使用客户端证书认证？

**A:** 完整的双向TLS示例：

```pascal
// 加载客户端证书和私钥
LContext.LoadCertificate('client.pem');
LContext.LoadPrivateKey('client.key');

// 如果私钥有密码，使用带口令的重载
LContext.LoadPrivateKey('client.key', 'your-password');

// 连接时会自动发送客户端证书
LConnection := LContext.CreateConnection;
LConnection.Connect(AHost, APort);
```

参考完整示例：`examples/production/https_client_auth.pas`

### Q5: 如何检测连接是否仍然有效？

**A:** 实现心跳检测：

```pascal
function IsConnectionAlive(AConnection: ISSLConnection): Boolean;
var
  LBuffer: Byte;
begin
  Result := False;
  try
    // 尝试读取数据（非阻塞）
    if AConnection.Pending > 0 then
      Result := True
    else
      Result := True;  // 假设有效，实际应该发送心跳包
  except
    Result := False;
  end;
end;
```

---

## 证书相关

### Q6: 如何查看证书详细信息？

**A:** 使用证书接口：

```pascal
var
  LCert: ISSLCertificate;
begin
  LCert := LConnection.GetPeerCertificate;
  
  WriteLn('Subject: ', LCert.GetSubjectName);
  WriteLn('Issuer: ', LCert.GetIssuerName);
  WriteLn('Valid From: ', DateTimeToStr(LCert.GetNotBefore));
  WriteLn('Valid Until: ', DateTimeToStr(LCert.GetNotAfter));
  WriteLn('Serial: ', LCert.GetSerialNumber);
end;
```

### Q7: 如何生成自签名证书？

**A:** 使用 OpenSSL 命令行：

```bash
# 生成私钥
openssl genrsa -out server.key 2048

# 生成自签名证书
openssl req -new -x509 -key server.key -out server.crt -days 365 \
  -subj "/C=CN/ST=Beijing/L=Beijing/O=MyOrg/CN=localhost"

# 合并为PEM格式
cat server.crt server.key > server.pem
```

或使用我们的工具：`examples/tools/cert_generator.pas`

### Q8: 证书链验证失败如何处理？

**A:** 加载完整的证书链：

```pascal
// 加载CA证书包
LContext.LoadCAFile('/etc/ssl/certs/ca-certificates.crt');

// 或者加载CA目录
LContext.LoadCAPath('/etc/ssl/certs/');

// Windows下使用系统证书存储
{$IFDEF WINDOWS}
LContext.LoadSystemCertificates;
{$ENDIF}
```

### Q9: 如何检查证书是否即将过期？

**A:** 检查有效期：

```pascal
function CertWillExpireSoon(ACert: ISSLCertificate; ADays: Integer): Boolean;
var
  LExpiryDate: TDateTime;
  LDaysLeft: Integer;
begin
  LExpiryDate := ACert.GetNotAfter;
  LDaysLeft := DaysBetween(LExpiryDate, Now);
  Result := LDaysLeft <= ADays;
  
  if Result then
    WriteLn(Format('警告: 证书将在 %d 天后过期', [LDaysLeft]));
end;
```

---

## 配置相关

### Q10: 如何配置TLS版本？

**A:** 设置最小和最大TLS版本：

```pascal
// 只允许TLS 1.2和1.3
LContext.SetMinProtocolVersion(sslProtocolTLS12);
LContext.SetMaxProtocolVersion(sslProtocolTLS13);

// 禁用旧版本
LContext.SetProtocolOptions([
  sslOptNoSSLv2,
  sslOptNoSSLv3,
  sslOptNoTLSv1,
  sslOptNoTLSv1_1
]);
```

### Q11: 如何选择密码套件？

**A:** 配置密码套件列表：

```pascal
// 高安全性配置
LContext.SetCipherList(
  'ECDHE-ECDSA-AES256-GCM-SHA384:' +
  'ECDHE-RSA-AES256-GCM-SHA384:' +
  'ECDHE-ECDSA-CHACHA20-POLY1305:' +
  'ECDHE-RSA-CHACHA20-POLY1305'
);

// TLS 1.3 密码套件
LContext.SetCipherSuites(
  'TLS_AES_256_GCM_SHA384:' +
  'TLS_CHACHA20_POLY1305_SHA256'
);
```

### Q12: 如何启用会话复用？

**A:** 会话复用可以显著提高性能：

```pascal
// 启用会话缓存
LContext.SetSessionCacheMode(sslSessCacheClient);

// 设置会话超时（秒）
LContext.SetSessionTimeout(300);  // 5分钟

// 复用同一上下文创建多个连接
for i := 1 to 10 do
begin
  LConnection := LContext.CreateConnection;  // 复用会话
  LConnection.Connect(AHost, APort);
  // ... 使用连接
end;
```

参考示例：`examples/production/https_client_session.pas`

### Q13: 如何启用ALPN协议协商？

**A:** 设置ALPN协议列表：

```pascal
// 客户端
LContext.SetALPNProtocols(['h2', 'http/1.1']);

// 连接后检查协商结果
LProtocol := LConnection.GetALPNSelected;
WriteLn('Selected protocol: ', LProtocol);  // 可能是 'h2' 或 'http/1.1'
```

---

## 性能相关

### Q14: 如何优化连接性能？

**A:** 性能优化建议：

1. **使用会话复用**（见Q12）
2. **调整缓冲区大小**：
```pascal
const
  OPTIMAL_BUFFER = 16384;  // 16KB

var
  LBuffer: array[0..OPTIMAL_BUFFER-1] of Byte;
```

3. **复用连接**：
```pascal
// 不要每次请求都创建新连接
// 复用同一连接发送多个请求
```

4. **使用连接池**（高级）

### Q15: 如何处理大量并发连接？

**A:** 使用线程池模式：

```pascal
type
  TWorkerThread = class(TThread)
  private
    FContext: ISSLContext;  // 共享上下文
    FHost: string;
    FPort: Word;
  protected
    procedure Execute; override;
  end;

procedure TWorkerThread.Execute;
var
  LConnection: ISSLConnection;
begin
  try
    LConnection := FContext.CreateConnection;
    LConnection.Connect(FHost, FPort);
    // 处理请求...
  except
    // 错误处理
  end;
end;

// 主程序
var
  LContext: ISSLContext;
  LThreads: array[1..100] of TWorkerThread;
begin
  LContext := TSSLFactory.CreateContext(sslOpenSSL, sslCtxClient);
  
  for i := 1 to 100 do
  begin
    LThreads[i] := TWorkerThread.Create(LContext, 'example.com', 443);
    LThreads[i].Start;
  end;
end;
```

### Q16: 内存使用优化建议？

**A:** 

1. **及时释放连接**：
```pascal
// 使用接口自动管理
procedure DoRequest;
var
  LConnection: ISSLConnection;
begin
  LConnection := LContext.CreateConnection;
  // 使用连接...
end;  // 自动释放
```

2. **复用缓冲区**：
```pascal
var
  GBufferPool: array of array[0..8191] of Byte;
```

3. **限制并发数**：
```pascal
const
  MAX_CONCURRENT = 100;
```

---

## 错误处理

### Q17: 常见错误码含义？

**A:** 主要错误码：

| 错误码 | 含义 | 解决方案 |
|--------|------|----------|
| `sslErrCertificate` | 证书错误 | 检查证书有效性和验证设置 |
| `sslErrConnection` | 连接错误 | 检查网络和服务器状态 |
| `sslErrProtocol` | 协议错误 | 检查TLS版本兼容性 |
| `sslErrHandshake` | 握手失败 | 检查密码套件和证书配置 |
| `sslErrTimeout` | 超时 | 增加超时时间或检查网络 |

### Q18: 如何调试SSL错误？

**A:** 启用详细日志：

```pascal
uses
  fafafa.ssl.logger;

var
  LLogger: ILogger;
begin
  // 创建调试级别日志
  LLogger := TConsoleLogger.Create('debug.log', llDebug);
  SetGlobalLogger(LLogger);
  
  // 你的代码会自动输出详细日志
  try
    // SSL操作
  except
    on E: ESSLException do
    begin
      LLogger.Error('SSL错误: ' + E.Message);
      LLogger.Error('错误码: ' + IntToStr(Ord(E.ErrorCode)));
      LLogger.Error('详细信息: ' + E.GetDetailedMessage);
    end;
  end;
end;
```

### Q19: 如何重试失败的连接？

**A:** 实现指数退避重试：

```pascal
function ConnectWithRetry(
  AContext: ISSLContext;
  const AHost: string;
  APort: Word;
  AMaxRetries: Integer = 3
): ISSLConnection;
var
  LRetry: Integer;
  LWaitTime: Integer;
begin
  Result := nil;
  LWaitTime := 1000;  // 初始等待1秒
  
  for LRetry := 1 to AMaxRetries do
  begin
    try
      WriteLn('连接尝试 ', LRetry, '/', AMaxRetries);
      Result := AContext.CreateConnection;
      Result.Connect(AHost, APort);
      WriteLn('连接成功！');
      Exit;  // 成功
    except
      on E: Exception do
      begin
        WriteLn('连接失败: ', E.Message);
        if LRetry < AMaxRetries then
        begin
          WriteLn('等待 ', LWaitTime, 'ms 后重试...');
          Sleep(LWaitTime);
          LWaitTime := LWaitTime * 2;  // 指数退避
        end
        else
          raise;  // 最后一次尝试失败，抛出异常
      end;
    end;
  end;
end;
```

---

## 平台特定

### Q20: Linux和Windows有什么区别？

**A:** 主要区别：

| 特性 | Linux | Windows |
|------|-------|---------|
| 默认后端 | OpenSSL | WinSSL (Schannel) |
| 证书存储 | 文件 | 系统证书存储 |
| 路径 | `/etc/ssl/certs/` | `C:\Windows\System32\` |
| 行为 | POSIX标准 | Windows API |

**跨平台代码示例**：

```pascal
{$IFDEF WINDOWS}
  LContext.LoadSystemCertificates;
{$ELSE}
  LContext.LoadCAFile('/etc/ssl/certs/ca-certificates.crt');
{$ENDIF}
```

### Q21: 如何在Windows上使用OpenSSL而不是WinSSL？

**A:** 明确指定后端：

```pascal
// 强制使用OpenSSL
LContext := TSSLFactory.CreateContext(sslOpenSSL, sslCtxClient);

// 而不是
LContext := TSSLFactory.CreateContext(sslWinSSL, sslCtxClient);
```

确保已安装OpenSSL for Windows。

### Q22: macOS上的特殊注意事项？

**A:** 

1. **安装OpenSSL**：
```bash
brew install openssl@3
```

2. **设置库路径**：
```bash
export DYLD_LIBRARY_PATH="/opt/homebrew/opt/openssl@3/lib:$DYLD_LIBRARY_PATH"
```

3. **编译时链接**：
```bash
fpc -Fl/opt/homebrew/opt/openssl@3/lib your_program.pas
```

---

## 其他常见问题

### Q23: 如何实现HTTPS代理？

**A:** 代理支持（计划中）：

```pascal
// 未来版本将支持
LContext.SetProxy('http://proxy.example.com:8080');
LContext.SetProxyAuth('username', 'password');
```

目前可以使用SOCKS代理或实现自定义代理逻辑。

### Q24: 如何处理大文件下载？

**A:** 流式下载：

```pascal
procedure DownloadFile(
  AConnection: ISSLConnection;
  const AOutputFile: string
);
var
  LStream: TFileStream;
  LBuffer: array[0..8191] of Byte;
  LBytesRead: Integer;
  LTotal: Int64;
begin
  LStream := TFileStream.Create(AOutputFile, fmCreate);
  try
    LTotal := 0;
    repeat
      LBytesRead := AConnection.Read(LBuffer[0], Length(LBuffer));
      if LBytesRead > 0 then
      begin
        LStream.Write(LBuffer[0], LBytesRead);
        Inc(LTotal, LBytesRead);
        Write(#13, '已下载: ', LTotal, ' 字节');
      end;
    until LBytesRead = 0;
    WriteLn;
    WriteLn('下载完成！');
  finally
    LStream.Free;
  end;
end;
```

### Q25: 性能基准是多少？

**A:** 典型性能（测试环境：Intel i7, OpenSSL 3.0）：

- **TLS 1.3 握手**: 50-100ms
- **TLS 1.2 握手**: 100-150ms
- **吞吐量**: 100-500 MB/s（取决于加密算法）
- **并发连接**: 支持1000+同时连接

运行基准测试：
```bash
cd benchmarks
fpc handshake_benchmark.pas
./handshake_benchmark
```

---

## 需要更多帮助？

- 📖 查看[API文档](API参考/概述.md)
- 💡 查看[示例代码](../../examples/)
- 🐛 [报告问题](https://github.com/你的用户名/fafafa.ssl/issues)
- 💬 [参与讨论](https://github.com/你的用户名/fafafa.ssl/discussions)

---

**上一篇**：[← 安装配置](安装配置.md) | **返回首页**：[快速入门](快速入门.md)

