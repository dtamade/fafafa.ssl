# fafafa.ssl 用户指南

> **版本**: v0.8  
> **最后更新**: 2025-10-24

本指南详细介绍如何使用 fafafa.ssl 库构建安全的 SSL/TLS 应用程序。

## 目录

1. [安装与配置](#安装与配置)
2. [基础概念](#基础概念)
3. [常见场景](#常见场景)
4. [最佳实践](#最佳实践)
5. [性能优化](#性能优化)

---

## 安装与配置

### 系统要求
- **Free Pascal Compiler (FPC)** 3.2.0+
- **Lazarus IDE** 2.0+ (可选)
- **OpenSSL** 1.1.1+ 或 3.x (Windows/Linux/macOS)
- **Windows**: Windows 7+ (用于 WinSSL)

### 安装 OpenSSL

**Windows**:
```powershell
# 使用官方安装程序
# 下载: https://slproweb.com/products/Win32OpenSSL.html

# 或使用 Chocolatey
choco install openssl

# 或复制 DLL 到程序目录
# libcrypto-3-x64.dll
# libssl-3-x64.dll
```

**Linux (Ubuntu/Debian)**:
```bash
sudo apt update
sudo apt install libssl3 libssl-dev
```

**Linux (Fedora/RHEL)**:
```bash
sudo dnf install openssl openssl-devel
```

**macOS**:
```bash
brew install openssl@3
```

### 集成到项目

**方法 1: 直接引用源码**
```pascal
program myapp;

uses
  fafafa.ssl.openssl,
  fafafa.ssl.abstract.intf;

begin
  // 你的代码...
end.
```

**编译**:
```bash
fpc -Fusrc -Fusrc/openssl myapp.pas
```

**方法 2: 使用 Lazarus 包**
1. 打开 Lazarus IDE
2. `Package` → `Open Package File (.lpk)`
3. 选择 `fafafa_ssl.lpk`
4. 点击 `Compile`
5. 在项目依赖中添加 `fafafa_ssl`

---

## 基础概念

### SSL/TLS 工作流程

```
                  客户端                服务端
                    |                     |
1. 初始化库         | Initialize Library   |
2. 创建上下文       | Create Context       |
3. 配置证书/CA      | Load Cert & CA      | Load Cert & Key
4. 创建连接         | Create Connection    |
                    |                     |
5. 握手             |---- ClientHello ---->|
                    |<--- ServerHello -----|
                    |<--- Certificate -----|
                    |---- Finished ------->|
                    |<--- Finished --------|
                    |                     |
6. 数据传输         |<==== 加密数据 ======>|
                    |                     |
7. 关闭连接         |----- Shutdown ------>|
                    |<---- Shutdown -------|
```

### 核心组件

**ISSLLibrary**: 库管理器
- 初始化/清理资源
- 创建上下文、证书、连接
- 错误处理

**ISSLContext**: 连接配置
- 协议版本设置
- 证书和密钥加载
- 验证模式配置
- 密码套件选择

**ISSLCertificate**: 证书管理
- 加载/保存证书
- 证书信息提取
- 证书验证

**ISSLConnection**: 连接处理
- 握手执行
- 数据加密/解密传输
- 状态查询

---

## 常见场景

### 场景 1: HTTPS 客户端

```pascal
program https_client;

uses
  SysUtils, fafafa.ssl.openssl, fafafa.ssl.abstract.intf;

var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LResponse: string;
begin
  // 1. 初始化库
  LLib := CreateOpenSSLLibrary;
  if not LLib.Initialize then
    raise Exception.Create('Failed to initialize SSL');
  
  try
    // 2. 创建客户端上下文
    LContext := LLib.CreateContext(sslCtxClient);
    
    // 3. 配置 TLS 1.2+
    LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    
    // 4. 加载系统 CA 证书（自动）
    // LContext.LoadCAFile('/etc/ssl/certs/ca-bundle.crt'); // Linux
    // LContext.LoadCAFile('C:\Windows\System32\curl-ca-bundle.crt'); // Windows
    
    // 5. 启用证书验证
    LContext.SetVerifyMode([sslVerifyPeer]);
    
    // 6. 连接到服务器
    LConn := LContext.CreateConnection(ConnectToServer('example.com', 443));
    if LConn.Connect then
    begin
      // 7. 验证证书主机名
      if LConn.GetPeerCertificate.VerifyHostname('example.com') then
      begin
        // 8. 发送 HTTP 请求
        LConn.WriteString('GET / HTTP/1.1'#13#10 +
                          'Host: example.com'#13#10 +
                          'Connection: close'#13#10#13#10);
        
        // 9. 接收响应
        LResponse := LConn.ReadString;
        WriteLn('Response:', LResponse);
      end;
      
      // 10. 优雅关闭
      LConn.Shutdown;
    end;
  finally
    LLib.Finalize;
  end;
end.
```

### 场景 2: HTTPS 服务器

```pascal
program https_server;

uses
  SysUtils, fafafa.ssl.openssl, fafafa.ssl.abstract.intf;

var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LRequest: string;
begin
  LLib := CreateOpenSSLLibrary;
  LLib.Initialize;
  
  try
    // 创建服务端上下文
    LContext := LLib.CreateContext(sslCtxServer);
    
    // 加载服务器证书和私钥
    LContext.LoadCertificate('server.crt');
    LContext.LoadPrivateKey('server.key');
    
    // 可选：要求客户端证书
    // LContext.LoadCAFile('client-ca.crt');
    // LContext.SetVerifyMode([sslVerifyPeer, sslVerifyFailIfNoPeerCert]);
    
    // 接受客户端连接
    while True do
    begin
      LConn := LContext.CreateConnection(AcceptClient);
      if LConn.Accept then
      begin
        // 读取请求
        LRequest := LConn.ReadString;
        WriteLn('Request: ', LRequest);
        
        // 发送响应
        LConn.WriteString('HTTP/1.1 200 OK'#13#10 +
                          'Content-Type: text/plain'#13#10 +
                          'Connection: close'#13#10#13#10 +
                          'Hello, SSL!');
        
        LConn.Shutdown;
      end;
    end;
  finally
    LLib.Finalize;
  end;
end.
```

### 场景 3: 证书验证与管理

```pascal
program cert_verify;

uses
  SysUtils, fafafa.ssl.openssl, fafafa.ssl.abstract.intf;

var
  LLib: ISSLLibrary;
  LCert: ISSLCertificate;
  LStore: ISSLCertificateStore;
  LResult: TSSLCertVerifyResult;
  LAltNames: TStringList;
begin
  LLib := CreateOpenSSLLibrary;
  LLib.Initialize;
  
  try
    // 加载证书
    LCert := LLib.CreateCertificate;
    if not LCert.LoadFromFile('server.crt') then
      raise Exception.Create('Failed to load certificate');
    
    // 显示证书信息
    WriteLn('主题: ', LCert.GetSubject);
    WriteLn('颁发者: ', LCert.GetIssuer);
    WriteLn('序列号: ', LCert.GetSerialNumber);
    WriteLn('有效期: ', DateTimeToStr(LCert.GetNotBefore), ' - ', 
            DateTimeToStr(LCert.GetNotAfter));
    WriteLn('指纹 (SHA256): ', LCert.GetFingerprintSHA256);
    
    // 显示扩展
    LAltNames := LCert.GetSubjectAltNames;
    WriteLn('主题备用名称:');
    for var i := 0 to LAltNames.Count - 1 do
      WriteLn('  ', LAltNames[i]);
    LAltNames.Free;
    
    // 基础验证
    LStore := LLib.CreateCertificateStore;
    LStore.LoadFromFile('ca-bundle.crt');
    
    if LCert.Verify(LStore) then
      WriteLn('✓ 基础验证通过')
    else
      WriteLn('✗ 基础验证失败');
    
    // 增强验证（含吊销检查）
    if LCert.VerifyEx(LStore, [sslCertVerifyCheckRevocation, 
                                 sslCertVerifyCheckOCSP], LResult) then
    begin
      WriteLn('✓ 增强验证通过');
      WriteLn('  详细信息: ', LResult.DetailedInfo);
    end
    else
    begin
      WriteLn('✗ 增强验证失败');
      WriteLn('  错误代码: ', LResult.ErrorCode);
      WriteLn('  错误消息: ', LResult.ErrorMessage);
    end;
    
    // 检查状态
    WriteLn('是否过期: ', BoolToStr(LCert.IsExpired, True));
    WriteLn('是否自签名: ', BoolToStr(LCert.IsSelfSigned, True));
    WriteLn('是否 CA: ', BoolToStr(LCert.IsCA, True));
  finally
    LLib.Finalize;
  end;
end.
```

### 场景 4: WinSSL 企业功能

```pascal
program winssl_enterprise;

{$IFDEF WINDOWS}
uses
  SysUtils, fafafa.ssl.winssl.factory, fafafa.ssl.winssl.enterprise;

var
  LLib: ISSLLibrary;
  LConfig: TSSLEnterpriseConfig;
  LRoots: TStringList;
begin
  LLib := CreateWinSSLLibrary;
  LLib.Initialize;
  
  try
    // 创建企业配置
    LConfig := TSSLEnterpriseConfig.Create;
    try
      LConfig.LoadFromSystem;
      
      // 检测 FIPS 模式
      if LConfig.IsFipsModeEnabled then
        WriteLn('✓ FIPS 模式已启用')
      else
        WriteLn('ℹ FIPS 模式未启用');
      
      // 获取企业受信任根证书
      LRoots := LConfig.GetEnterpriseTrustedRoots;
      WriteLn('企业受信任根证书数量: ', LRoots.Count);
      for var i := 0 to LRoots.Count - 1 do
        WriteLn('  ', LRoots[i]);
      
      // 读取组策略
      var LPolicies := LConfig.GetGroupPolicies;
      WriteLn('组策略:');
      for var i := 0 to LPolicies.Count - 1 do
        WriteLn('  ', LPolicies[i]);
    finally
      LConfig.Free;
    end;
  finally
    LLib.Finalize;
  end;
end;
{$ENDIF}
```

---

## 最佳实践

### 1. 协议版本选择

**推荐**:
```pascal
// 仅使用现代协议
LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
```

**避免**:
```pascal
// 不要使用已废弃的协议
LContext.SetProtocolVersions([sslProtocolSSL30, sslProtocolTLS10]); // 不安全！
```

### 2. 证书验证

**推荐**:
```pascal
// 始终验证服务器证书
LContext.SetVerifyMode([sslVerifyPeer]);

// 验证主机名匹配
if not LCert.VerifyHostname('example.com') then
  raise Exception.Create('主机名不匹配');
  
// 使用增强验证检查吊销
LCert.VerifyEx(LStore, [sslCertVerifyCheckRevocation], LResult);
```

**避免**:
```pascal
// 不要跳过证书验证！
LContext.SetVerifyMode([]); // 不安全！
```

### 3. 密码套件配置

**推荐**:
```pascal
// 使用强密码套件
LContext.SetCipherSuites('TLS_AES_256_GCM_SHA384:TLS_AES_128_GCM_SHA256');

// 或使用 Mozilla 推荐配置
LContext.SetCipherList('ECDHE+AESGCM:ECDHE+CHACHA20:DHE+AESGCM:DHE+CHACHA20:!aNULL:!MD5:!DSS');
```

### 4. 错误处理

**推荐**:
```pascal
try
  if not LConn.Connect then
  begin
    var LErrorMsg := LLib.GetLastErrorString;
    var LErrorCode := ClassifyOpenSSLError(LLib.GetLastError);
    
    case LErrorCode of
      sslErrCertificateVerifyFailed:
        WriteLn('证书验证失败: ', LErrorMsg);
      sslErrHandshakeFailed:
        WriteLn('握手失败: ', LErrorMsg);
      sslErrTimeout:
        WriteLn('连接超时: ', LErrorMsg);
    else
      WriteLn('连接失败: ', LErrorMsg);
    end;
  end;
finally
  LLib.ClearError;
end;
```

### 5. 资源管理

**推荐**:
```pascal
var
  LLib: ISSLLibrary;
begin
  LLib := CreateOpenSSLLibrary;
  LLib.Initialize;
  
  try
    // 使用库...
  finally
    LLib.Finalize;  // 总是清理资源
  end;
end;
```

### 6. 日志记录

**推荐**:
```pascal
procedure MyLogCallback(aLevel: TSSLLogLevel; const aMessage: string);
begin
  case aLevel of
    sslLogDebug:   WriteLn('[DEBUG] ', aMessage);
    sslLogInfo:    WriteLn('[INFO] ', aMessage);
    sslLogWarning: WriteLn('[WARN] ', aMessage);
    sslLogError:   WriteLn('[ERROR] ', aMessage);
  end;
end;

begin
  LLib.SetLogCallback(@MyLogCallback);
  LLib.Log(sslLogInfo, '应用程序启动');
end;
```

---

## 性能优化

### 1. 会话复用

```pascal
// 启用会话缓存
LContext.SetSessionCacheMode(True);
LContext.SetSessionTimeout(300);  // 5 分钟
LContext.SetSessionCacheSize(1024);

// 检查会话是否复用
if LConn.IsSessionResumed then
  WriteLn('会话已复用 - 握手更快！');
```

### 2. 使用 ALPN

```pascal
// 设置 ALPN 协议（HTTP/2）
LContext.SetALPNProtocols('h2,http/1.1');

// 检查协商结果
var LProto := LConn.GetALPNSelectedProtocol;
WriteLn('协商的协议: ', LProto);
```

### 3. 缓冲区优化

```pascal
// 批量读写以减少系统调用
const BUFFER_SIZE = 16384;  // 16 KB
var
  LBuffer: array[0..BUFFER_SIZE-1] of Byte;
  LBytesRead: Integer;
begin
  repeat
    LBytesRead := LConn.Read(LBuffer, BUFFER_SIZE);
    if LBytesRead > 0 then
      ProcessData(@LBuffer, LBytesRead);
  until LBytesRead <= 0;
end;
```

### 4. 选择高效密码套件

```pascal
// 优先使用 AES-NI 加速的套件
LContext.SetCipherSuites('TLS_AES_128_GCM_SHA256'); // 硬件加速

// 避免使用慢速算法
// 不推荐: 'TLS_RSA_WITH_3DES_EDE_CBC_SHA'
```

---

## 常见问题

### Q: 如何选择 OpenSSL 还是 WinSSL？

**A**:
- **OpenSSL**: 跨平台、功能全面、社区支持好
- **WinSSL**: Windows 专用、无额外依赖、企业集成

**建议**:
- 跨平台应用 → OpenSSL
- Windows 企业应用 → WinSSL
- 嵌入式/轻量 → MbedTLS (未来)

### Q: 证书验证失败怎么办？

**A**:
1. 检查 CA 证书是否正确加载
2. 验证证书链是否完整
3. 检查证书是否过期
4. 确认主机名是否匹配
5. 检查系统时间是否正确
6. 查看详细错误消息

### Q: 握手超时怎么办？

**A**:
- 增加超时时间
- 检查网络连接
- 验证服务器配置
- 检查防火墙设置

---

## 下一步

- 阅读 [API_REFERENCE.md](API_REFERENCE.md) 了解完整 API
- 查看 [TROUBLESHOOTING.md](TROUBLESHOOTING.md) 解决问题
- 参考 [examples/](../examples/) 目录获取更多示例
- 加入社区讨论

---

**反馈与支持**: [GitHub Issues](https://github.com/dtamade/fafafa.ssl/issues)

