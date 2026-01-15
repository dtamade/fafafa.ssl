# fafafa.ssl 故障排除指南

> **版本**: v0.8  
> **最后更新**: 2025-10-24

本指南帮助你快速诊断和解决 fafafa.ssl 使用中的常见问题。

## 目录

- [安装问题](#安装问题)
- [编译问题](#编译问题)
- [运行时错误](#运行时错误)
- [连接问题](#连接问题)
- [证书问题](#证书问题)
- [性能问题](#性能问题)
- [平台特定问题](#平台特定问题)

---

## 安装问题

### 问题: 找不到 OpenSSL 库

**错误信息**:
```
Failed to load OpenSSL library
Error: Cannot load libcrypto-3-x64.dll
```

**解决方案**:

**Windows**:
```powershell
# 方法 1: 安装 OpenSSL
choco install openssl

# 方法 2: 下载并安装
# https://slproweb.com/products/Win32OpenSSL.html

# 方法 3: 复制 DLL 到程序目录
copy C:\OpenSSL-Win64\bin\libcrypto-3-x64.dll .
copy C:\OpenSSL-Win64\bin\libssl-3-x64.dll .

# 方法 4: 添加到 PATH
$env:PATH += ";C:\OpenSSL-Win64\bin"
```

**Linux**:
```bash
# Ubuntu/Debian
sudo apt update
sudo apt install libssl3 libssl-dev

# Fedora/RHEL
sudo dnf install openssl openssl-devel

# 检查安装
ldconfig -p | grep libssl
```

**macOS**:
```bash
# 安装
brew install openssl@3

# 设置路径
export DYLD_LIBRARY_PATH=/usr/local/opt/openssl@3/lib:$DYLD_LIBRARY_PATH
```

### 问题: 版本不兼容

**错误信息**:
```
OpenSSL version 1.0.2 not supported
```

**解决方案**:
```bash
# 检查当前版本
openssl version

# 需要 OpenSSL 1.1.1+ 或 3.x
# 升级到支持的版本
```

---

## 编译问题

### 问题: Unit not found

**错误信息**:
```
Fatal: Can't find unit fafafa.ssl.openssl
```

**解决方案**:
```bash
# 确保使用 -Fu 指定源码路径
fpc -Fusrc -Fusrc/openssl your_program.pas

# 或使用绝对路径
fpc -FuD:\projects\Pascal\lazarus\My\libs\fafafa.ssl\src your_program.pas
```

### 问题: Identifier not found

**错误信息**:
```
Error: Identifier not found "ISSLLibrary"
```

**解决方案**:
```pascal
// 确保引入正确的单元
uses
  fafafa.ssl.abstract.intf,    // 抽象接口
  fafafa.ssl.abstract.types,   // 类型定义
  fafafa.ssl.openssl;          // OpenSSL 实现

// 或者只引入主单元（它会自动引入依赖）
uses
  fafafa.ssl.openssl;
```

### 问题: Compilation mode mismatch

**错误信息**:
```
Error: Compilation modes differ: objfpc vs delphi
```

**解决方案**:
```pascal
// 在程序顶部添加编译指令
{$mode objfpc}{$H+}
```

---

## 运行时错误

### 问题: 初始化失败

**错误信息**:
```
Failed to initialize SSL library
```

**诊断步骤**:
```pascal
// 1. 检查库是否可用
if not OpenSSLAvailable then
begin
  WriteLn('OpenSSL not available');
  WriteLn('Error: ', GetOpenSSLErrorString);
  Exit;
end;

// 2. 尝试加载库
if not LoadOpenSSL then
begin
  WriteLn('Failed to load OpenSSL');
  WriteLn('Library path: ', GetOpenSSLLibraryPath);
  Exit;
end;

// 3. 检查版本
WriteLn('OpenSSL version: ', GetOpenSSLVersion);
if GetOpenSSLVersionNumber < $1010100F then  // 1.1.1
  WriteLn('Warning: OpenSSL version too old');
```

### 问题: Access violation

**错误信息**:
```
An exception occurred at $000000010001234:
EAccessViolation: Access violation
```

**常见原因**:
1. 使用未初始化的对象
2. 释放已释放的内存
3. OpenSSL 函数指针为 nil

**解决方案**:
```pascal
// 1. 检查对象是否有效
if LCert = nil then
begin
  WriteLn('Certificate object is nil');
  Exit;
end;

// 2. 检查 OpenSSL 函数是否加载
if not Assigned(X509_new) then
begin
  WriteLn('X509_new function not loaded');
  WriteLn('Call LoadOpenSSL first');
  Exit;
end;

// 3. 使用 try-finally 保护资源
var
  LLib: ISSLLibrary;
begin
  LLib := CreateOpenSSLLibrary;
  try
    LLib.Initialize;
    // ... 使用库 ...
  finally
    LLib.Finalize;
  end;
end;
```

---

## 连接问题

### 问题: 握手失败

**错误信息**:
```
SSL handshake failed
Error: tlsv1 alert protocol version
```

**诊断**:
```pascal
// 1. 检查协议版本
LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);

// 2. 查看详细错误
if not LConn.Connect then
begin
  var LErr := LLib.GetLastError;
  WriteLn('Error code: ', LErr);
  WriteLn('Error string: ', GetOpenSSLErrorString(LErr));
  WriteLn('Classification: ', Ord(ClassifyOpenSSLError(LErr)));
end;

// 3. 启用调试日志
LLib.SetLogCallback(@MyLogCallback);
```

**常见原因与解决方案**:

| 错误消息 | 原因 | 解决方案 |
|---------|------|----------|
| `protocol version` | 协议版本不匹配 | 启用 TLS 1.2+ |
| `certificate verify failed` | 证书验证失败 | 检查 CA 证书 |
| `certificate unknown` | 客户端证书问题 | 检查客户端证书配置 |
| `handshake failure` | 密码套件不匹配 | 配置兼容的密码套件 |
| `unknown ca` | CA 证书未知 | 加载正确的 CA 证书 |

### 问题: 连接超时

**错误信息**:
```
Connection timeout
```

**解决方案**:
```pascal
// 1. 设置超时
LConn.SetTimeout(30000);  // 30 秒

// 2. 检查网络连接
if not CanConnect(aHost, aPort) then
  raise Exception.Create('Cannot reach server');

// 3. 检查 DNS 解析
var LIP := ResolveDNS(aHost);
WriteLn('Resolved IP: ', LIP);

// 4. 测试原始 TCP 连接
if TestTCPConnection(aHost, aPort) then
  WriteLn('TCP connection OK')
else
  WriteLn('TCP connection failed');
```

### 问题: 连接意外关闭

**错误信息**:
```
SSL connection closed unexpectedly
Error: EOF occurred in violation of protocol
```

**原因**:
- 服务器强制关闭连接
- 网络中断
- 防火墙干扰

**解决方案**:
```pascal
// 1. 实现重连逻辑
var
  LRetries: Integer = 3;
  LConnected: Boolean = False;
begin
  for var i := 1 to LRetries do
  begin
    try
      if LConn.Connect then
      begin
        LConnected := True;
        Break;
      end;
    except
      on E: Exception do
      begin
        WriteLn('Retry ', i, ' failed: ', E.Message);
        Sleep(1000 * i);  // 递增延迟
      end;
    end;
  end;
end;

// 2. 检查 Keep-Alive
LConn.SetKeepAlive(True, 60, 3);  // 60秒，3次探测

// 3. 优雅关闭
try
  LConn.Shutdown;
finally
  LConn.Close;
end;
```

---

## 证书问题

### 问题: 证书验证失败

**错误信息**:
```
Certificate verify failed: unable to get local issuer certificate
```

**诊断步骤**:
```pascal
// 1. 检查证书
var
  LCert: ISSLCertificate;
begin
  LCert := LLib.CreateCertificate;
  if LCert.LoadFromFile('mycert.pem') then
  begin
    WriteLn('Subject: ', LCert.GetSubject);
    WriteLn('Issuer: ', LCert.GetIssuer);
    WriteLn('Valid from: ', DateTimeToStr(LCert.GetNotBefore));
    WriteLn('Valid to: ', DateTimeToStr(LCert.GetNotAfter));
    WriteLn('Is expired: ', BoolToStr(LCert.IsExpired, True));
    WriteLn('Is self-signed: ', BoolToStr(LCert.IsSelfSigned, True));
  end;
end;

// 2. 检查证书链
var
  LChain: TSSLCertificateArray;
begin
  LChain := LConn.GetPeerCertificateChain;
  WriteLn('Certificate chain length: ', Length(LChain));
  for var i := 0 to High(LChain) do
    WriteLn('  [', i, '] ', LChain[i].GetSubject);
end;

// 3. 使用增强验证获取详细信息
var
  LResult: TSSLCertVerifyResult;
begin
  if not LCert.VerifyEx(LStore, [sslCertVerifyCheckRevocation], LResult) then
  begin
    WriteLn('Verification failed:');
    WriteLn('  Error code: ', LResult.ErrorCode);
    WriteLn('  Error message: ', LResult.ErrorMessage);
    WriteLn('  Chain status: ', LResult.ChainStatus);
    WriteLn('  Revocation status: ', LResult.RevocationStatus);
    WriteLn('  Details: ', LResult.DetailedInfo);
  end;
end;
```

**解决方案**:

**缺少 CA 证书**:
```pascal
// 加载 CA 证书包
LContext.LoadCAFile('/etc/ssl/certs/ca-bundle.crt');  // Linux
LContext.LoadCAFile('C:\Windows\curl-ca-bundle.crt');  // Windows

// 或加载整个目录
LContext.LoadCAPath('/etc/ssl/certs');
```

**自签名证书**:
```pascal
// 方法 1: 禁用验证（仅用于测试！）
LContext.SetVerifyMode([]);

// 方法 2: 添加自签名证书到信任存储
LStore := LLib.CreateCertificateStore;
LStore.AddCertificate(LSelfSignedCert);
LContext.SetCertificateStore(LStore);

// 方法 3: 使用验证标志允许自签名
LCert.VerifyEx(LStore, [sslCertVerifyAllowSelfSigned], LResult);
```

### 问题: 主机名不匹配

**错误信息**:
```
Hostname verification failed: certificate is for *.example.com, not www.test.com
```

**解决方案**:
```pascal
// 1. 检查主机名
var
  LHostname: string = 'www.example.com';
begin
  if not LCert.VerifyHostname(LHostname) then
  begin
    WriteLn('Hostname mismatch');
    
    // 显示证书中的主机名
    var LAltNames := LCert.GetSubjectAltNames;
    WriteLn('Certificate is valid for:');
    for var i := 0 to LAltNames.Count - 1 do
      WriteLn('  ', LAltNames[i]);
    LAltNames.Free;
  end;
end;

// 2. 使用正确的主机名连接
// 确保使用的主机名与证书匹配

// 3. 仅用于测试：忽略主机名验证
LCert.VerifyEx(LStore, [sslCertVerifyIgnoreHostname], LResult);
```

### 问题: 证书已过期

**错误信息**:
```
Certificate has expired
```

**解决方案**:
```pascal
// 1. 检查证书有效期
if LCert.IsExpired then
begin
  WriteLn('Certificate expired on: ', DateTimeToStr(LCert.GetNotAfter));
  // 更新证书
end;

// 2. 检查系统时间
WriteLn('System time: ', DateTimeToStr(Now));
// 确保系统时间正确

// 3. 仅用于测试：忽略过期检查
LCert.VerifyEx(LStore, [sslCertVerifyIgnoreExpiry], LResult);
```

---

## 性能问题

### 问题: 握手太慢

**诊断**:
```pascal
var
  LStartTime, LEndTime: TDateTime;
begin
  LStartTime := Now;
  if LConn.Connect then
  begin
    LEndTime := Now;
    WriteLn('Handshake time: ', MilliSecondsBetween(LEndTime, LStartTime), ' ms');
  end;
end;
```

**优化方案**:
```pascal
// 1. 启用会话复用
LContext.SetSessionCacheMode(True);
LContext.SetSessionCacheSize(1024);
LContext.SetSessionTimeout(300);

// 2. 使用更快的密码套件
LContext.SetCipherSuites('TLS_AES_128_GCM_SHA256');  // 硬件加速

// 3. 使用 ECDHE 而非 RSA
LContext.SetCipherList('ECDHE+AESGCM');

// 4. 检查是否复用会话
if LConn.IsSessionResumed then
  WriteLn('Session reused - handshake faster!');
```

### 问题: 数据传输慢

**优化方案**:
```pascal
// 1. 增大缓冲区
const BUFFER_SIZE = 65536;  // 64 KB

// 2. 批量读写
var
  LBuffer: array[0..BUFFER_SIZE-1] of Byte;
  LTotal: Int64 = 0;
begin
  repeat
    var LRead := LConn.Read(LBuffer, BUFFER_SIZE);
    if LRead > 0 then
    begin
      ProcessData(@LBuffer, LRead);
      Inc(LTotal, LRead);
    end;
  until LRead <= 0;
  
  WriteLn('Total received: ', LTotal, ' bytes');
end;

// 3. 使用非阻塞 I/O
LConn.SetBlocking(False);
```

---

## 平台特定问题

### Windows

**问题: WinSSL 企业功能不工作**

```pascal
// 检查 Windows 版本
{$IFDEF WINDOWS}
if TOSVersion.Major < 7 then
  WriteLn('Warning: Windows 7+ required for full WinSSL support');

// 检查 FIPS 模式
var LConfig := TSSLEnterpriseConfig.Create;
try
  LConfig.LoadFromSystem;
  if LConfig.IsFipsModeEnabled then
    WriteLn('FIPS mode enabled')
  else
    WriteLn('FIPS mode not enabled');
finally
  LConfig.Free;
end;
{$ENDIF}
```

### Linux

**问题: CA 证书路径不正确**

```bash
# 不同发行版的 CA 证书路径
# Ubuntu/Debian
/etc/ssl/certs/ca-certificates.crt

# Fedora/RHEL
/etc/pki/tls/certs/ca-bundle.crt

# OpenSUSE
/etc/ssl/ca-bundle.pem

# 查找 CA 证书
find /etc -name "ca-bundle.crt" -o -name "ca-certificates.crt" 2>/dev/null
```

### macOS

**问题: OpenSSL 找不到**

```bash
# macOS 默认不带 OpenSSL 3
# 需要通过 Homebrew 安装
brew install openssl@3

# 设置库路径
export DYLD_LIBRARY_PATH=/usr/local/opt/openssl@3/lib

# 或在代码中指定路径
LoadOpenSSL('/usr/local/opt/openssl@3/lib');
```

---

## 调试技巧

### 启用详细日志

```pascal
// 1. 设置日志回调
procedure MyLogCallback(aLevel: TSSLLogLevel; const aMessage: string);
begin
  var LLevelStr: string;
  case aLevel of
    sslLogDebug:   LLevelStr := 'DEBUG';
    sslLogInfo:    LLevelStr := 'INFO';
    sslLogWarning: LLevelStr := 'WARN';
    sslLogError:   LLevelStr := 'ERROR';
  end;
  
  WriteLn(Format('[%s] %s: %s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
    LLevelStr,
    aMessage
  ]));
end;

begin
  LLib.SetLogCallback(@MyLogCallback);
  LLib.Log(sslLogInfo, 'Application started');
end;

// 2. 启用 OpenSSL 调试（环境变量）
// Windows: set OPENSSL_DEBUG=1
// Linux: export OPENSSL_DEBUG=1
```

### 使用 Wireshark 抓包

```bash
# 设置 SSLKEYLOGFILE 环境变量
export SSLKEYLOGFILE=/tmp/sslkeys.log

# 运行程序
./your_program

# 在 Wireshark 中配置
# Edit -> Preferences -> Protocols -> TLS
# (Pre)-Master-Secret log filename: /tmp/sslkeys.log
```

### 测试工具

```bash
# OpenSSL s_client（测试服务器）
openssl s_client -connect example.com:443 -showcerts

# OpenSSL s_server（测试客户端）
openssl s_server -accept 4433 -cert server.crt -key server.key

# cURL（验证连接）
curl -v --cacert ca-bundle.crt https://example.com

# nmap（扫描支持的密码套件）
nmap --script ssl-enum-ciphers -p 443 example.com
```

---

## 获取帮助

如果以上方法仍无法解决问题：

1. **检查文档**
   - [API_REFERENCE.md](API_REFERENCE.md)
   - [USER_GUIDE.md](USER_GUIDE.md)
   - [examples/](../examples/)

2. **搜索 Issues**
   - [GitHub Issues](https://github.com/dtamade/fafafa.ssl/issues)
   - 搜索类似问题

3. **提交 Bug 报告**
   - 包含完整错误信息
   - 提供最小可复现示例
   - 说明环境（OS、FPC/Lazarus 版本、OpenSSL 版本）
   - 附加相关日志

4. **社区讨论**
   - [GitHub Discussions](https://github.com/dtamade/fafafa.ssl/discussions)

---

**持续更新中** - 如有新问题，请提交 Issue 帮助我们改进此文档。

