# fafafa.ssl 快速入门指南

**版本**: v0.9 RC  
**更新日期**: 2025-10-24

---

## 🚀 5分钟快速入门

### 第一步：安装

#### 通过 Lazarus IDE

1. 打开 Lazarus IDE
2. Package → Open Package File (.lpk)
3. 选择 `fafafa_ssl.lpk`
4. 点击 "Compile"
5. 点击 "Use" → "Install"

#### 手动编译

```bash
lazbuild --build-mode=Release fafafa_ssl.lpk
```

---

### 第二步：第一个 HTTPS 请求

创建新项目 `FirstHTTPS.lpr`:

```pascal
program FirstHTTPS;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.factory;

var
  LResponse: string;
begin
  // 一行代码实现 HTTPS GET！
  LResponse := TSSLFactory.QuickHTTPSGet('https://www.example.com');
  
  WriteLn('Response received:');
  WriteLn(LResponse);
end.
```

**编译运行**:
```bash
fpc -Fusrc FirstHTTPS.lpr
./FirstHTTPS
```

**输出**:
```
Response received:
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

✅ **恭喜！你已经完成第一个 HTTPS 请求！**

---

## 📚 核心概念

### 1. 多后端架构

fafafa.ssl 支持多个 SSL/TLS 后端：

```
应用代码
   ↓
统一接口 (ISSLLibrary, ISSLContext, ISSLConnection)
   ↓
┌──────────┬──────────┬──────────┐
│ OpenSSL  │ WinSSL   │ MbedTLS  │
│  3.x/1.1 │ Schannel │ (计划中) │
└──────────┴──────────┴──────────┘
```

**优势**:
- Windows 平台零依赖（使用 WinSSL）
- 跨平台一致性（使用 OpenSSL）
- 企业友好（组策略、FIPS、智能卡）

### 2. 自动检测

```pascal
var
  LLib: ISSLLibrary;
begin
  // 自动选择最佳后端
  LLib := TSSLFactory.CreateBest;
  
  WriteLn('Using: ', LLib.GetLibraryName);
  // Windows: "WinSSL (Schannel)"
  // Linux/macOS: "OpenSSL 3.1.0"
end;
```

### 3. 手动选择后端

```pascal
// 强制使用 OpenSSL
LLib := TSSLFactory.CreateOpenSSL;

// 强制使用 WinSSL (Windows only)
LLib := TSSLFactory.CreateWinSSL;
```

---

## 💡 常见场景

### 场景 1: HTTPS 客户端

```pascal
uses
  fafafa.ssl.factory, fafafa.ssl.abstract.intf;

var
  LConn: ISSLConnection;
  LRequest, LResponse: string;
begin
  // 快速创建 HTTPS 连接
  LConn := TSSLFactory.QuickClientConnection('www.google.com', 443);
  
  // 发送 HTTP GET 请求
  LRequest := 'GET / HTTP/1.1' + #13#10 +
              'Host: www.google.com' + #13#10 +
              'Connection: close' + #13#10 + #13#10;
  
  LConn.Write(LRequest);
  LResponse := LConn.ReadAll;
  
  WriteLn(LResponse);
end;
```

### 场景 2: 证书信息

```pascal
uses
  fafafa.ssl.factory;

var
  LCert: ISSLCertificate;
begin
  // 加载证书
  LCert := TSSLFactory.LoadCertificateFromFile('mycert.pem');
  
  // 显示证书信息
  WriteLn('Subject: ', LCert.GetSubject);
  WriteLn('Issuer: ', LCert.GetIssuer);
  WriteLn('Valid From: ', DateTimeToStr(LCert.GetNotBefore));
  WriteLn('Valid To: ', DateTimeToStr(LCert.GetNotAfter));
  WriteLn('Serial: ', LCert.GetSerialNumber);
  
  // 检查证书有效性
  if LCert.IsExpired then
    WriteLn('⚠️ Certificate has expired!');
  
  if LCert.IsSelfSigned then
    WriteLn('ℹ️ Self-signed certificate');
end;
```

### 场景 3: 文件加密/解密

```pascal
uses
  fafafa.ssl.utils;

begin
  // AES-256-GCM 加密
  EncryptFile(
    'secret.txt',      // 输入文件
    'secret.enc',      // 输出文件
    'my-password-123', // 密码
    'AES-256-GCM'      // 算法
  );
  
  WriteLn('✅ File encrypted');
  
  // 解密
  DecryptFile(
    'secret.enc',
    'decrypted.txt',
    'my-password-123',
    'AES-256-GCM'
  );
  
  WriteLn('✅ File decrypted');
end;
```

### 场景 4: 数字签名

```pascal
uses
  fafafa.ssl.factory;

var
  LPrivKey: ISSLPrivateKey;
  LSignature: TBytes;
  LData: string;
begin
  // 加载私钥
  LPrivKey := TSSLFactory.LoadPrivateKeyFromFile('private.pem', 'password');
  
  // 签名数据
  LData := 'Important message';
  LSignature := LPrivKey.Sign(LData, 'SHA256');
  
  WriteLn('Signature (Base64): ', EncodeBase64(LSignature));
  
  // 验证签名
  if LPrivKey.Verify(LData, LSignature, 'SHA256') then
    WriteLn('✅ Signature valid')
  else
    WriteLn('❌ Signature invalid');
end;
```

---

## 🔧 高级配置

### 配置 SSL 上下文

```pascal
uses
  fafafa.ssl.factory, fafafa.ssl.abstract.intf;

var
  LContext: ISSLContext;
begin
  LContext := TSSLFactory.CreateBest.CreateContext(sslRoleClient);
  
  // 设置协议版本
  LContext.SetMinProtocolVersion(sslProtoTLS12);
  LContext.SetMaxProtocolVersion(sslProtoTLS13);
  
  // 设置密码套件
  LContext.SetCipherList('ECDHE-RSA-AES256-GCM-SHA384:ECDHE-RSA-AES128-GCM-SHA256');
  
  // 加载证书和私钥 (服务器端)
  LContext.LoadCertificate('server.crt');
  LContext.LoadPrivateKey('server.key', 'password');
  
  // 加载 CA 证书 (客户端)
  LContext.LoadCAFile('ca-bundle.crt');
  
  // 启用证书验证
  LContext.SetVerifyMode(sslVerifyPeer);
  
  // 创建连接
  var LConn := LContext.CreateConnection('example.com', 443);
  LConn.Connect;
end;
```

### WinSSL 企业功能

```pascal
uses
  fafafa.ssl.winssl.enterprise;

var
  LConfig: TSSLEnterpriseConfig;
begin
  LConfig := TSSLEnterpriseConfig.Create;
  try
    LConfig.LoadFromSystem;
    
    // 检查 FIPS 模式
    if LConfig.IsFIPSEnabled then
      WriteLn('✅ FIPS 140-2 compliant mode');
    
    // 获取企业信任的根证书
    var LRoots := LConfig.GetTrustedRoots;
    WriteLn('Enterprise roots: ', Length(LRoots));
    
    // 读取组策略
    var LPolicy := LConfig.ReadGroupPolicy('CryptoPolicy');
    WriteLn('Crypto policy: ', LPolicy);
  finally
    LConfig.Free;
  end;
end;
```

---

## 🐛 故障排除

### 问题 1: OpenSSL 库未找到

**错误**: `Could not load OpenSSL library`

**解决方案**:

**Windows**:
```bash
# 下载 OpenSSL for Windows
# https://slproweb.com/products/Win32OpenSSL.html
# 或将 libssl-3-x64.dll, libcrypto-3-x64.dll 放到 exe 目录
```

**Linux**:
```bash
sudo apt install libssl3  # Ubuntu/Debian
sudo yum install openssl-libs  # CentOS/RHEL
```

**macOS**:
```bash
brew install openssl@3
```

### 问题 2: 证书验证失败

**错误**: `Certificate verification failed`

**解决方案**:

```pascal
// 临时禁用验证（仅用于测试！）
LContext.SetVerifyMode(sslVerifyNone);

// 或加载正确的 CA 证书
LContext.LoadCAFile('/etc/ssl/certs/ca-certificates.crt');  // Linux
LContext.LoadCAFile('C:\Windows\System32\cert.pem');  // Windows
```

### 问题 3: 连接超时

**错误**: `Connection timed out`

**解决方案**:

```pascal
// 增加超时时间
LConn.SetTimeout(30000);  // 30 秒

// 或使用代理
LConn.SetProxy('proxy.company.com', 8080);
```

---

## 📖 下一步

### 学习更多

- [用户指南](02_user_guide/README.md) - 深入了解概念和用法
- [API 参考](03_api_reference/README.md) - 完整 API 文档
- [示例应用](../examples/) - 11+ 实际示例

### 示例程序

- `examples/simple_https_client/` - 简单 HTTPS 客户端
- `examples/certificate_info/` - 证书信息查看
- `examples/file_encryption/` - 文件加密工具
- `examples/ssl_server/` - SSL 服务器
- `examples/rest_api_client/` - REST API 客户端

### 获取帮助

- GitHub Issues: [github.com/yourusername/fafafa.ssl/issues](https://github.com)
- FAQ: [docs/06_troubleshooting/faq.md](06_troubleshooting/faq.md)
- 社区: [discussions](https://github.com/yourusername/fafafa.ssl/discussions)

---

## 🎉 恭喜！

你已经掌握了 fafafa.ssl 的基础用法。继续探索更多功能，构建安全的应用程序！

**记住核心原则**:
1. 🔒 **安全第一** - 始终验证证书
2. 🌍 **跨平台** - 一套代码，多平台运行
3. 🚀 **简单易用** - 从简单API开始，逐步深入

---

**快速入门指南** | [用户指南 →](02_user_guide/README.md)

