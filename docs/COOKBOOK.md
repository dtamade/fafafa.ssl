# fafafa.ssl Cookbook

实用代码片段集合 - 复制即用的场景化解决方案

---

## 目录

1. [TLS 连接](#tls-连接)
2. [证书操作](#证书操作)
3. [加密操作](#加密操作)
4. [错误处理](#错误处理)
5. [高级场景](#高级场景)

---

## TLS 连接

### 最简 HTTPS 请求

```pascal
uses fafafa.ssl.connection.builder, fafafa.ssl.quick;

var Conn: ISSLConnection;
begin
  Conn := TSSLConnectionBuilder.Create
    .WithHostname('api.example.com')
    .BuildClient;
  try
    Conn.WriteString('GET / HTTP/1.1'#13#10'Host: api.example.com'#13#10#13#10);
    WriteLn(Conn.ReadString(4096));
  finally
    Conn.Shutdown;
  end;
end;
```

### 带超时的 TLS 连接

```pascal
Conn := TSSLConnectionBuilder.Create
  .WithHostname('api.example.com')
  .WithTimeout(30000)  // 30 秒超时
  .WithVerifyPeer(True)
  .BuildClient;
```

### 跳过证书验证（仅测试用）

```pascal
Conn := TSSLConnectionBuilder.Create
  .WithHostname('localhost')
  .WithVerifyPeer(False)  // 危险：仅用于测试
  .BuildClient;
```

### 指定 TLS 版本

```pascal
Ctx := TSSLContextBuilder.Create
  .WithTLS13Only  // 仅 TLS 1.3
  .BuildClient;
```

---

## 证书操作

### 一键生成自签名证书

```pascal
uses fafafa.ssl.quick;

// 生成到文件
TSSLQuick.GenerateCertFiles('localhost', 'server.crt', 'server.key');

// 或获取接口对象
var KeyPair := TSSLQuick.GenerateSelfSigned('localhost');
KeyPair.SaveToFiles('server.crt', 'server.key');
```

### 带完整信息的证书

```pascal
uses fafafa.ssl.cert.builder;

KeyPair := TCertificateBuilder.Create
  .WithCommonName('api.example.com')
  .WithOrganization('My Company')
  .WithCountry('CN')
  .ValidFor(365)
  .WithRSAKey(2048)
  .AsServerCert
  .AddSubjectAltName('DNS:api.example.com')
  .AddSubjectAltName('DNS:*.example.com')
  .SelfSigned;
```

### 读取证书信息

```pascal
var Cert := TSSLQuick.LoadCertificate('server.crt');
WriteLn('Subject: ', Cert.GetSubjectCN);
WriteLn('Issuer: ', Cert.GetIssuerCN);
WriteLn('Valid Until: ', DateToStr(Cert.GetNotAfter));
WriteLn('Days Until Expiry: ', Cert.GetDaysUntilExpiry);
```

### 验证证书链

```pascal
var Store := TSSLFactory.CreateCertificateStore;
Store.LoadSystemStore;  // 加载系统 CA
if Store.Verify(Cert) then
  WriteLn('Certificate is valid')
else
  WriteLn('Certificate verification failed');
```

---

## 加密操作

### SHA-256 哈希

```pascal
uses fafafa.ssl.crypto.utils;

// 字符串哈希
var HashHex := TCryptoUtils.SHA256Hex('Hello World');

// 文件哈希
var FileHash := TCryptoUtils.SHA256File('document.pdf');

// 无异常版本
var Hash: TBytes;
if TCryptoUtils.TrySHA256('data', Hash) then
  WriteLn('Hash: ', BytesToHex(Hash));
```

### AES-256-GCM 加密

```pascal
// 生成密钥和 IV
var Key := TCryptoUtils.GenerateKey(256);  // 32 字节
var IV := TCryptoUtils.SecureRandom(12);   // 12 字节 (GCM 推荐)

// 加密
var Ciphertext := TCryptoUtils.AES_GCM_Encrypt(Plaintext, Key, IV);

// 解密
var Decrypted := TCryptoUtils.AES_GCM_Decrypt(Ciphertext, Key, IV);
```

### 密码派生密钥 (PBKDF2)

```pascal
var Salt := TCryptoUtils.SecureRandom(16);
var Key := TCryptoUtils.PBKDF2('user_password', Salt, 100000, 32);
```

### 安全随机数

```pascal
// 生成随机字节
var RandomBytes := TCryptoUtils.SecureRandom(32);

// 生成随机 AES 密钥
var AESKey := TCryptoUtils.GenerateKey(256);
```

---

## 错误处理

### Result 类型（推荐）

```pascal
uses fafafa.ssl.base;

var Result := TCryptoUtils.TrySHA256Result('data');
if Result.IsOk then
  ProcessData(Result.Unwrap)
else
  WriteLn('Error: ', Result.ErrorMessage);
```

### Try 方法（无异常）

```pascal
var Hash: TBytes;
if TCryptoUtils.TrySHA256('data', Hash) then
  // 成功
else
  // 失败，检查 GetLastError
```

### 传统异常处理

```pascal
try
  Hash := TCryptoUtils.SHA256('data');
except
  on E: ESSLException do
    WriteLn('SSL Error: ', E.Message);
end;
```

---

## 高级场景

### 双向 TLS (mTLS)

```pascal
Ctx := TSSLContextBuilder.Create
  .WithTLS12And13
  .WithVerifyPeer
  .WithSystemRoots
  .WithCertificate('client.crt')
  .WithPrivateKey('client.key')
  .BuildClient;
```

### 会话复用

```pascal
// 首次连接
var Session := Conn.GetSession;

// 后续连接复用会话
Conn2 := Ctx.CreateConnection(Socket2);
Conn2.SetSession(Session);
Conn2.Connect;

if Conn2.IsSessionReused then
  WriteLn('Session reused - faster handshake!');
```

### 连接 AWS IoT

```pascal
Ctx := TSSLContextBuilder.Create
  .WithTLS12And13
  .WithVerifyPeer
  .WithCertificate('device-cert.pem')
  .WithPrivateKey('device-key.pem')
  .WithCAFile('AmazonRootCA1.pem')
  .BuildClient;

Conn := TSSLConnectionBuilder.Create
  .WithContext(Ctx)
  .WithHostname('xxx.iot.region.amazonaws.com')
  .WithPort(8883)
  .BuildClient;
```

### 验证自签名证书

```pascal
Ctx := TSSLContextBuilder.Create
  .WithTLS12And13
  .WithVerifyPeer
  .WithCAFile('my-ca.crt')  // 自签名 CA
  .BuildClient;
```

---

## 后端选择

### 使用 OpenSSL（默认）

```pascal
Lib := TSSLFactory.GetLibrary(sslOpenSSL);
```

### 使用 WinSSL（Windows 原生）

```pascal
Lib := TSSLFactory.GetLibrary(sslWinSSL);
```

### 使用 MbedTLS（Preview）

```pascal
{$DEFINE ENABLE_MBEDTLS}
Lib := TSSLFactory.GetLibrary(sslMbedTLS);
```

---

*最后更新: 2026-01-12*
