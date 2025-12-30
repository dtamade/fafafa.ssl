# 🚀 fafafa.ssl 快速入门指南

欢迎使用 **fafafa.ssl** - 一个专注、纯粹的FreePascal SSL/TLS库！

---

## 📖 什么是 fafafa.ssl？

**fafafa.ssl 是一个SSL/TLS库，不是HTTP库。**

它专注于做好一件事：SSL/TLS加密。

```
fafafa.ssl 提供：
  ✓ SSL/TLS加密层
  ✓ 证书管理
  ✓ 密码学工具
  ✓ Socket暴露

应用层协议（用户自己实现）：
  ✗ HTTP/HTTPS
  ✗ SMTP/SMTPS  
  ✗ FTP/FTPS
```

---

## 🎯 核心理念

遵循Unix哲学："做好一件事"

- **fafafa.ssl** 负责SSL/TLS加密
- **用户** 使用暴露的Socket实现自己的协议
- **结果** 灵活、可控、专业

---

## ⚙️ 安装

### Linux (Debian/Ubuntu)

```bash
sudo apt-get install fpc fp-units-fcl fp-units-net
```

### Windows

下载并安装 [Free Pascal](https://www.freepascal.org/download.html)

---

## 📥 获取代码

```bash
git clone https://github.com/your-username/fafafa.ssl.git
cd fafafa.ssl
```

---

## ✅ 验证环境

```bash
cd tests/unit
lazbuild test_basic_compilation.lpi
./test_basic_compilation
```

看到 "✅ All Tests Passed" 说明环境正确！

---

## 📚 第一个例子

### 示例1：计算哈希

```pascal
program hash_example;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.utils,
  fafafa.ssl.types;

var
  LHash: string;
begin
  // 计算SHA256哈希
  LHash := ComputeDigest('Hello, World!', dtSHA256);
  WriteLn('SHA256: ', LHash);
end.
```

**编译运行**：

```bash
cd examples
fpc -Fu../src -Fu../src/openssl -Fi../src hash_example.pas
./hash_example
```

---

### 示例2：SSL/TLS连接（完整流程）

```pascal
program ssl_example;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  {$IFDEF WINDOWS}WinSock2{$ELSE}Sockets, BaseUnix{$ENDIF},
  fafafa.ssl.factory, fafafa.ssl.abstract.types, fafafa.ssl.abstract.intf;

var
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LSocket: THandle;
  LRequest: string;
  LBuffer: array[0..4095] of Byte;
  LBytesRead: Integer;
begin
  // 1. 创建SSL上下文
  LContext := TSSLFactory.CreateContext(sslCtxClient);
  LContext.SetVerifyMode([sslVerifyPeer]);
  
  // 2. 用户自己创建socket（这里用系统API，也可以用Synapse/Indy）
  {$IFDEF WINDOWS}
  LSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  // ... 连接到example.com:443 ...
  {$ELSE}
  LSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
  // ... 连接到example.com:443 ...
  {$ENDIF}
  
  // 3. 将socket传入SSL库（SSL库不创建socket）
  LConnection := LContext.CreateConnection(LSocket);
  LConnection.SetHostname('example.com');
  
  // 4. SSL握手
  if not LConnection.Connect then
    raise Exception.Create('SSL握手失败');
  
  WriteLn('✓ SSL连接成功');
  WriteLn('协议: ', Ord(LConnection.GetProtocol));
  WriteLn('密码套件: ', LConnection.GetCipher);
  
  // 5. 发送数据（用户自己构造HTTP请求）
  LRequest := 
    'GET / HTTP/1.1'#13#10 +
    'Host: example.com'#13#10 +
    'Connection: close'#13#10#13#10;
  LConnection.Write(@LRequest[1], Length(LRequest));
  
  // 6. 接收响应（用户自己解析HTTP响应）
  repeat
    LBytesRead := LConnection.Read(@LBuffer[0], SizeOf(LBuffer));
    if LBytesRead > 0 then
      Write(string(PAnsiChar(@LBuffer[0])));
  until LBytesRead <= 0;
end.
```

**重要说明**:
- fafafa.ssl **不创建socket**，用户需要自己创建
- 用户可以用任何方式创建socket：
  - 系统API（WinSock2、BSD Socket）
  - 网络库（Synapse、Indy、lNet等）
- 然后将socket传入`CreateConnection(aSocket: THandle)`

**使用lazbuild编译**：

```bash
cd examples
lazbuild simple_ssl_connection.lpi
./simple_ssl_connection
```

---

## 🔧 核心概念

### 1. SSL上下文 (ISSLContext)

管理SSL配置和创建连接：

```pascal
LContext := TSSLFactory.CreateContext(sslClient);
LContext.SetVerifyMode([sslVerifyPeer]);
LContext.SetMinProtocolVersion(sslProtocolTLS12);
  ```

### 2. SSL连接 (ISSLConnection)

处理SSL/TLS通信：

  ```pascal
LConnection := LContext.CreateConnection(LSocket);
LConnection.SetSNI('example.com');
LConnection.Connect;  // SSL握手
```

### 3. Socket接口

**fafafa.ssl不创建socket**，只接收socket：

```pascal
// 用户创建socket（任何方式）
LSocket := CreateSocketSomehow();

// 传入SSL库
LConnection := LContext.CreateConnection(LSocket);
```

### 4. 工具函数

密码学工具：

```pascal
// 哈希
LHash := ComputeDigest('data', dtSHA256);

// Base64编码
LEncoded := Base64Encode('data');
LDecoded := Base64Decode(LEncoded);

// Hex编码
LHex := HexEncode('data');
LData := HexDecode(LHex);
```

---

## 🌟 常见场景

### 场景1：实现自己的HTTP客户端

```pascal
// 你自己构造HTTP请求
LRequest := 'GET /api/data HTTP/1.1'#13#10 +
            'Host: api.example.com'#13#10 +
            'Authorization: Bearer your-token'#13#10 +
            'Connection: close'#13#10#13#10;

// 发送
LConnection.Write(@LRequest[1], Length(LRequest));

// 接收并解析响应（你自己实现解析）
// ...
```

### 场景2：使用现成的HTTP库

如果你不想自己实现HTTP：

**方案A：使用fpHTTPClient (FCL内置)**

```pascal
uses
  fafafa.ssl.openssl,  // SSL/TLS层
  fphttpclient;         // HTTP层

var
  Client: TFPHTTPClient;
begin
  Client := TFPHTTPClient.Create(nil);
  try
    WriteLn(Client.Get('https://example.com'));
  finally
    Client.Free;
  end;
  end;
```

**方案B：使用Synapse**

```pascal
uses
  fafafa.ssl.openssl,  // SSL/TLS层
  httpsend,            // Synapse HTTP
  ssl_openssl;         // Synapse SSL支持
  
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    HTTP.HTTPMethod('GET', 'https://example.com');
    // ...
  finally
    HTTP.Free;
  end;
end;
```

---

## 🎓 进阶主题

### Windows零依赖（WinSSL）

Windows上默认使用WinSSL，无需OpenSSL DLL：

```pascal
// Windows上自动使用WinSSL
LContext := TSSLFactory.CreateContext(sslClient);
WriteLn('使用后端: ', TSSLFactory.GetDefaultLibraryName);
// 输出: WinSSL
```

### 强制使用OpenSSL

```pascal
TSSLFactory.SetDefaultLibrary(sslOpenSSL);
LContext := TSSLFactory.CreateContext(sslClient);
```

### 证书验证

```pascal
// 严格验证
LContext.SetVerifyMode([sslVerifyPeer, sslVerifyFailIfNoPeerCert]);
  
// 加载CA证书
LContext.LoadCAFile('/path/to/ca-bundle.crt');
  
// 自定义验证回调
LContext.SetVerifyCallback(@MyVerifyCallback);
```

### 客户端证书

```pascal
// 加载客户端证书
LContext.LoadCertificate('/path/to/client.crt');
LContext.LoadPrivateKey('/path/to/client.key');
```

---

## 📂 示例程序

项目提供了多个完整示例：

```bash
cd examples

# 基础SSL连接
lazbuild simple_ssl_connection.lpi && ./simple_ssl_connection

# 证书验证
lazbuild certificate_example.lpi && ./certificate_example

# 哈希和编码
lazbuild hash_example.lpi && ./hash_example
```

---

## 🐛 常见问题

### Q: 为什么没有内置HTTP客户端？

**A**: fafafa.ssl专注于SSL/TLS加密，HTTP是应用层协议。这种设计：
- 遵循单一职责原则
- 避免重复造轮子  
- 给用户最大灵活性
- 代码更稳定易维护

### Q: 我该如何发起HTTPS请求？

**A**: 两种方案：
1. 用fafafa.ssl的Socket自己实现HTTP（完全控制）
2. 使用fpHTTPClient/Synapse/Indy + fafafa.ssl（成熟方案）

### Q: Linux上能用吗？

**A**: 可以！
- **核心功能**：100%可用（SSL/TLS、证书、工具）
- **Socket**：Windows完整，Linux需要使用其他库或自己实现
- **建议**：Linux上配合Synapse/fpHTTPClient使用

### Q: Windows上需要OpenSSL DLL吗？

**A**: 不需要！Windows上默认使用WinSSL（Windows原生Schannel），零依赖。

---

## 📖 更多资源

- [架构设计](ARCHITECTURE.md) - 理解设计理念
- [API参考](docs/API_REFERENCE.md) - 完整API文档
- [示例代码](examples/) - 各种使用示例
- [审查报告](AUDIT_REPORT.md) - 项目质量报告

---

## 🤝 获取帮助

- **GitHub Issues**: [提交问题](https://github.com/your-username/fafafa.ssl/issues)
- **GitHub Discussions**: [讨论交流](https://github.com/your-username/fafafa.ssl/discussions)

---

## 🎉 下一步

1. ✅ 运行示例程序熟悉API
2. ✅ 阅读[架构设计](ARCHITECTURE.md)理解理念
3. ✅ 查看[示例代码](examples/)学习用法
4. ✅ 开始在你的项目中使用！

---

**祝你使用愉快！** 🚀

如有问题，随时在GitHub上联系我们。

---

## 🔐 高级功能

### 证书验证和管理

**fafafa.ssl** 提供完整的证书验证和管理功能：

```pascal
// 创建证书存储
Store := SSLLib.CreateCertificateStore;

// 加载系统证书
Store.LoadSystemStore;

// 加载证书
Cert := SSLLib.CreateCertificate;
Cert.LoadFromFile('cert.pem');

// 基础验证
if Cert.Verify(Store) then
  WriteLn('Certificate is valid');

// 高级验证（带详细结果）
if Cert.VerifyEx(Store, [sslCertVerifyCheckTime], VerifyResult) then
  WriteLn('Valid: ', VerifyResult.ErrorMessage)
else
  WriteLn('Invalid: ', VerifyResult.DetailedInfo);

// 主机名验证
if Cert.VerifyHostname('example.com') then
  WriteLn('Hostname matches');

// 证书信息
WriteLn('Subject: ', Cert.GetSubject);
WriteLn('Issuer: ', Cert.GetIssuer);
WriteLn('Serial: ', Cert.GetSerialNumber);
WriteLn('SHA256: ', Cert.GetFingerprintSHA256);
WriteLn('Not Before: ', DateTimeToStr(Cert.GetNotBefore));
WriteLn('Not After: ', DateTimeToStr(Cert.GetNotAfter));

// 证书状态检查
WriteLn('Is Expired: ', Cert.IsExpired);
WriteLn('Is Self-Signed: ', Cert.IsSelfSigned);
WriteLn('Is CA: ', Cert.IsCA);
```

### 证书搜索

```pascal
// 按不同条件搜索证书
Cert := Store.FindBySubject('DigiCert');
Cert := Store.FindByIssuer('VeriSign');
Cert := Store.FindBySerialNumber('1234567890');
Cert := Store.FindByFingerprint('AB:CD:EF:...');

// 枚举所有证书
for I := 0 to Store.GetCount - 1 do
begin
  Cert := Store.GetCertificate(I);
  WriteLn(Cert.GetSubject);
end;

// 构建证书链
Chain := Store.BuildCertificateChain(Cert);
WriteLn('Chain length: ', Length(Chain));
```

### 会话复用

提高SSL/TLS性能，避免重复握手：

```pascal
// 保存会话
Session := Connection.GetSession;
SessionData := Session.Serialize;
// 保存到文件或数据库...

// 加载会话
Session := SSLLib.CreateSession;
Session.Deserialize(SessionData);

// 复用会话
NewConnection := Context.CreateConnection(NewSocket);
NewConnection.SetSession(Session);
NewConnection.Connect;  // 快速恢复，无需完整握手

// 会话信息
WriteLn('Session ID: ', Session.GetID);
WriteLn('Timeout: ', Session.GetTimeout);
WriteLn('Protocol: ', ProtocolVersionToString(Session.GetProtocolVersion));
WriteLn('Cipher: ', Session.GetCipherName);

// 克隆会话（用于连接池）
Session2 := Session.Clone;
```

### 公钥管理

```pascal
// 获取公钥信息
WriteLn('Public Key Algorithm: ', Cert.GetPublicKeyAlgorithm);  // RSA, DSA, EC, DH
WriteLn('Signature Algorithm: ', Cert.GetSignatureAlgorithm);

// 获取公钥数据
PubKey := Cert.GetPublicKey;
```

---

## 📚 示例程序

查看 `examples/` 目录获取完整示例：

- **`certificate_verification_example.pas`** - 证书验证和搜索
- **`session_reuse_example.pas`** - 会话复用和持久化

编译运行示例：

```bash
cd examples
fpc certificate_verification_example.pas
./certificate_verification_example
```

---

## 🎯 完整功能清单

### ISSLCertificate - 证书管理
- ✅ 加载/保存 (PEM, DER, 文件, 流, 内存)
- ✅ 信息提取 (主题, 颁发者, 序列号, 日期, 指纹)
- ✅ 验证 (基础, 高级, 主机名)
- ✅ 状态检查 (过期, 自签名, CA)
- ✅ 公钥管理 (算法识别)

### ISSLCertificateStore - 证书存储
- ✅ 加载 (系统, 文件, 路径)
- ✅ 管理 (添加, 删除, 清空)
- ✅ 搜索 (主题, 颁发者, 序列号, 指纹)
- ✅ 枚举 (计数, 获取)
- ✅ 验证 (单证书验证, 链构建)

### ISSLSession - 会话管理
- ✅ 信息 (ID, 创建时间, 超时)
- ✅ 属性 (协议, 密码套件, 对端证书)
- ✅ 序列化/反序列化 (持久化)
- ✅ 复制 (Clone)

### ISSLContext - SSL上下文
- ✅ 协议配置 (TLS 1.0-1.3)
- ✅ 证书加载 (文件, 流, 内存)
- ✅ 密码套件管理
- ✅ 验证模式设置

### ISSLConnection - SSL连接
- ✅ 握手 (Connect, Accept)
- ✅ 数据传输 (Read, Write)
- ✅ 重协商 (Renegotiate)
- ✅ 证书获取 (对端证书, 证书链)
- ✅ 会话管理 (Get/Set Session)

---

## 🌍 跨平台支持

### Linux
- 后端: **OpenSSL** (libssl, libcrypto)
- 状态: ✅ 完全支持
- 版本: OpenSSL 1.1.x, 3.x

### Windows
- 后端: **WinSSL** (Schannel)
- 状态: ✅ 完全支持
- 优势: 零外部依赖

### macOS
- 后端: **OpenSSL**
- 状态: ⚠️ 理论支持
- 需要: 安装 OpenSSL

### Android
- 后端: **OpenSSL**
- 状态: ⚠️ 待验证
- 需要: 交叉编译配置

---

## 🔧 故障排除

### OpenSSL 未找到

**Linux:**
```bash
sudo apt-get install libssl-dev
```

**macOS:**
```bash
brew install openssl@3
```

### 编译错误

确保包含路径正确：
```bash
fpc -Fu/path/to/fafafa.ssl/src yourprogram.pas
```

或使用Lazarus IDE自动管理路径。

### Windows Schannel限制

WinSSL对某些高级功能支持有限，建议关键应用使用OpenSSL后端。

---

## 📖 API参考

完整API文档请参阅源码注释：

- `src/fafafa.ssl.abstract.intf.pas` - 接口定义
- `src/fafafa.ssl.abstract.types.pas` - 类型定义
- `src/fafafa.ssl.factory.pas` - 工厂模式入口

---

## ⚡ 性能优化建议

1. **使用会话复用** - 避免重复SSL握手
2. **连接池** - 复用连接对象
3. **选择合适的密码套件** - 平衡安全性和性能
4. **调整缓冲区大小** - 根据实际情况优化

---

## 🤝 贡献

欢迎贡献！请遵循项目的代码风格和设计理念。

---

## 📄 许可证

请查看 LICENSE 文件。

---

**最后更新**: 2025-11-03  
**版本**: 1.0  
**状态**: 生产就绪 ✅
