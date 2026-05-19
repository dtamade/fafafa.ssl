# fafafa.ssl 安全最佳实践指南

本文档提供了使用 fafafa.ssl 库时的安全最佳实践，包括证书固定、DANE 验证、密钥轮换等高级安全功能。

## 目录

1. [证书固定 (Certificate Pinning)](#证书固定-certificate-pinning)
2. [DANE (DNS-based Authentication)](#dane-dns-based-authentication)
3. [密钥轮换 (Certificate Rotation)](#密钥轮换-certificate-rotation)
4. [TLS 配置最佳实践](#tls-配置最佳实践)
5. [常见安全陷阱](#常见安全陷阱)

---

## 证书固定 (Certificate Pinning)

证书固定是一种额外的安全层，通过验证服务器证书或公钥的哈希值来防止中间人攻击。

### 为什么需要证书固定？

即使使用了标准的 TLS/SSL 验证，攻击者仍然可能通过以下方式进行中间人攻击：

- 伪造的 CA 证书
- 被入侵的 CA
- 系统信任存储被篡改

证书固定通过预先指定可信的证书或公钥哈希，提供了额外的保护层。

### OWASP 推荐的最佳实践

1. **使用公钥固定而非证书固定**
   - 公钥固定更灵活，证书更新时不需要更改 Pin
   - 证书固定在证书更新时需要同步更新应用

2. **至少配置 2 个 Pin（主要 + 备用）**
   - 主要 Pin：当前使用的证书/公钥
   - 备用 Pin：备用证书/公钥或中间 CA
   - 防止证书更新时服务中断

3. **Pin 应该在标准 X.509 验证之后进行**
   - 先验证证书链的有效性
   - 再验证 Pin 是否匹配

### 基本用法

```pascal
uses
  fafafa.ssl, fafafa.ssl.context.builder, fafafa.ssl.cert.pinning;

var
  Ctx: ISSLContext;
begin
  // 创建客户端上下文
  Ctx := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer
    .WithSystemRoots
    .BuildClient;

  // 添加主要 Pin（公钥固定）
  Ctx.AddCertificatePinBase64(
    'X3pGTSOuJeEVw989IJ/cEtXUEmy52zs1TZQrU06KUKg=',
    1,  // ptPublicKey
    'Primary Pin',
    False
  );

  // 添加备用 Pin
  Ctx.AddCertificatePinBase64(
    'YLh1dUR9y6Kja30RrAn7JKnbQG/uEtLMkBgFF2Fuihg=',
    1,  // ptPublicKey
    'Backup Pin',
    True
  );

  // 启用证书固定
  Ctx.SetCertificatePinningEnabled(True);
end;
```

### 如何提取证书 Pin

#### 方法 1：使用 OpenSSL 命令行

```bash
# 提取公钥 Pin（推荐）
openssl s_client -connect example.com:443 | \
  openssl x509 -pubkey -noout | \
  openssl pkey -pubin -outform der | \
  openssl dgst -sha256 -binary | \
  openssl enc -base64

# 提取证书 Pin
openssl s_client -connect example.com:443 | \
  openssl x509 -outform der | \
  openssl dgst -sha256 -binary | \
  openssl enc -base64
```

#### 方法 2：使用 fafafa.ssl API

这里走的是 OpenSSL raw certificate handle 路径，不是 backend-neutral helper。

```pascal
uses
  fafafa.ssl.cert.pinning,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.x509;

var
  Validator: TPinValidator;
  Cert: PX509;
  PubKeyHash: TBytes;
begin
  Validator := TPinValidator.Create;
  Cert := nil;
  try
    // 从证书文件加载
    Cert := LoadCertificateFromPEM('server.crt');

    // 提取公钥哈希
    PubKeyHash := Validator.ExtractPublicKeyHash(Cert);

    // 转换为 Base64
    WriteLn('Public Key Pin: ', TEncodingUtils.BytesToBase64(PubKeyHash));
  finally
    if Cert <> nil then
      X509_free(Cert);
    Validator.Free;
  end;
end;
```

### 高级用法：Pin 过期管理

```pascal
uses
  fafafa.ssl.cert.pinning;

var
  Validator: TPinValidator;
  Pin: TCertificatePin;
begin
  Validator := TPinValidator.Create;
  try
    // 添加带过期时间的 Pin
    Pin := TCertificatePin.FromBase64(
      'X3pGTSOuJeEVw989IJ/cEtXUEmy52zs1TZQrU06KUKg=',
      ptPublicKey,
      'Primary Pin',
      False
    );
    Pin.ExpiryDate := EncodeDate(2027, 12, 31);  // 设置过期时间

    // 检查 Pin 是否有效
    if Pin.IsValid then
      WriteLn('Pin is valid')
    else
      WriteLn('Pin has expired');
  finally
    Validator.Free;
  end;
end;
```

---

## DANE (DNS-based Authentication)

DANE (RFC 6698) 使用 DNSSEC 保护的 DNS TLSA 记录来验证 TLS 证书，提供了一种基于 DNS 的证书验证机制。

### 为什么使用 DANE？

- **去中心化信任**：不依赖传统的 CA 体系
- **防止 CA 被入侵**：即使 CA 被入侵，攻击者也无法伪造 DNSSEC 签名的 TLSA 记录
- **灵活的信任模型**：支持多种证书使用模式

### DANE TLSA 记录类型

DANE TLSA 记录由三个字段组成：

1. **Certificate Usage（证书使用）**
   - `0` (CA Constraint): PKIX-TA，CA 约束
   - `1` (Service Certificate Constraint): PKIX-EE，服务证书约束
   - `2` (Trust Anchor Assertion): DANE-TA，信任锚断言
   - `3` (Domain-Issued Certificate): DANE-EE，域颁发证书（推荐）

2. **Selector（选择器）**
   - `0`: 完整证书
   - `1`: SubjectPublicKeyInfo（推荐）

3. **Matching Type（匹配类型）**
   - `0`: 精确匹配（不推荐，数据量大）
   - `1`: SHA-256 哈希（推荐）
   - `2`: SHA-512 哈希

### 基本用法

```pascal
uses
  fafafa.ssl.dane;

var
  Validator: TDANEValidator;
  Cert: PX509;
begin
  // 创建 DANE 验证器
  Validator := TDANEValidator.Create('example.com', 443);
  try
    // 查询 DNS TLSA 记录
    if Validator.QueryTLSARecords('example.com', 443) then
    begin
      WriteLn('Found ', Validator.GetRecordCount, ' TLSA records');

      // 验证证书
      if Validator.ValidateCertificate(Cert) then
        WriteLn('Certificate validated successfully')
      else
        WriteLn('Certificate validation failed');
    end
    else
      WriteLn('No TLSA records found');
  finally
    Validator.Free;
  end;
end;
```

### 手动添加 TLSA 记录（用于测试）

```pascal
uses
  fafafa.ssl.dane;

var
  Validator: TDANEValidator;
  Hash: TBytes;
begin
  Validator := TDANEValidator.Create('example.com', 443);
  try
    // 手动添加 TLSA 记录
    // 3 1 1 表示：DANE-EE + SPKI + SHA-256
    SetLength(Hash, 32);
    // ... 填充哈希值 ...

    Validator.AddTLSARecord(
      duDomainIssuedCert,    // Usage: 3
      dsSubjectPublicKeyInfo, // Selector: 1
      dmSHA256,               // Matching: 1
      Hash
    );

    // 验证证书
    if Validator.ValidateCertificate(Cert) then
      WriteLn('Certificate validated successfully');
  finally
    Validator.Free;
  end;
end;
```

### 配置 DNSSEC 验证

```pascal
uses
  fafafa.ssl.dane;

var
  Validator: TDANEValidatorEx;
begin
  Validator := TDANEValidatorEx.Create('example.com', 443);
  try
    // 要求 DNSSEC 验证
    Validator.RequireDNSSEC := True;

    // 设置自定义 DNS 解析器
    Validator.SetDNSResolver('8.8.8.8');

    // 设置 DNS 查询超时
    Validator.SetDNSTimeout(5000);  // 5 秒

    // 验证 DNSSEC 链
    if Validator.VerifyDNSSEC then
      WriteLn('DNSSEC validation successful')
    else
      WriteLn('DNSSEC validation failed');
  finally
    Validator.Free;
  end;
end;
```

---

## 密钥轮换 (Certificate Rotation)

密钥轮换是定期更换 TLS 证书和私钥的过程，是保持系统安全的重要实践。

### 为什么需要密钥轮换？

- **限制密钥泄露的影响**：即使密钥被泄露，影响范围也有限
- **符合合规要求**：许多安全标准要求定期轮换密钥
- **防止密钥老化**：长期使用的密钥更容易被破解

### 基本用法

```pascal
uses
  fafafa.ssl.cert.rotation;

var
  Manager: TCertificateRotationManager;
  Config: TRotationConfig;
begin
  // 配置轮换参数
  Config.CertificatePath := '/path/to/cert.pem';
  Config.PrivateKeyPath := '/path/to/key.pem';
  Config.CheckIntervalSeconds := 3600;  // 每小时检查一次
  Config.AutoReloadOnChange := True;    // 文件变化时自动重载

  // 创建轮换管理器
  Manager := TCertificateRotationManager.Create(Config);
  try
    // 启动监控
    Manager.StartMonitoring;

    // 检查证书过期时间
    var DaysRemaining: Integer;
    if Manager.CheckExpiry(DaysRemaining) then
      WriteLn('Certificate expires in ', DaysRemaining, ' days');

    // 手动重载证书
    if Manager.ManualReload then
      WriteLn('Certificate reloaded successfully');

    // 停止监控
    Manager.StopMonitoring;
  finally
    Manager.Free;
  end;
end;
```

### 高级用法：自动轮换

```pascal
uses
  fafafa.ssl.cert.rotation;

var
  Manager: TCertRotationManager;
begin
  Manager := TCertRotationManager.Create;
  try
    // 配置自动轮换
    Manager.RotationInterval := 90;  // 每 90 天轮换
    Manager.GracePeriod := 7;        // 7 天宽限期
    Manager.AutoRotate := True;      // 启用自动轮换

    // 设置轮换策略
    Manager.Strategy := rsGraceful;  // 优雅轮换

    // 设置回调
    Manager.OnRotationStart := @HandleRotationStart;
    Manager.OnRotationComplete := @HandleRotationComplete;
    Manager.OnRotationFailed := @HandleRotationFailed;

    // 启动轮换管理器
    Manager.Start;

    // 检查轮换状态
    WriteLn(Manager.GetRotationInfo);
  finally
    Manager.Free;
  end;
end;

procedure HandleRotationStart(Sender: TObject);
begin
  WriteLn('Certificate rotation started');
end;

procedure HandleRotationComplete(Sender: TObject);
begin
  WriteLn('Certificate rotation completed successfully');
end;

procedure HandleRotationFailed(Sender: TObject; const AError: string);
begin
  WriteLn('Certificate rotation failed: ', AError);
end;
```

---

## TLS 配置最佳实践

### 1. 使用强加密套件

```pascal
uses
  fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
begin
  Ctx := TSSLContextBuilder.Create
    .WithTLS12And13              // 只使用 TLS 1.2 和 1.3
    .WithStrongCipherSuites      // 使用强加密套件
    .WithPerfectForwardSecrecy   // 启用完美前向保密
    .BuildClient;
end;
```

### 2. 启用证书验证

```pascal
uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
  ClientConn: ISSLClientConnection;
begin
  Ctx := TSSLContextBuilder.Create
    .WithVerifyPeer              // 验证对等方证书
    .WithSystemRoots             // 使用系统信任存储
    .BuildClient;

  Conn := Ctx.CreateConnection(Socket);
  ClientConn := Conn as ISSLClientConnection;
  ClientConn.SetServerName('example.com');

  if not Conn.Connect then
    raise Exception.Create('TLS handshake failed');
end;
```

也可以使用 `TSSLConnector.ConnectSocket(..., 'example.com')`，其本质同样是把 hostname/SNI 放到连接上，而不是 context 上。
这里展开 direct `ISSLConnection`，是为了把 hostname/SNI 的连接级责任显式写出来；如果你不需要这层低层控制，继续使用 `TSSLConnector.ConnectSocket(..., host)` 也同样正确。

### 3. 配置 OCSP Stapling

```pascal
uses
  fafafa.ssl.base,
  fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
begin
  Ctx := TSSLContextBuilder.Create
    .WithVerifyPeer
    .WithOCSPStapling(True)              // 请求 stapled OCSP response
    .WithOCSPStaplingRequired(False)     // 可选：高风险路径可改成 True
    .BuildClient;
end;
```

这条配置的当前语义是：

- `WithOCSPStapling(True)` 会让 client 在握手里请求 stapled OCSP response。
- `WithOCSPStaplingRequired(True)` 会在 `verify-peer` 的 non-resumed full-handshake path 上，对缺失或未通过当前有界校验的 stapled response fail-closed。
- 如果关闭 `verify-peer`，当前实现不会因为 `required` 被 fail-closed。
- 对 resumed TLS 1.3 path，`required` 也不会因为 resumed flight 缺少新的 stapled response 被触发。
- 握手完成后，可以通过 `ISSLOCSPStapling` 读取 raw response、verified bit 和状态文本。

示例：

```pascal
var
  OCSP: ISSLOCSPStapling;
begin
  if Supports(Conn, ISSLOCSPStapling, OCSP) then
    WriteLn(OCSP.GetOCSPResponseStatus);
end;
```

这里的最佳实践是把三件事分开看：client stapled-response request/consume、可选的 client online OCSP check，以及服务端 caller-provided stapled-response issuance。当前 public server-side path 已经可以通过 `WithServerOCSPStapledResponseFile(...)` 或 `ISSLServerOCSPStaplingContext` 配置材料，但它仍然不负责在线抓取或刷新 OCSP response。就 FreePascal backend capability 而言，`KnownIssues` 已不再把 OCSP / CT / resumption 列为剩余缺口，当前只剩 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.` 这条边界。

### 4. 使用会话恢复

```pascal
uses
  fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
begin
  Ctx := TSSLContextBuilder.Create
    .WithSessionCache            // 启用会话缓存
    .WithSessionTickets          // 启用会话票据
    .BuildClient;
end;
```

---

## 常见安全陷阱

### 1. ❌ 禁用证书验证

```pascal
// 错误示例 - 永远不要这样做！
Ctx := TSSLContextBuilder.Create
  .WithoutVerifyPeer             // 危险！
  .BuildClient;
```

**正确做法**：始终启用证书验证，如果遇到自签名证书，应该将其添加到信任存储。

### 2. ❌ 使用弱加密套件

```pascal
// 错误示例 - 不要使用弱加密
Ctx := TSSLContextBuilder.Create
  .WithSSL3                      // SSL 3.0 已被废弃
  .WithTLS10                     // TLS 1.0 已被废弃
  .BuildClient;
```

**正确做法**：只使用 TLS 1.2 和 TLS 1.3。

### 3. ❌ 忽略证书过期

```pascal
// 错误示例 - 不检查证书过期
var Conn: ISSLConnection;
begin
  Conn := Ctx.CreateConnection(Socket);
  // 没有检查证书有效期
end;
```

**正确做法**：定期检查证书过期时间，并设置自动轮换。

### 4. ❌ 硬编码密钥和证书

```pascal
// 错误示例 - 不要硬编码敏感信息
const
  PRIVATE_KEY = '-----BEGIN PRIVATE KEY-----...';
```

**正确做法**：从安全的配置文件或密钥管理系统加载。

### 5. ❌ 不使用证书固定

```pascal
// 错误示例 - 对于关键服务不使用证书固定
Ctx := TSSLContextBuilder.Create
  .WithVerifyPeer
  .BuildClient;
// 没有添加证书固定
```

**正确做法**：对于关键服务（如支付、认证），应该使用证书固定。

---

## 安全检查清单

在部署使用 fafafa.ssl 的应用之前，请确保：

- [ ] 只使用 TLS 1.2 和 TLS 1.3
- [ ] 启用了证书验证（VerifyPeer）
- [ ] 启用了主机名验证（VerifyHostname）
- [ ] 使用了强加密套件
- [ ] 对关键服务启用了证书固定
- [ ] 配置了至少 2 个证书 Pin（主要 + 备用）
- [ ] 设置了证书轮换机制
- [ ] 定期检查证书过期时间
- [ ] 不在代码中硬编码密钥和证书
- [ ] 在需要 stapled response 的客户端路径上启用了 `VerifyPeer` + OCSP stapling，并按风险决定是否使用 `required`
- [ ] 配置了适当的日志记录
- [ ] 测试了证书更新流程

---

## 参考资源

- [OWASP Certificate Pinning Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Pinning_Cheat_Sheet.html)
- [RFC 6698 - DANE TLSA](https://tools.ietf.org/html/rfc6698)
- [Mozilla SSL Configuration Generator](https://ssl-config.mozilla.org/)
- [SSL Labs Best Practices](https://github.com/ssllabs/research/wiki/SSL-and-TLS-Deployment-Best-Practices)

---

## 获取帮助

如果您在使用 fafafa.ssl 时遇到安全相关的问题，请：

1. 查看本文档和 API 文档
2. 查看示例代码（`examples/` 目录）
3. 提交 Issue 到 GitHub 仓库
4. 联系安全团队

**注意**：如果您发现了安全漏洞，请通过私密渠道报告，不要公开披露。
