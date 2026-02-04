# fafafa.ssl 安全指南

> **版本**: v0.8  
> **最后更新**: 2025-10-24

本指南提供使用 fafafa.ssl 构建安全应用程序的最佳实践和安全建议。

## 目录

- [安全原则](#安全原则)
- [TLS/SSL 配置](#tlsssl-配置)
- [证书管理](#证书管理)
- [密钥管理](#密钥管理)
- [认证与授权](#认证与授权)
- [数据保护](#数据保护)
- [安全审计](#安全审计)
- [漏洞防护](#漏洞防护)

---

## 安全原则

### 纵深防御

1. **多层安全控制**
   - 网络层：防火墙、VPN
   - 传输层：TLS 1.2/1.3
   - 应用层：认证、授权
   - 数据层：加密、完整性

2. **最小权限原则**
   - 仅授予必要的权限
   - 定期审查和撤销不需要的权限
   - 使用专用服务账户

3. **安全默认配置**
   - 默认拒绝策略
   - 禁用不安全功能
   - 启用所有安全选项

---

## TLS/SSL 配置

### 推荐的协议版本

✅ **推荐**:
```pascal
// 仅使用现代安全协议
LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
```

❌ **禁止**:
```pascal
// 不要使用已废弃的协议
LContext.SetProtocolVersions([
  sslProtocolSSL20,  // 严重漏洞
  sslProtocolSSL30,  // POODLE 攻击
  sslProtocolTLS10,  // BEAST 攻击
  sslProtocolTLS11   // 已过时
]);
```

### 密码套件配置

**Mozilla 现代配置**（推荐）:
```pascal
// 仅支持 TLS 1.3 密码套件
LContext.SetCipherSuites(
  'TLS_AES_256_GCM_SHA384:' +
  'TLS_AES_128_GCM_SHA256:' +
  'TLS_CHACHA20_POLY1305_SHA256'
);

// TLS 1.2 向后兼容
LContext.SetCipherList(
  'ECDHE-ECDSA-AES256-GCM-SHA384:' +
  'ECDHE-RSA-AES256-GCM-SHA384:' +
  'ECDHE-ECDSA-CHACHA20-POLY1305:' +
  'ECDHE-RSA-CHACHA20-POLY1305:' +
  'ECDHE-ECDSA-AES128-GCM-SHA256:' +
  'ECDHE-RSA-AES128-GCM-SHA256'
);
```

**禁用的密码套件**:
```pascal
// 不要使用这些不安全的密码套件
// - NULL cipher (无加密)
// - EXPORT cipher (弱加密)
// - DES/3DES (已过时)
// - RC4 (已破解)
// - MD5 (碰撞攻击)
// - anon (无身份验证)
```

### 完美前向保密 (PFS)

```pascal
// 使用 ECDHE 或 DHE 密码套件
LContext.SetCipherList('ECDHE+AESGCM:DHE+AESGCM:!RSA');

// 配置 DH 参数（如果使用 DHE）
LContext.LoadDHParams('dhparam.pem');  // 至少 2048 位
```

### HSTS (HTTP Strict Transport Security)

```pascal
// 在 HTTP 响应中添加 HSTS 头
procedure AddSecurityHeaders(aResponse: THTTPResponse);
begin
  aResponse.SetHeader('Strict-Transport-Security', 
    'max-age=31536000; includeSubDomains; preload');
  aResponse.SetHeader('X-Content-Type-Options', 'nosniff');
  aResponse.SetHeader('X-Frame-Options', 'DENY');
  aResponse.SetHeader('X-XSS-Protection', '1; mode=block');
end;
```

---

## 证书管理

### 证书获取

**推荐方式**:
1. **Let's Encrypt**（免费、自动化）
2. **商业 CA**（Extended Validation）
3. **企业 CA**（内部使用）

❌ **避免**:
- 自签名证书（生产环境）
- 过期证书
- 弱签名算法（MD5、SHA-1）

### 证书验证

**客户端验证**:
```pascal
// 严格验证服务器证书
LContext.SetVerifyMode([sslVerifyPeer]);

// 加载受信任的 CA 证书
LContext.LoadCAFile('/etc/ssl/certs/ca-bundle.crt');

// 验证主机名
if not LConn.GetPeerCertificate.VerifyHostname('example.com') then
  raise ESSLException.Create('Hostname mismatch');

// 增强验证（含吊销检查）
var LResult: TSSLCertVerifyResult;
if not LCert.VerifyEx(LStore, [
  sslCertVerifyCheckRevocation,
  sslCertVerifyCheckOCSP
], LResult) then
  raise ESSLException.Create(LResult.ErrorMessage);
```

**服务器验证**（双向TLS）:
```pascal
// 要求客户端证书
LContext.SetVerifyMode([
  sslVerifyPeer,
  sslVerifyFailIfNoPeerCert
]);

// 加载客户端 CA
LContext.LoadCAFile('client-ca.crt');

// 验证客户端证书
var LClientCert := LConn.GetPeerCertificate;
if not LClientCert.IsValid then
  raise ESSLException.Create('Invalid client certificate');
```

### 证书固定 (Certificate Pinning)

```pascal
// 固定证书指纹（高安全场景）
const
  EXPECTED_SHA256_FINGERPRINT = 
    'E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855';

procedure VerifyCertificatePinning(aCert: ISSLCertificate);
var
  LFingerprint: string;
begin
  LFingerprint := aCert.GetFingerprintSHA256;
  if LFingerprint <> EXPECTED_SHA256_FINGERPRINT then
    raise ESSLException.Create('Certificate pinning failed');
end;
```

### 证书轮换

```pascal
// 自动证书更新（Let's Encrypt）
procedure AutoRenewCertificate;
begin
  // 1. 检查证书有效期
  var LCert := LoadCertificate('server.crt');
  var LExpiryDate := LCert.GetNotAfter;
  
  // 2. 提前 30 天更新
  if DaysBetween(Now, LExpiryDate) < 30 then
  begin
    // 3. 申请新证书
    RequestNewCertificate;
    
    // 4. 验证新证书
    if ValidateNewCertificate then
    begin
      // 5. 重新加载配置
      ReloadSSLConfig;
      
      // 6. 记录日志
      LogInfo('Certificate renewed successfully');
    end;
  end;
end;
```

---

## 密钥管理

### 密钥生成

```bash
# RSA 密钥（至少 2048 位，推荐 3072 或 4096）
openssl genrsa -out server.key 4096

# ECDSA 密钥（更快，更小，推荐）
openssl ecparam -genkey -name secp384r1 -out server.key

# Ed25519 密钥（现代，最快）
openssl genpkey -algorithm Ed25519 -out server.key
```

### 密钥保护

**文件权限**:
```bash
# 私钥应设置为最严格权限
chmod 400 server.key
chown myapp:myapp server.key

# 禁止其他用户读取
chmod 700 /path/to/certs/
```

**加密存储**:
```pascal
// 使用密码保护私钥
procedure GenerateEncryptedKey;
begin
  // 生成加密的私钥
  ExecuteCommand('openssl genrsa -aes256 -out server.key 4096');
  
  // 解密时需要密码
  LContext.LoadPrivateKey('server.key', 'strong-password');
end;
```

**硬件安全模块 (HSM)**:
```pascal
// 使用 HSM 存储私钥（企业级）
procedure UseHSM;
begin
  // 配置 PKCS#11 引擎
  LoadPKCS11Engine('/usr/lib/libpkcs11.so');
  
  // 从 HSM 加载密钥
  LPrivKey := LoadKeyFromHSM('slot-0', 'key-id', 'pin');
  LContext.SetPrivateKey(LPrivKey);
end;
```

### 密钥轮换

```pascal
// 定期轮换密钥（每年）
procedure RotateKeys;
begin
  // 1. 生成新密钥对
  GenerateNewKeyPair;
  
  // 2. 获取新证书
  RequestCertificateWithNewKey;
  
  // 3. 测试新配置
  if TestNewConfiguration then
  begin
    // 4. 部署新密钥和证书
    DeployNewKeyAndCert;
    
    // 5. 撤销旧证书
    RevokePreviousCertificate;
    
    // 6. 安全删除旧密钥
    SecureDeleteOldKey;
  end;
end;
```

---

## 认证与授权

### 客户端认证

```pascal
// 使用客户端证书认证
procedure AuthenticateClient(aConn: ISSLConnection);
var
  LClientCert: ISSLCertificate;
  LCommonName: string;
begin
  // 获取客户端证书
  LClientCert := aConn.GetPeerCertificate;
  if LClientCert = nil then
    raise EAuthenticationFailed.Create('No client certificate');
  
  // 验证证书
  if not LClientCert.Verify(FCAStore) then
    raise EAuthenticationFailed.Create('Certificate verification failed');
  
  // 提取身份信息
  LCommonName := ExtractCommonName(LClientCert.GetSubject);
  
  // 授权检查
  if not IsAuthorized(LCommonName) then
    raise EAuthorizationFailed.Create('Access denied');
  
  // 创建会话
  CreateAuthenticatedSession(LCommonName);
end;
```

### API 密钥认证

```pascal
// API 密钥验证
procedure ValidateAPIKey(const aAPIKey: string);
var
  LHash: string;
  LExpiry: TDateTime;
begin
  // 1. 验证格式
  if not IsValidAPIKeyFormat(aAPIKey) then
    raise EInvalidAPIKey.Create('Invalid API key format');
  
  // 2. 查询数据库
  if not LookupAPIKey(aAPIKey, LHash, LExpiry) then
    raise EInvalidAPIKey.Create('Unknown API key');
  
  // 3. 检查过期
  if LExpiry < Now then
    raise EInvalidAPIKey.Create('API key expired');
  
  // 4. 验证哈希
  if not VerifyHash(aAPIKey, LHash) then
    raise EInvalidAPIKey.Create('Invalid API key');
  
  // 5. 速率限制
  if ExceedsRateLimit(aAPIKey) then
    raise ERateLimitExceeded.Create('Rate limit exceeded');
end;
```

---

## 数据保护

### 传输中数据

```pascal
// 确保所有数据通过 TLS 传输
procedure SendSensitiveData(aConn: ISSLConnection; const aData: string);
begin
  // 验证连接是否加密
  if not aConn.IsConnected then
    raise Exception.Create('Not connected');
  
  if aConn.GetProtocolVersion < sslProtocolTLS12 then
    raise Exception.Create('Insecure protocol version');
  
  // 发送数据
  aConn.WriteString(aData);
end;
```

### 静态数据

```pascal
// 加密敏感数据
function EncryptSensitiveData(const aPlainText: string; const aKey: TBytes): TBytes;
begin
  Result := AES256_GCM_Encrypt(aPlainText, aKey);
end;

// 安全擦除内存
procedure SecureWipeMemory(var aData: TBytes);
begin
  if Length(aData) > 0 then
  begin
    FillChar(aData[0], Length(aData), 0);
    ReallocMem(aData, 0);
  end;
end;

// 使用后立即清除敏感数据
procedure ProcessPassword(const aPassword: string);
var
  LPasswordBytes: TBytes;
begin
  LPasswordBytes := BytesOf(aPassword);
  try
    // 处理密码...
  finally
    SecureWipeMemory(LPasswordBytes);
  end;
end;
```

---

## 安全审计

### 日志记录

```pascal
// 记录安全事件
procedure LogSecurityEvent(const aEvent: string; aLevel: TSecurityLevel);
var
  LLogEntry: TJSONObject;
begin
  LLogEntry := TJSONObject.Create;
  try
    LLogEntry.Add('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', NowUTC));
    LLogEntry.Add('event', aEvent);
    LLogEntry.Add('level', SecurityLevelToString(aLevel));
    LLogEntry.Add('source_ip', GetClientIP);
    LLogEntry.Add('user', GetCurrentUser);
    
    // 写入审计日志
    AppendToAuditLog(LLogEntry);
    
    // 高危事件发送告警
    if aLevel >= sslSecurityCritical then
      SendSecurityAlert(aEvent);
  finally
    LLogEntry.Free;
  end;
end;

// 记录的事件类型
// - 认证失败
// - 授权失败
// - 证书验证失败
// - 异常连接
// - 配置更改
```

### 入侵检测

```pascal
// 检测异常行为
procedure DetectAnomalies;
begin
  // 1. 短时间内大量失败连接
  if GetFailedConnectionsPerMinute > 10 then
    BlockIPAddress(GetClientIP, 3600);  // 封禁 1 小时
  
  // 2. 非法证书访问
  if DetectInvalidCertificate then
    LogSecurityEvent('Invalid certificate detected', sslSecurityWarning);
  
  // 3. 协议降级攻击
  if DetectProtocolDowngrade then
    LogSecurityEvent('Protocol downgrade attack detected', sslSecurityCritical);
end;
```

---

## 漏洞防护

### BEAST 攻击防护

```pascal
// 禁用 CBC 模式密码套件（TLS 1.0）
LContext.SetCipherList('!CBC');

// 或仅使用 TLS 1.2+
LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
```

### POODLE 攻击防护

```pascal
// 禁用 SSL 3.0
LContext.SetProtocolVersions([
  sslProtocolTLS10,  // 如果必须支持旧客户端
  sslProtocolTLS11,
  sslProtocolTLS12,
  sslProtocolTLS13
]);
```

### Heartbleed 防护

```bash
# 升级 OpenSSL 到修复版本
# OpenSSL 1.0.1g+ 或 1.0.2+
openssl version
```

### 重协商攻击防护

```pascal
// 禁用不安全的重协商
if Assigned(SSL_CTX_set_options) then
  SSL_CTX_set_options(FSSLCtx, SSL_OP_NO_RENEGOTIATION);
```

### 时序攻击防护

```pascal
// 使用恒定时间比较
function ConstantTimeCompare(const a, b: TBytes): Boolean;
var
  i, diff: Integer;
begin
  diff := Length(a) xor Length(b);
  for i := 0 to Min(Length(a), Length(b)) - 1 do
    diff := diff or (a[i] xor b[i]);
  Result := diff = 0;
end;
```

---

## 安全检查清单

### 部署前

- [ ] 使用 TLS 1.2/1.3
- [ ] 配置强密码套件
- [ ] 启用完美前向保密
- [ ] 证书验证已启用
- [ ] 私钥权限正确（400）
- [ ] 禁用不安全协议
- [ ] HSTS 已配置
- [ ] 安全头已添加
- [ ] 日志记录已启用
- [ ] 速率限制已配置

### 运行时

- [ ] 监控证书到期
- [ ] 检测异常连接
- [ ] 审计安全事件
- [ ] 定期安全扫描
- [ ] 及时应用安全补丁
- [ ] 密钥定期轮换
- [ ] 备份审计日志
- [ ] 测试灾难恢复

### 测试工具

```bash
# SSL Labs 在线测试
https://www.ssllabs.com/ssltest/

# testssl.sh（本地测试）
./testssl.sh --full example.com:443

# nmap 扫描
nmap --script ssl-enum-ciphers -p 443 example.com

# OpenSSL 测试
openssl s_client -connect example.com:443 -tls1_3
```

---

## 安全资源

- **OWASP Top 10**: https://owasp.org/www-project-top-ten/
- **CWE Top 25**: https://cwe.mitre.org/top25/
- **Mozilla SSL 配置**: https://ssl-config.mozilla.org/
- **SSL Labs 最佳实践**: https://github.com/ssllabs/research/wiki/SSL-and-TLS-Deployment-Best-Practices

---

## 报告安全漏洞

如发现安全漏洞，请：

1. **不要**公开发布
2. 发送邮件到：security@example.com
3. 包含详细信息和复现步骤
4. 等待回复（通常 48 小时内）

我们承诺：
- 及时响应
- 负责任地披露
- 致谢贡献者

---

**保持安全！** 🔒

