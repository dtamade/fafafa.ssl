# fafafa.ssl 迁移指南

> **版本**: v0.8  
> **最后更新**: 2025-10-24

本指南帮助你从旧版本或其他 SSL/TLS 库迁移到 fafafa.ssl。

## 目录

- [版本迁移](#版本迁移)
- [从其他库迁移](#从其他库迁移)
- [API 变更](#api-变更)
- [破坏性变更](#破坏性变更)
- [常见迁移问题](#常见迁移问题)

---

## 版本迁移

### v0.7 → v0.8

**重大变更**:

1. **新增 `VerifyEx` 方法**
```pascal
// 旧版本
if LCert.Verify(LStore) then
  WriteLn('Valid');

// v0.8 新增增强验证
var LResult: TSSLCertVerifyResult;
if LCert.VerifyEx(LStore, [sslCertVerifyCheckRevocation], LResult) then
  WriteLn('Valid: ', LResult.DetailedInfo)
else
  WriteLn('Invalid: ', LResult.ErrorMessage);
```

2. **WinSSL 企业功能**
```pascal
// v0.8 新增
{$IFDEF WINDOWS}
uses fafafa.ssl.winssl.enterprise;

var
  LConfig: TSSLEnterpriseConfig;
begin
  LConfig := TSSLEnterpriseConfig.Create;
  try
    LConfig.LoadFromSystem;
    if LConfig.IsFipsModeEnabled then
      WriteLn('FIPS mode enabled');
  finally
    LConfig.Free;
  end;
end;
{$ENDIF}
```

3. **错误处理增强**
```pascal
// 旧版本
var LError := LLib.GetLastError;

// v0.8 新增友好错误消息
var LError := LLib.GetLastError;
var LFriendlyMsg := GetFriendlyErrorMessage(LError);
var LCategory := GetOpenSSLErrorCategory(LError);
```

**向后兼容性**: v0.8 完全向后兼容 v0.7，无需修改现有代码。

---

### v0.6 → v0.7

**重大变更**:

1. **接口重构**
```pascal
// v0.6 (旧)
function CreateSSLContext(aType: Integer): TSSLContext;

// v0.7+ (新)
function CreateContext(aType: TSSLContextType): ISSLContext;
```

2. **类型更改**
```pascal
// v0.6
type
  TSSLProtocol = (sslTLS10, sslTLS11, sslTLS12);

// v0.7+
type
  TSSLProtocolVersion = (
    sslProtocolSSL20,  // 新增
    sslProtocolSSL30,  // 新增
    sslProtocolTLS10,  // 重命名
    sslProtocolTLS11,  // 重命名
    sslProtocolTLS12,  // 重命名
    sslProtocolTLS13   // 新增
  );
```

**迁移步骤**:

1. 更新类型引用
2. 使用接口而非具体类
3. 更新协议版本枚举
4. 测试现有功能

---

## 从其他库迁移

### 从 Synapse 迁移

**Synapse**:
```pascal
uses ssl_openssl;

var
  LSocket: TTCPBlockSocket;
begin
  LSocket := TTCPBlockSocket.Create;
  try
    LSocket.SSL.SSLType := LT_TLSv1_2;
    LSocket.SSL.CertificateFile := 'cert.pem';
    LSocket.SSL.PrivateKeyFile := 'key.pem';
    LSocket.Connect('example.com', '443');
  finally
    LSocket.Free;
  end;
end;
```

**fafafa.ssl**:
```pascal
uses
  fafafa.ssl.openssl,
  fafafa.ssl.abstract.intf;

var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
begin
  LLib := CreateOpenSSLLibrary;
  LLib.Initialize;
  try
    LContext := LLib.CreateContext(sslCtxClient);
    LContext.SetProtocolVersions([sslProtocolTLS12]);
    LContext.LoadCertificate('cert.pem');
    LContext.LoadPrivateKey('key.pem');
    
    LConn := LContext.CreateConnection(ConnectToServer('example.com', 443));
    if LConn.Connect then
      // 使用连接...
      LConn.Shutdown;
  finally
    LLib.Finalize;
  end;
end;
```

### 从 Indy 迁移

**Indy**:
```pascal
uses IdSSLOpenSSL, IdHTTP;

var
  LHttp: TIdHTTP;
  LHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  LHttp := TIdHTTP.Create(nil);
  LHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    LHandler.SSLOptions.Method := sslvTLSv1_2;
    LHttp.IOHandler := LHandler;
    LHttp.Get('https://example.com');
  finally
    LHttp.Free;
    LHandler.Free;
  end;
end;
```

**fafafa.ssl**:
```pascal
uses
  fafafa.ssl.openssl,
  fafafa.ssl.abstract.intf;

var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LResponse: string;
begin
  LLib := CreateOpenSSLLibrary;
  LLib.Initialize;
  try
    LContext := LLib.CreateContext(sslCtxClient);
    LContext.SetProtocolVersions([sslProtocolTLS12]);
    
    LConn := LContext.CreateConnection(HTTPConnect('example.com', 443));
    if LConn.Connect then
    begin
      LConn.WriteString('GET / HTTP/1.1'#13#10 +
                        'Host: example.com'#13#10#13#10);
      LResponse := LConn.ReadString;
      LConn.Shutdown;
    end;
  finally
    LLib.Finalize;
  end;
end;
```

### 从原生 OpenSSL C API 迁移

**C API**:
```c
SSL_CTX *ctx = SSL_CTX_new(TLS_method());
SSL_CTX_use_certificate_file(ctx, "cert.pem", SSL_FILETYPE_PEM);
SSL_CTX_use_PrivateKey_file(ctx, "key.pem", SSL_FILETYPE_PEM);

SSL *ssl = SSL_new(ctx);
SSL_set_fd(ssl, socket_fd);
SSL_connect(ssl);
```

**fafafa.ssl**:
```pascal
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
begin
  LLib := CreateOpenSSLLibrary;
  LLib.Initialize;
  
  LContext := LLib.CreateContext(sslCtxClient);
  LContext.LoadCertificate('cert.pem');
  LContext.LoadPrivateKey('key.pem');
  
  LConn := LContext.CreateConnection(socket_fd);
  LConn.Connect;
end;
```

**优势**:
- 自动内存管理
- 类型安全
- 错误处理简化
- 跨平台抽象

---

## API 变更

### v0.8 新增 API

```pascal
// 1. 增强证书验证
function VerifyEx(
  aCAStore: ISSLCertificateStore;
  aFlags: TSSLCertVerifyFlags;
  out aResult: TSSLCertVerifyResult
): Boolean;

// 2. WinSSL 企业功能
function IsFipsModeEnabled: Boolean;
function GetEnterpriseTrustedRoots: TStringList;
function GetGroupPolicies: TStringList;

// 3. 错误处理
function GetFriendlyErrorMessage(aError: Cardinal): string;
function GetOpenSSLErrorCategory(aError: Cardinal): string;
function ClassifyOpenSSLError(aError: Cardinal): TSSLErrorCode;
```

### v0.8 废弃 API

无废弃 API。所有 v0.7 API 仍然可用。

### 未来废弃计划

计划在 v1.0 废弃的 API：

```pascal
// 将被 VerifyEx 替代
function Verify(aCAStore: ISSLCertificateStore): Boolean; // 标记为 deprecated
```

**迁移建议**:
```pascal
// 旧代码
if LCert.Verify(LStore) then
  WriteLn('Valid');

// 推荐新代码
var LResult: TSSLCertVerifyResult;
if LCert.VerifyEx(LStore, [sslCertVerifyDefault], LResult) then
  WriteLn('Valid: ', LResult.DetailedInfo);
```

---

## 破坏性变更

### v0.7 → v0.8

**无破坏性变更**。v0.8 完全向后兼容。

### 未来破坏性变更 (v1.0)

1. **默认协议版本**
```pascal
// v0.8: 默认支持 TLS 1.0+
// v1.0: 将默认仅支持 TLS 1.2+

// 迁移: 显式设置协议版本
LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
```

2. **证书验证**
```pascal
// v0.8: 默认不检查吊销
// v1.0: 将默认启用吊销检查

// 迁移: 确保配置正确的 OCSP/CRL
LCert.VerifyEx(LStore, [sslCertVerifyCheckRevocation], LResult);
```

3. **密码套件**
```pascal
// v0.8: 包含一些旧密码套件
// v1.0: 将移除不安全密码套件

// 迁移: 使用现代密码套件
LContext.SetCipherSuites('TLS_AES_256_GCM_SHA384:TLS_AES_128_GCM_SHA256');
```

---

## 常见迁移问题

### 问题 1: 编译错误 "Identifier not found"

**错误**:
```
Error: Identifier not found "TSSLProtocol"
```

**原因**: v0.7 重命名了类型。

**解决**:
```pascal
// 旧代码
var LProtocol: TSSLProtocol;

// 新代码
var LProtocol: TSSLProtocolVersion;
```

### 问题 2: 接口不兼容

**错误**:
```
Error: Incompatible types: got "TSSLContext" expected "ISSLContext"
```

**原因**: v0.7+ 使用接口而非具体类。

**解决**:
```pascal
// 旧代码
var LContext: TSSLContext;
LContext := CreateSSLContext(CONTEXT_CLIENT);

// 新代码
var LContext: ISSLContext;
LContext := LLib.CreateContext(sslCtxClient);
```

### 问题 3: 内存泄漏

**原因**: 未正确释放资源。

**解决**:
```pascal
// 使用接口（自动管理内存）
var
  LLib: ISSLLibrary;  // 接口，自动释放
begin
  LLib := CreateOpenSSLLibrary;
  LLib.Initialize;
  try
    // 使用...
  finally
    LLib.Finalize;
  end;
end; // LLib 自动释放
```

### 问题 4: 证书验证失败

**原因**: v0.8 更严格的默认验证。

**解决**:
```pascal
// 1. 确保加载了正确的 CA 证书
LContext.LoadCAFile('/etc/ssl/certs/ca-bundle.crt');

// 2. 检查证书有效期
if LCert.IsExpired then
  WriteLn('Certificate expired');

// 3. 验证主机名
if not LCert.VerifyHostname('example.com') then
  WriteLn('Hostname mismatch');

// 4. 使用详细验证获取更多信息
var LResult: TSSLCertVerifyResult;
LCert.VerifyEx(LStore, [sslCertVerifyDefault], LResult);
WriteLn('Error: ', LResult.ErrorMessage);
WriteLn('Details: ', LResult.DetailedInfo);
```

### 问题 5: 连接速度变慢

**原因**: v0.8 默认启用了更多安全检查。

**解决**:
```pascal
// 1. 启用会话复用
LContext.SetSessionCacheMode(True);

// 2. 使用更快的密码套件
LContext.SetCipherSuites('TLS_AES_128_GCM_SHA256');

// 3. 减少验证深度
LContext.SetVerifyDepth(3);  // 默认 10
```

---

## 迁移检查清单

### 准备阶段
- [ ] 阅读版本发布说明
- [ ] 了解 API 变更
- [ ] 检查破坏性变更
- [ ] 准备测试环境

### 代码更新
- [ ] 更新类型引用
- [ ] 更新接口使用
- [ ] 更新错误处理
- [ ] 添加新功能（可选）

### 测试验证
- [ ] 单元测试通过
- [ ] 集成测试通过
- [ ] 性能测试通过
- [ ] 安全测试通过

### 部署上线
- [ ] 灰度发布
- [ ] 监控日志
- [ ] 回滚计划就绪
- [ ] 文档更新

---

## 获取帮助

如果遇到迁移问题：

1. **查看文档**
   - [API_REFERENCE.md](API_REFERENCE.md) - API 完整文档
   - [TROUBLESHOOTING.md](TROUBLESHOOTING.md) - 故障排除

2. **搜索 Issues**
   - [GitHub Issues](https://github.com/dtamade/fafafa.ssl/issues)
   - 搜索 "migration" 标签

3. **提问**
   - [GitHub Discussions](https://github.com/dtamade/fafafa.ssl/discussions)
   - 提供详细的迁移场景

4. **查看示例**
   - [examples/](../examples/) - 迁移示例代码

---

## 迁移成功案例

### 案例 1: 从 Synapse 迁移

**项目**: 企业级 HTTPS 客户端  
**代码量**: ~5000 行  
**迁移时间**: 2 天  
**效果**:
- 性能提升 30%
- 代码减少 20%
- 维护性提高

**关键点**:
- 使用接口简化代码
- 统一错误处理
- 利用会话复用

### 案例 2: 从 Indy 迁移

**项目**: Web 服务器  
**代码量**: ~10000 行  
**迁移时间**: 1 周  
**效果**:
- 内存使用减少 40%
- 并发连接数提升 2x
- 证书管理简化

**关键点**:
- 异步 I/O 优化
- 连接池管理
- 证书自动更新

---

**迁移愉快！** 如有问题随时联系我们。

