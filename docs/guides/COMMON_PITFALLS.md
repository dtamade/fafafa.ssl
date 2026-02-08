# fafafa.ssl 常见坑与解决方案

> **目标**：帮助开发者避免最常见的 TLS/SSL 开发陷阱

## 1. OpenSSL 版本差异

### 坑：OpenSSL 1.1.1 vs 3.x API 不兼容

**症状**：
- 编译通过但运行时崩溃
- 某些函数返回意外结果
- `ERR_get_error()` 返回未知错误码

**原因**：OpenSSL 3.x 弃用了大量 1.1.1 API

**解决方案**：

```pascal
// 检测 OpenSSL 版本
uses fafafa.ssl.openssl.api;

if OpenSSLVersion >= $30000000 then
  WriteLn('OpenSSL 3.x')
else if OpenSSLVersion >= $10101000 then
  WriteLn('OpenSSL 1.1.1');
```

**常见弃用 API 对照**：

| 1.1.1 API | 3.x 替代 | 说明 |
|-----------|----------|------|
| `EVP_MD_CTX_create` | `EVP_MD_CTX_new` | 哈希上下文 |
| `EVP_MD_CTX_destroy` | `EVP_MD_CTX_free` | 释放上下文 |
| `RSA_generate_key` | `EVP_PKEY_keygen` | 密钥生成 |
| `SSL_CTX_set_tmp_dh` | `SSL_CTX_set0_tmp_dh_pkey` | DH 参数 |

**最佳实践**：
- 使用 fafafa.ssl 高层封装，自动处理版本差异
- 参考 `docs/reference/OPENSSL_1_1_1_VS_3X_DIFF_AND_REGRESSION.md`

---

## 2. 证书存储路径

### 坑：跨平台证书路径不一致

**症状**：
- Linux 上正常，macOS/Windows 上证书验证失败
- `SSL_CTX_load_verify_locations` 返回错误

**各平台默认路径**：

| 平台 | 系统根证书路径 |
|------|----------------|
| Debian/Ubuntu | `/etc/ssl/certs/ca-certificates.crt` |
| RHEL/CentOS | `/etc/pki/tls/certs/ca-bundle.crt` |
| macOS | `/etc/ssl/cert.pem` 或 Keychain |
| Windows | 系统证书存储（无文件路径） |

**解决方案**：

```pascal
// 方法1：使用 WithSystemRoots（推荐）
Ctx := TSSLContextBuilder.Create
  .WithSystemRoots  // 自动检测平台
  .BuildClient;

// 方法2：显式指定路径
{$IFDEF LINUX}
  CAPath := '/etc/ssl/certs';
{$ENDIF}
{$IFDEF DARWIN}
  CAPath := '/etc/ssl/cert.pem';
{$ENDIF}
{$IFDEF WINDOWS}
  // Windows 使用系统存储，无需路径
{$ENDIF}
```

### 坑：macOS brew OpenSSL 路径问题

**症状**：
- `brew install openssl@3` 后仍找不到库
- 程序加载系统自带的 LibreSSL 而非 OpenSSL

**原因**：macOS 不将 brew OpenSSL 链接到系统路径

**解决方案**：

```bash
# 方法1：设置环境变量
export DYLD_LIBRARY_PATH=/opt/homebrew/opt/openssl@3/lib:$DYLD_LIBRARY_PATH

# 方法2：编译时指定 rpath
fpc -k"-rpath /opt/homebrew/opt/openssl@3/lib" your_program.pas

# 方法3：代码中显式指定
TSSLLibrary.Instance.SetCustomLibraryPath('/opt/homebrew/opt/openssl@3/lib');
```

---

## 3. SNI 与 Hostname 验证

### 坑：忘记设置 SNI 导致握手失败

**症状**：
- 连接某些服务器时握手失败
- 收到错误证书（默认证书而非目标域名证书）
- 错误：`SSL_ERROR_SSL` 或 `certificate verify failed`

**原因**：现代服务器依赖 SNI 选择正确证书

**解决方案**：

```pascal
// ❌ 错误：未设置 SNI
Conn := Ctx.CreateConnection(Socket);
Conn.Connect;  // 可能失败或收到错误证书

// ✅ 正确：设置 SNI
Conn := Ctx.CreateConnection(Socket);
Conn.SetServerName('api.example.com');  // 关键！
Conn.Connect;
```

### 坑：Hostname 验证被绕过

**症状**：
- 连接成功但实际连到了错误服务器
- 中间人攻击风险

**原因**：未启用 hostname 验证

**解决方案**：

```pascal
// ✅ 正确：启用 hostname 验证
Ctx := TSSLContextBuilder.Create
  .WithVerifyPeer           // 验证证书
  .WithVerifyHostname       // 验证 hostname（关键！）
  .WithSystemRoots
  .BuildClient;

// 或使用 Connector（自动处理）
TLS := TSSLConnector.FromContext(Ctx);
Stream := TLS.ConnectSocket(Socket, 'api.example.com');  // 自动设置 SNI + hostname 验证
```

---

## 4. 证书验证

### 坑：开发时禁用验证，生产忘记启用

**症状**：
- 开发环境正常
- 生产环境遭受中间人攻击

**解决方案**：

```pascal
// ❌ 危险：禁用验证（仅限开发调试）
{$IFDEF DEBUG}
Ctx := TSSLContextBuilder.Create
  .WithVerifyNone  // 仅调试！
  .BuildClient;
{$ELSE}
// ✅ 生产：必须启用验证
Ctx := TSSLContextBuilder.Create
  .WithVerifyPeer
  .WithVerifyHostname
  .WithSystemRoots
  .BuildClient;
{$ENDIF}
```

### 坑：自签名证书验证失败

**症状**：
- 内部服务使用自签名证书
- `certificate verify failed: self signed certificate`

**解决方案**：

```pascal
// 方法1：添加自签名证书到信任列表
Ctx := TSSLContextBuilder.Create
  .WithVerifyPeer
  .WithCAFile('/path/to/internal-ca.pem')  // 添加内部 CA
  .BuildClient;

// 方法2：证书固定（更安全）
Ctx := TSSLContextBuilder.Create
  .WithVerifyPeer
  .WithPinnedCertificate('/path/to/server.pem')
  .BuildClient;
```

---

## 5. 协议版本

### 坑：使用过时协议

**症状**：
- 连接被服务器拒绝
- 安全扫描报告漏洞

**原因**：TLS 1.0/1.1 已被弃用

**解决方案**：

```pascal
// ❌ 不安全：允许旧协议
Ctx := TSSLContextBuilder.Create
  .WithMinVersion(sslTLS10)  // 危险！
  .BuildClient;

// ✅ 安全：仅允许 TLS 1.2+
Ctx := TSSLContextBuilder.Create
  .WithTLS12And13  // 推荐
  .BuildClient;

// 或显式指定
Ctx := TSSLContextBuilder.Create
  .WithMinVersion(sslTLS12)
  .WithMaxVersion(sslTLS13)
  .BuildClient;
```

---

## 6. 内存管理

### 坑：OpenSSL 对象泄漏

**症状**：
- 内存持续增长
- 长时间运行后崩溃

**常见泄漏点**：

```pascal
// ❌ 泄漏：未释放 X509
var
  Cert: PX509;
begin
  Cert := d2i_X509(nil, @Data, Length(Data));
  // 使用 Cert...
  // 忘记释放！
end;

// ✅ 正确：释放 X509
var
  Cert: PX509;
begin
  Cert := d2i_X509(nil, @Data, Length(Data));
  try
    // 使用 Cert...
  finally
    X509_free(Cert);  // 必须释放
  end;
end;
```

**最佳实践**：
- 使用 fafafa.ssl 高层接口（自动管理内存）
- 使用 HeapTrc 检测泄漏：`fpc -gh -gl program.pas`

---

## 7. 多线程

### 坑：OpenSSL 线程安全配置

**症状**：
- 多线程环境下随机崩溃
- 数据损坏

**解决方案**：

```pascal
// OpenSSL 1.1.1+ 默认线程安全
// 但仍需注意：

// ❌ 错误：多线程共享连接
var
  SharedConn: ISSLConnection;  // 危险！

// ✅ 正确：每线程独立连接
procedure ThreadProc;
var
  Conn: ISSLConnection;
begin
  Conn := Ctx.CreateConnection(Socket);  // 线程独立
  // ...
end;
```

---

## 8. 错误处理

### 坑：忽略 OpenSSL 错误

**症状**：
- 操作失败但不知道原因
- 难以调试

**解决方案**：

```pascal
uses fafafa.ssl.openssl.api.err;

// 获取详细错误信息
procedure CheckSSLError;
var
  ErrCode: Cardinal;
  ErrMsg: string;
begin
  ErrCode := ERR_get_error;
  while ErrCode <> 0 do
  begin
    ErrMsg := ERR_error_string(ErrCode, nil);
    WriteLn('SSL Error: ', ErrMsg);
    ErrCode := ERR_get_error;
  end;
end;

// 使用示例
if not Conn.Connect then
begin
  WriteLn('Connect failed');
  CheckSSLError;  // 打印详细错误
end;
```

---

## 快速检查清单

在部署前检查以下项目：

- [ ] 启用了证书验证 (`WithVerifyPeer`)
- [ ] 启用了 hostname 验证 (`WithVerifyHostname`)
- [ ] 设置了 SNI (`SetServerName`)
- [ ] 使用 TLS 1.2 或更高版本
- [ ] 正确处理了 OpenSSL 对象释放
- [ ] 测试了目标平台的证书路径
- [ ] 检查了 OpenSSL 版本兼容性

---

## 相关文档

- `docs/guides/QUICKSTART.md` - 快速入门
- `docs/guides/FAQ.md` - 常见问题
- `docs/reference/OPENSSL_1_1_1_VS_3X_DIFF_AND_REGRESSION.md` - 版本差异
- `docs/reference/STORE_CROSS_PLATFORM_DIFFERENCES.md` - 跨平台差异
