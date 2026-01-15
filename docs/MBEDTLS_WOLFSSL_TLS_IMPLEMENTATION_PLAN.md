# MbedTLS/WolfSSL TLS 主链路实现计划

**创建日期**: 2026-01-12
**目标**: 将 MbedTLS/WolfSSL 后端从 Preview 升级到 Stable

---

## 当前状态

| 后端 | 状态 | 框架 | 单元测试 | TLS 主链路 |
|------|------|------|----------|-----------|
| MbedTLS | Preview | ✅ 完成 | ✅ 274 测试 | ❌ 待实现 |
| WolfSSL | Preview | ✅ 完成 | ✅ 274 测试 | ❌ 待实现 |

### 已完成模块

- `fafafa.ssl.mbedtls.base` - 基础类型定义
- `fafafa.ssl.mbedtls.api` - API 绑定
- `fafafa.ssl.mbedtls.lib` - 库管理
- `fafafa.ssl.mbedtls.context` - 上下文管理
- `fafafa.ssl.mbedtls.connection` - 连接接口（框架）
- `fafafa.ssl.mbedtls.certificate` - 证书管理
- `fafafa.ssl.mbedtls.session` - 会话管理

- `fafafa.ssl.wolfssl.base` - 基础类型定义
- `fafafa.ssl.wolfssl.api` - API 绑定
- `fafafa.ssl.wolfssl.lib` - 库管理
- `fafafa.ssl.wolfssl.context` - 上下文管理（含连接）
- `fafafa.ssl.wolfssl.certificate` - 证书管理
- `fafafa.ssl.wolfssl.session` - 会话管理

---

## TLS 主链路实现任务

### Phase 1: MbedTLS TLS 握手实现

#### 1.1 客户端握手 (Client Handshake)

**文件**: `src/fafafa.ssl.mbedtls.connection.pas`

```pascal
function TMbedTLSConnection.Connect: Boolean;
begin
  // 1. 设置 BIO 回调（socket I/O）
  mbedtls_ssl_set_bio(FSSLContext, @FSocket,
    @mbedtls_net_send, @mbedtls_net_recv, nil);

  // 2. 设置 SNI（如果有）
  if FServerName <> '' then
    mbedtls_ssl_set_hostname(FSSLContext, PAnsiChar(FServerName));

  // 3. 执行握手
  repeat
    Result := mbedtls_ssl_handshake(FSSLContext);
  until (Result <> MBEDTLS_ERR_SSL_WANT_READ) and
        (Result <> MBEDTLS_ERR_SSL_WANT_WRITE);

  FHandshakeComplete := (Result = 0);
  Result := FHandshakeComplete;
end;
```

**关键点**:
- [ ] 实现 `mbedtls_net_send` / `mbedtls_net_recv` 回调
- [ ] 处理非阻塞 I/O（WANT_READ/WANT_WRITE）
- [ ] SNI 支持
- [ ] 证书验证回调

#### 1.2 服务端握手 (Server Handshake)

```pascal
function TMbedTLSConnection.Accept: Boolean;
begin
  // 服务端使用相同的握手流程
  // 但需要先加载服务器证书和私钥
  Result := DoHandshake = sslHandshakeComplete;
end;
```

#### 1.3 数据传输

```pascal
function TMbedTLSConnection.Read(var ABuffer; ACount: Integer): Integer;
begin
  Result := mbedtls_ssl_read(FSSLContext, @ABuffer, ACount);
  if Result < 0 then
    FLastError := Result;
end;

function TMbedTLSConnection.Write(const ABuffer; ACount: Integer): Integer;
begin
  Result := mbedtls_ssl_write(FSSLContext, @ABuffer, ACount);
  if Result < 0 then
    FLastError := Result;
end;
```

### Phase 2: WolfSSL TLS 握手实现

#### 2.1 客户端握手

**文件**: `src/fafafa.ssl.wolfssl.context.pas` (TWolfSSLConnection)

```pascal
function TWolfSSLConnection.Connect: Boolean;
var
  Ret: Integer;
begin
  // 1. 设置 socket
  wolfSSL_set_fd(FSSL, FSocket);

  // 2. 设置 SNI
  if FServerName <> '' then
    wolfSSL_UseSNI(FSSL, WOLFSSL_SNI_HOST_NAME,
      PAnsiChar(FServerName), Length(FServerName));

  // 3. 执行握手
  Ret := wolfSSL_connect(FSSL);
  FHandshakeComplete := (Ret = WOLFSSL_SUCCESS);
  Result := FHandshakeComplete;
end;
```

#### 2.2 服务端握手

```pascal
function TWolfSSLConnection.Accept: Boolean;
var
  Ret: Integer;
begin
  wolfSSL_set_fd(FSSL, FSocket);
  Ret := wolfSSL_accept(FSSL);
  FHandshakeComplete := (Ret = WOLFSSL_SUCCESS);
  Result := FHandshakeComplete;
end;
```

### Phase 3: 证书验证

#### 3.1 MbedTLS 证书验证

```pascal
procedure TMbedTLSConnection.SetupCertVerification;
begin
  // 设置验证回调
  mbedtls_ssl_conf_verify(FSSLConfig, @CertVerifyCallback, Self);

  // 设置 CA 证书
  mbedtls_ssl_conf_ca_chain(FSSLConfig, FCACerts, nil);
end;
```

#### 3.2 WolfSSL 证书验证

```pascal
procedure TWolfSSLConnection.SetupCertVerification;
begin
  // 设置验证模式
  wolfSSL_CTX_set_verify(FCtx, WOLFSSL_VERIFY_PEER, @VerifyCallback);

  // 加载 CA 证书
  wolfSSL_CTX_load_verify_locations(FCtx, PAnsiChar(CAFile), nil);
end;
```

### Phase 4: 会话复用

#### 4.1 MbedTLS 会话复用

```pascal
function TMbedTLSConnection.GetSession: ISSLSession;
begin
  // 导出会话数据
  mbedtls_ssl_get_session(FSSLContext, @SessionData);
  Result := TMbedTLSSession.Create(SessionData);
end;

procedure TMbedTLSConnection.SetSession(ASession: ISSLSession);
begin
  // 恢复会话
  mbedtls_ssl_set_session(FSSLContext,
    (ASession as TMbedTLSSession).GetNativeHandle);
end;
```

#### 4.2 WolfSSL 会话复用

```pascal
function TWolfSSLConnection.GetSession: ISSLSession;
begin
  Result := TWolfSSLSession.Create(wolfSSL_get_session(FSSL));
end;

procedure TWolfSSLConnection.SetSession(ASession: ISSLSession);
begin
  wolfSSL_set_session(FSSL,
    (ASession as TWolfSSLSession).GetNativeHandle);
end;
```

---

## 测试计划

### 集成测试

| 测试 | 描述 | 优先级 |
|------|------|--------|
| TLS 1.2 握手 | 连接到真实服务器 | P0 |
| TLS 1.3 握手 | 连接到支持 TLS 1.3 的服务器 | P0 |
| 证书验证 | 验证服务器证书 | P0 |
| SNI 支持 | 多域名服务器测试 | P1 |
| 会话复用 | 验证会话恢复 | P1 |
| 双向 TLS | 客户端证书认证 | P2 |

### 契约测试

确保 MbedTLS/WolfSSL 后端与 OpenSSL 后端行为一致：

```pascal
// tests/contract/test_backend_tls_contract.pas
procedure TestTLSHandshake_AllBackends;
begin
  for Backend in [sslOpenSSL, sslMbedTLS, sslWolfSSL] do
  begin
    Lib := TSSLFactory.GetLibrary(Backend);
    Ctx := Lib.CreateContext(sslCtxClient);
    Conn := Ctx.CreateConnection(Socket);

    // 所有后端应该能成功握手
    Assert(Conn.Connect, Backend + ' handshake failed');

    // 协议版本应该一致
    Assert(Conn.GetProtocolVersion in [sslProtocolTLS12, sslProtocolTLS13]);
  end;
end;
```

---

## 依赖项

### MbedTLS

```bash
# Ubuntu/Debian
sudo apt-get install libmbedtls-dev

# 验证
pkg-config --modversion mbedtls  # 应显示 3.x
```

### WolfSSL

```bash
# 从源码编译（推荐）
git clone https://github.com/wolfSSL/wolfssl.git
cd wolfssl
./autogen.sh
./configure --enable-tls13 --enable-sni --enable-alpn
make && sudo make install

# 验证
pkg-config --modversion wolfssl  # 应显示 5.x
```

---

## 实施顺序

1. **MbedTLS 客户端握手** - 最基础功能
2. **MbedTLS 数据传输** - Read/Write
3. **MbedTLS 证书验证** - 安全性
4. **WolfSSL 客户端握手** - 第二后端
5. **WolfSSL 数据传输** - Read/Write
6. **WolfSSL 证书验证** - 安全性
7. **会话复用** - 性能优化
8. **服务端支持** - 完整功能

---

## 预期结果

| 指标 | 当前 | 目标 |
|------|------|------|
| MbedTLS 状态 | Preview | Stable |
| WolfSSL 状态 | Preview | Stable |
| TLS 1.2 支持 | ❌ | ✅ |
| TLS 1.3 支持 | ❌ | ✅ |
| 证书验证 | ❌ | ✅ |
| 会话复用 | ❌ | ✅ |

---

*最后更新: 2026-01-12*
