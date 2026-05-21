# Store 使用指南（OpenSSL / WinSSL）

本文给出 `ISSLCertificateStore` 的最小可用用法，覆盖跨平台通用接入与平台特定策略。

---

## 1. 何时使用 Store

当你需要以下能力时：

- 加载系统根证书（用于 TLS 对端证书验证）
- 加载自定义 CA 文件或目录
- 在 Windows 上访问系统证书存储（`ROOT` / `MY` / `CA`）
- 构建并检查证书链

---

## 2. 跨平台通用接入（推荐）

优先使用 `TSSLFactory.CreateCertificateStore` + `LoadSystemStore`，把证书存储注入到 `ISSLContext`。

```pascal
uses
  SysUtils,
  fafafa.ssl;

var
  LContext: ISSLContext;
  LStore: ISSLCertificateStore;
begin
  LContext := TSSLFactory.CreateContext(sslCtxClient, sslAutoDetect);
  LStore := TSSLFactory.CreateCertificateStore(sslAutoDetect);

  if (LContext = nil) or (LStore = nil) then
    raise Exception.Create('初始化 SSL 上下文或证书存储失败');

  if not LStore.LoadSystemStore then
    raise Exception.Create('加载系统证书失败');

  LContext.SetCertificateStore(LStore);
  LContext.SetVerifyMode([sslVerifyPeer]);
end;
```

---

## 3. OpenSSL：加载自定义 CA（文件/目录）

```pascal
uses
  SysUtils,
  fafafa.ssl;

var
  LStore: ISSLCertificateStore;
begin
  LStore := TSSLFactory.CreateCertificateStore(sslOpenSSL);
  if LStore = nil then
    raise Exception.Create('CreateCertificateStore(sslOpenSSL) 失败');

  if not LStore.LoadFromFile('/etc/ssl/cert.pem') then
    raise Exception.Create('加载 CA 文件失败');

  if not LStore.LoadFromPath('/etc/ssl/certs') then
    raise Exception.Create('加载 CA 目录失败');
end;
```

说明：
- OpenSSL 后端支持 `LoadSystemStore`，内部会先调用 `X509_STORE_set_default_paths`，再尝试固定路径列表。
- 详细差异见：`docs/reference/STORE_CROSS_PLATFORM_DIFFERENCES.md`。

---

## 4. Windows：打开指定系统存储（WinSSL）

在 Windows 场景，可用 WinSSL 辅助函数直接打开系统存储：

```pascal
{$IFDEF WINDOWS}
uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.winssl.certstore;

var
  LStore: ISSLCertificateStore;
  LCert: ISSLCertificate;
begin
  LStore := OpenSystemStore(SSL_STORE_MY);
  if LStore = nil then
    raise Exception.Create('打开 MY 证书存储失败');

  LCert := LStore.FindBySubject('CN=MyServer');
  if LCert = nil then
    WriteLn('未找到目标证书');
end;
{$ENDIF}
```

常见系统存储名：
- `SSL_STORE_ROOT`
- `SSL_STORE_MY`
- `SSL_STORE_CA`
- `SSL_STORE_TRUST`
- `SSL_STORE_DISALLOWED`

---

## 5. 证书验证与链构建

```pascal
uses
  SysUtils,
  fafafa.ssl;

var
  LStore: ISSLCertificateStore;
  LCert: ISSLCertificate;
  LChain: TSSLCertificateArray;
begin
  LStore := TSSLFactory.CreateCertificateStore(sslAutoDetect);
  LStore.LoadSystemStore;

  LCert := TSSLFactory.CreateCertificate(sslAutoDetect);
  if not LCert.LoadFromFile('server_cert.pem') then
    raise Exception.Create('加载证书失败');

  if not LStore.VerifyCertificate(LCert) then
    raise Exception.Create('证书验证失败');

  LChain := LStore.BuildCertificateChain(LCert);
  WriteLn('链长度: ', Length(LChain));
end;
```

---

## 6. 失败排查建议

- `LoadSystemStore` 失败：先检查后端是否正确加载（OpenSSL/WinSSL）。
- `LoadFromFile` 失败：检查证书编码（PEM/DER）与路径权限。
- `VerifyCertificate` 失败：检查证书有效期、链完整性与根证书来源。
- Windows 场景失败：优先检查存储名、用户权限与系统策略（GPO）。

---

## 7. 回归验证命令

```bash
bash scripts/run_all_module_tests.sh --modules Store --verbose
```

补充：
- Store 模块报告：`docs/test_reports/P2_STORE_MODULE_REPORT.md`
- 跨平台差异：`docs/reference/STORE_CROSS_PLATFORM_DIFFERENCES.md`
