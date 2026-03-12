# STORE 跨平台差异说明（OpenSSL / WinSSL）

## 目标
本文基于当前代码实现，说明 `fafafa.ssl` 在 Linux / macOS / Windows 下证书存储（Store）能力与行为差异，并给出可执行验证建议。

## 后端与平台映射

| 平台 | 常见后端 | Store 实现入口 | 说明 |
|------|----------|----------------|------|
| Linux | OpenSSL | `src/fafafa.ssl.openssl.certstore.pas` | 基于 `X509_STORE`，并支持 `OSSL_STORE_*` URI 加载能力 |
| macOS | OpenSSL | `src/fafafa.ssl.openssl.certstore.pas` | 当前与 Linux 使用同一 `LoadSystemStore` 路径策略（无 macOS 专属分支） |
| Windows | WinSSL (Schannel) | `src/fafafa.ssl.winssl.certstore.pas` | 基于系统证书存储（`HCERTSTORE`）与 WinAPI 链验证 |

## OpenSSL 后端（Linux/macOS）

### 系统证书加载行为
`TOpenSSLCertificateStore.LoadSystemStore` 采用两段式策略：

1. 调用 `X509_STORE_set_default_paths(FStore)`（若符号可用）
2. 依次尝试固定路径（目录走 `LoadFromPath`，文件走 `LoadFromFile`）
   - `/etc/ssl/certs`
   - `/etc/pki/tls/certs`
   - `/usr/share/ca-certificates`
   - `/usr/local/share/ca-certificates`
   - `/etc/ssl/cert.pem`

> 代码位置：`src/fafafa.ssl.openssl.certstore.pas:550`

### 结果判定注意点
`LoadSystemStore` 的返回值由 `LoadedAny or (GetCount > 0)` 决定；
`GetCount` 基于当前对象缓存证书数（`FCertificates.Count`）。

> 代码位置：`src/fafafa.ssl.openssl.certstore.pas:381`、`src/fafafa.ssl.openssl.certstore.pas:596`

### URI 加载行为（OSSL_STORE）
OpenSSL Store API 路径由 `fafafa.ssl.openssl.api.store` 处理，典型调用链：
- `OSSL_STORE_open`
- `OSSL_STORE_expect`
- `OSSL_STORE_load`
- `OSSL_STORE_INFO_get1_CERT` / `OSSL_STORE_INFO_get1_PKEY`

该能力支持 `file:` URI 等对象加载（证书/私钥）。

> 代码位置：`src/fafafa.ssl.openssl.api.store.pas:445`

## WinSSL 后端（Windows）

### 系统存储模型
`TWinSSLCertificateStore` 支持常见系统存储名：
- `ROOT`
- `MY`
- `CA`
- `Trust`
- `Disallowed`

`LoadSystemStore` 默认加载 `ROOT`。

> 代码位置：`src/fafafa.ssl.winssl.certstore.pas:92`、`src/fafafa.ssl.winssl.certstore.pas:443`、`src/fafafa.ssl.winssl.certstore.pas:681`

### 打开与验证行为
- 打开系统存储使用 `CertOpenSystemStoreW`
- 证书链验证使用 `CertGetCertificateChain`

与 OpenSSL 的 `X509_STORE`/`X509_verify_cert` 路径不同，WinSSL 更依赖系统策略与本机证书存储状态。

> 代码位置：`src/fafafa.ssl.winssl.certstore.pas:226`、`src/fafafa.ssl.winssl.certstore.pas:621`

## 已验证失败路径（P2）

- Store invalid payload：`tests/fixtures/p2/store/store_invalid_cert_payload_v1.txt`
- Store missing file：`tests/crypto/test_p2_store_comprehensive.pas:307`
- 最新聚焦回归（含 Store）：`docs/archive/reports/test-report-history/test_report_20260207_022236.txt`

## 差异总结

1. **信任源来源不同**
   - OpenSSL：文件系统路径 + URI
   - WinSSL：系统证书存储（ROOT/MY/CA 等）

2. **常见故障模式不同**
   - OpenSSL：路径不存在、文件格式错误、URI 对象类型不匹配
   - WinSSL：存储名不正确、系统证书权限/策略限制、链策略差异

3. **测试策略建议**
   - OpenSSL 侧保留文件/URI 失败场景
   - Windows 侧补充系统存储访问失败与链构建策略场景

## 后续建议

- macOS：补充 Homebrew OpenSSL 路径与系统 Keychain 协同策略说明。
- Windows：补充存储访问权限与企业策略（GPO）场景回归。
