# fafafa.ssl API 清单

这是当前 public surface 的高层索引，不再维护历史 phase snapshot、测试统计或性能数字。

如果你要判断某个接口的完整签名、owner interface 或 backend capability truth，请优先看：

- `docs/reference/API_REFERENCE.md`
- `docs/BACKEND_CAPABILITY_MATRIX.md`
- backend-specific capability/reference 页面

## 核心 runtime family

### ISSLContext

当前 `ISSLContext` 已有以下实现：

- `TOpenSSLContext` (`src/fafafa.ssl.openssl.context.pas`)
- `TWinSSLContext` (`src/fafafa.ssl.winssl.context.pas`)
- `TFreePascalContext` (`src/fafafa.ssl.freepascal.context.pas`)
- `TMbedTLSContext` (`src/fafafa.ssl.mbedtls.context.pas`)
- `TWolfSSLContext` (`src/fafafa.ssl.wolfssl.context.pas`)

这些 context 会再按 backend capability 暴露 optional interface，例如：

- `ISSLEarlyDataContext`
- `ISSLServerOCSPStaplingContext`

不要再把 `OpenSSL / WinSSL` 误当成当前唯一 shipped context family。

### ISSLConnection

当前 `ISSLConnection` 已有以下实现：

- `TOpenSSLConnection` (`src/fafafa.ssl.openssl.connection.pas`)
- `TWinSSLConnection` (`src/fafafa.ssl.winssl.connection.pas`)
- `TFreePascalConnection` (`src/fafafa.ssl.freepascal.connection.pas`)
- `TMbedTLSConnection` (`src/fafafa.ssl.mbedtls.connection.pas`)
- `TWolfSSLConnection` (`src/fafafa.ssl.wolfssl.connection.pas`)

补充边界：

- 当前 `GetOCSPStaplingEnabled` / `GetOCSPResponse` / `IsOCSPResponseVerified` / `GetOCSPResponseStatus` 兼容入口已经 shipped，owner truth 在 `ISSLOCSPStapling` optional interface
- `ISSLConnection` 上的 session mirror 也只是兼容入口；新代码优先走 `ISSLSessionResumption`
- 不存在独立 shipped 的 `ISSLServerConnection` public interface；服务端特化 surface 继续通过 context/optional interface 暴露

### 证书、证书存储、会话

当前证书与会话 family 也已经不是双后端布局：

- `ISSLCertificate`
  - `TOpenSSLCertificate`
  - `TWinSSLCertificate`
  - `TFreePascalCertificate`
  - `TMbedTLSCertificate`
  - `TWolfSSLCertificate`
- `ISSLCertificateStore`
  - `TOpenSSLCertificateStore`
  - `TWinSSLCertificateStore`
  - `TFreePascalCertificateStore`
  - `TMbedTLSCertificateStore`
  - `TWolfSSLCertificateStore`
- `ISSLSession`
  - `TOpenSSLSession`
  - `TWinSSLSession`
  - `TFreePascalSession`
  - `TMbedTLSSession`
  - `TWolfSSLSession`

## 可选能力 surface

这些能力已经有明确 owner interface，不要再把它们写成“缺失方法待实现”：

- OCSP stapling: `ISSLOCSPStapling`
- 0-RTT / early data: `ISSLEarlyDataContext` / `ISSLEarlyDataConnection`
- 会话复用: `ISSLSessionResumption`
- Certificate Transparency: `ISSLCertificateTransparency` / `ISSLCertificateTransparencyValidation`

具体哪个 backend 当前发布这些能力，继续以 capability matrix 为准，而不是以本页做静态拍板。

## Builder 与 PKCS#11

`TSSLContextBuilder` 当前仍是高入口配置面，但 `PKCS#11` 的 public truth 已经不是“未来计划”：

- 当前 published PKCS#11 context path 只在 `OpenSSL` backend 暴露。
- 当前 capability truth 跟随 `TPKCS11BackendFactory.IsBackendAvailable(btAuto)`。
- 也就是说，仓库里确实有 shipped `LoadPrivateKeyFromPKCS11(...)` / `UsePKCS11(...)` 路径，但如果当前 OpenSSL runtime 缺少 Provider / ENGINE 必需 surface，`SupportsPKCS11` 会降为 `False`。
- `WinSSL` / `FreePascal` / `MbedTLS` / `WolfSSL` 当前 `SupportsPKCS11=False`。
- builder 侧当前支持 `pmNone` / `pmValue` / `pmEnvironment` / `pmFile`；`pmCallback` / `pmInteractive` 仍属于 lower-level `TPKCS11Config` / backend integration。

## 这页故意不再承载什么

以下内容改由更贴近 truth source 的地方维护，不再在本页重复复制：

- 历史 phase “完成度快照”
- 测试数量 / 通过率统计
- 性能基准数字
- “下一步计划” 待办列表

这样可以减少高入口参考页再次回漂成过期阶段报告。
