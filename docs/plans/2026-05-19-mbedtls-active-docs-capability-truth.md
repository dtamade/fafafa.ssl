# 2026-05-19 MbedTLS Active Docs Capability Truth

## Goal

继续沿着 interface/backend completeness 主线推进，收口 `MbedTLS` 高入口文档与当前 public API / capability 真相的明显漂移：

- `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
  - 仍把 `0-RTT` 写成“⚠️ 部分”
  - 仍把证书固定写成“通过回调”
  - 仍把自定义 I/O 写成对外发布的 callback surface
- `docs/guides/MBEDTLS_USER_GUIDE.md`
  - 仍保留大量过时 API 名称 / 旧签名
  - 仍把接口说成“完全相同”
  - 仍混入与当前 backend truth 不一致的 callback / FIPS / hostname / CA loading 说法

## Scope

- 只处理 MbedTLS 高入口 active docs truth：
  - `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
  - `docs/guides/MBEDTLS_USER_GUIDE.md`
- 用 focused shell contract 锁住：
  - capability wording
  - current public API names
  - key example snippets
- 不补做新的 MbedTLS runtime 能力
- 不扩到 OpenSSL / WinSSL / FreePascal 文档批量重写

## Files

- `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
- `docs/guides/MBEDTLS_USER_GUIDE.md`
- `tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `MbedTLS` 当前 `SupportsCallbacks=False`
  - verify / password / info callback non-nil assignment 会 fail-closed `unsupported`
- `MbedTLS` 当前 `SupportsPKCS12=False`
  - 没有 shipped PKCS#12 bundle create / parse / import surface
- `MbedTLS` 当前不发布 `ISSLEarlyDataContext / ISSLEarlyDataConnection`
  - 因而 0-RTT current public capability = none
- `MbedTLS` 证书固定当前走 context pinning API：
  - `AddCertificatePin`
  - `AddCertificatePinBase64`
  - `SetCertificatePinningEnabled`
  - 而不是 callback surface
- `MbedTLS` 连接 transport 当前只发布：
  - socket / stream `CreateConnection(...)`
  - 不发布 caller-supplied custom I/O callback seam

## Steps

1. 补 focused shell contract，让 MbedTLS active docs drift 先 RED。
2. 把 MbedTLS capability matrix 与 user guide 改回当前 public API / capability 真相。
3. 同步台账，避免后续重复把旧 MbedTLS 示例当成 current source truth。
4. 跑轻量验证并提交。

## Commands

```bash
bash -n tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh
bash tests/scripts/test_mbedtls_active_docs_capability_truth_contract.sh
git diff --check
```

## Expected Result

- `MbedTLS` active docs 不再把 unpublished callback / 0-RTT / custom I/O surface 讲得比源码更宽
- `MbedTLS` user guide 不再继续教授过时方法名和旧连接签名
- 调用方重新获得与当前 `ISSLLibrary.GetCapabilities` / `ISSLContext` / `ISSLConnection` 一致的高入口心智
