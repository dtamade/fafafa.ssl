# Optional Backends Certificate Public Surface Completeness

## Goal

把 `MbedTLS` / `WolfSSL` 证书对象里
已经发布给 `ISSLCertificate` 的
两个剩余空壳 surface：

- `GetPublicKey`
- `GetExtension`

从固定空串
收紧成与当前仓库既有 backend truth 一致的可用实现，
避免调用方在 optional backends 上仍然遇到：

- 证书已加载但 `GetPublicKey = ''`
- 已知扩展存在但 `GetExtension(OID) = ''`

## Scope

- 修改：
  - `src/fafafa.ssl.mbedtls.certificate.pas`
  - `src/fafafa.ssl.wolfssl.certificate.pas`
  - `tests/test_mbedtls_framework.pas`
  - `tests/test_wolfssl_framework.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不追求完整 PEM/DER 公钥导出
- 不扩新的 native X509 extension binding
- 不重开 broader certificate redesign

## Architecture Truth

- 已有 backend 现状：
  - `OpenSSL.GetPublicKey`
    当前走最小可用语义：
    - 返回 `GetPublicKeyAlgorithm`
  - `FreePascal.GetPublicKey`
    也已按同一 contract 收口：
    - 返回 `GetPublicKeyAlgorithm`
  - `FreePascal.GetExtension`
    已经能通过 `TX509Certificate.Extensions`
    找到 OID 并返回：
    - 有 `Value` 时返回十六进制
    - 否则返回扩展名
- optional backends 当前残缺：
  - `TMbedTLSCertificate.GetPublicKey = ''`
  - `TMbedTLSCertificate.GetExtension = ''`
  - `TWolfSSLCertificate.GetPublicKey = ''`
  - `TWolfSSLCertificate.GetExtension = ''`
- 这不是“增强功能”，而是已发布 certificate public surface 的实现缺口

## Fixture Truth

- `tests/certificate/test_certs/signer_ecdsa_cert.pem`
  - 已用于算法 metadata contract
  - 含 `Subject Key Identifier`
- contract 选择：
  - `GetPublicKey <> ''`
  - `GetPublicKey = GetPublicKeyAlgorithm`
  - `GetExtension('2.5.29.14') <> ''`

## Steps

1. 在 `MbedTLS` / `WolfSSL` framework tests 中先制造 RED：
   - 已加载证书后：
     - `GetPublicKey <> ''`
     - `GetPublicKey = GetPublicKeyAlgorithm`
     - `GetExtension('2.5.29.14') <> ''`
2. 在两组 certificate 实现中复用现有 parser truth：
   - `GetPublicKey` 对齐 `OpenSSL` / `FreePascal` 的最小 contract
   - `GetExtension` 复用 `TX509Certificate.Extensions`
3. focused verification：
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
   - `./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas`
   - `./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
   - `git diff --check`

## Expected Result

- `MbedTLS` / `WolfSSL`
  的 `GetPublicKey`
  不再返回空串
- `MbedTLS` / `WolfSSL`
  能对已知存在的 `Subject Key Identifier`
  发布非空 extension truth
- focused tests 通过并形成可复用台账

## Execution Result

- PASS
- `MbedTLS`
  新增 contract
  首轮 RED 打出 3 个失败，
  精确落在：
  - `GetPublicKey <> ''`
  - `GetPublicKey = GetPublicKeyAlgorithm`
  - `GetExtension('2.5.29.14') <> ''`
  修复后最终 `142 passed / 0 failed`
- `WolfSSL`
  同类 contract
  首轮 RED 也打出 3 个失败，
  修复后最终 `167 passed / 0 failed`
- 本批确定的语义收口：
  - `GetPublicKey`
    当前仓库 contract
    继续与 `OpenSSL` / `FreePascal` 保持一致：
    - 返回算法标识字符串
  - `GetExtension`
    通过 `TX509Certificate.Extensions`
    发布 parser truth：
    - 有原始值则返回十六进制
    - 否则返回扩展名
- `git diff --check`
  通过
