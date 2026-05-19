# Optional Backends Certificate Algorithm Metadata Completeness

## Goal

把 `MbedTLS` / `WolfSSL` 证书对象对外发布的算法元数据
从固定默认值壳
收紧成
基于真实 X.509 内容的 public truth，
避免调用方在加载 `ECDSA` 等非 `RSA` 证书时，
仍被错误地告知：

- `GetPublicKeyAlgorithm = RSA`
- `GetSignatureAlgorithm = SHA256withRSA`

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

- 不重开 broader certificate metadata redesign
- 不扩新的 native API binding
- 不顺手清扫其他文档专题

## Architecture Truth

- `TFreePascalCertificate`
  已经通过 `TX509Certificate` 暴露真实算法元数据：
  - `PublicKeyInfo.Algorithm.Name`
  - `SignatureAlgorithm.Name`
  - name 为空时 fallback 到 OID
- `TMbedTLSCertificate` 当前仍固定返回：
  - `RSA`
  - `SHA256withRSA`
- `TWolfSSLCertificate` 当前也仍固定返回：
  - `RSA`
  - `SHA256withRSA`
- `tests/test_mbedtls_framework.pas`
  与
  `tests/test_wolfssl_framework.pas`
  现在还把这些默认值当成测试真相
- 仓库已有可复用非 `RSA` 夹具：
  - `tests/certificate/test_certs/signer_ecdsa_cert.pem`
  - 其真实元数据已由 `openssl x509 -text` 确认：
    - `Public Key Algorithm: id-ecPublicKey`
    - `Signature Algorithm: ecdsa-with-SHA256`

## Steps

1. 在两组 framework tests 中制造 RED：
   - 加载 `signer_ecdsa_cert.pem`
   - 断言 `GetPublicKeyAlgorithm` / `GetSignatureAlgorithm` 暴露真实算法
   - 同步断言 `GetInfo` 字段跟 getter 一致
2. 在 `MbedTLS` / `WolfSSL` 证书实现中复用 `TX509Certificate`：
   - 优先取 `Name`
   - 为空时 fallback 到 OID
3. focused verification：
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
   - `./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas`
   - `./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
   - `git diff --check`

## Expected Result

- `MbedTLS` / `WolfSSL` 对已加载的 `ECDSA` 证书不再发布 RSA 默认壳
- `GetInfo.PublicKeyAlgorithm` / `GetInfo.SignatureAlgorithm`
  与 getter truth 一致
- focused tests 通过并形成可复用台账

## Execution Result

- PASS
- `tests/test_mbedtls_framework.pas`
  首轮 RED 直接打出 4 个失败，
  证明 `MbedTLS` 证书算法元数据仍是固定默认壳；
  修复后最终 `119 passed / 0 failed`
- `tests/test_wolfssl_framework.pas`
  首轮 RED 也打出 4 个失败，
  证明 `WolfSSL` 同样存在同类 drift；
  修复后最终 `144 passed / 0 failed`
- `TMbedTLSCertificate` / `TWolfSSLCertificate`
  现在都复用 `TX509Certificate`
  暴露：
  - `ecPublicKey`
  - `ecdsa-with-SHA256`
- `GetInfo.PublicKeyAlgorithm` /
  `GetInfo.SignatureAlgorithm`
  已与 getter truth 对齐
