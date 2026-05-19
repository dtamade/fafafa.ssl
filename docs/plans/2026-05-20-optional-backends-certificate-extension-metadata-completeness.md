# Optional Backends Certificate Extension Metadata Completeness

## Goal

把 `MbedTLS` / `WolfSSL` 证书对象的扩展类元数据
从“部分 getter 有值、`GetInfo` 快照仍残缺”
收紧成
与真实 X.509 扩展一致的 public truth，
避免调用方在读取证书扩展相关 surface 时继续拿到：

- `IsCA=False` 默认壳
- 空的 `SubjectAltNames`
- 空的 `KeyUsage` / `ExtendedKeyUsage`
- `GetInfo` 漏填 `PublicKeySize` / `IsCA` / `SubjectAltNames` / `KeyUsage`

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
- 不顺手清扫其他后端或文档专题

## Architecture Truth

- `TFreePascalCertificate`
  已经通过 `TX509Certificate`
  填好：
  - `PublicKeySize`
  - `IsCA`
  - `PathLength` / `PathLenConstraint`
  - `KeyUsage` bitfield
  - `SubjectAltNames`
  - `GetKeyUsage`
  - `GetExtendedKeyUsage`
- `TMbedTLSCertificate`
  当前仍存在：
  - `IsCA = False` 默认壳
  - `GetInfo` 没有填
    `PublicKeySize` /
    `IsCA` /
    `SubjectAltNames` /
    `KeyUsage`
- `TWolfSSLCertificate`
  当前仍存在：
  - `GetKeyUsage = []`
  - `GetExtendedKeyUsage = []`
  - `GetInfo` 同样漏填上面结构化字段
- 这类残缺已经落在
  `ISSLCertificate`
  与
  `TSSLCertificateInfo`
  的已发布 surface 上，
  会直接影响
  `certchain`
  和上层基于扩展字段的判断

## Fixtures

- `tests/certificate/test_certs/signer_ecdsa_cert.pem`
  - `Public Key Algorithm: id-ecPublicKey`
  - `Public-Key: (256 bit)`
  - `Basic Constraints: CA:TRUE`
- `tests/certs/san-test.pem`
  - `Subject Alternative Name`
    - `DNS:san-test.local`
    - `DNS:example.test`
    - `IP Address:127.0.0.1`
- `tests/certificate/test_certs/keyusage_cert.pem`
  - `Key Usage`
    - `Digital Signature`
    - `Key Encipherment`
  - `Extended Key Usage`
    - `TLS Web Server Authentication`
    - `TLS Web Client Authentication`

## Steps

1. 在两组 framework tests 中制造 RED：
   - `ECDSA` fixture:
     - `GetInfo.PublicKeySize`
     - `IsCA`
     - `GetInfo.IsCA`
   - `SAN` fixture:
     - `GetInfo.SubjectAltNames`
     - `GetSubjectAltNames`
   - `KeyUsage` fixture:
     - `GetKeyUsage`
     - `GetExtendedKeyUsage`
     - `GetInfo.KeyUsage`
2. 在 `MbedTLS` / `WolfSSL` 证书实现中继续复用 `TX509Certificate`：
   - 填完整 `GetInfo` 结构化字段
   - 收掉 `IsCA` / `KeyUsage` / `ExtendedKeyUsage` / `SAN` 的空壳
3. focused verification：
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
   - `./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas`
   - `./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
   - 可选 cross-check：
     - `tests/contract/test_backend_contract.pas`
   - `git diff --check`

## Expected Result

- `MbedTLS` / `WolfSSL`
  都能对已加载证书发布真实扩展元数据
- `GetInfo`
  不再是只填半边字段的残缺快照
- focused tests 通过并形成可复用台账

## Execution Result

- PASS
- `MbedTLS`
  新增扩展元数据 contract
  首轮 RED 打出 14 个失败，
  真实根因不是 parser 本身，
  而是 repeated load 后
  `FDERData` / `FPEMData`
  stale cache
  仍指向上一张证书；
  修复后最终 `139 passed / 0 failed`
- `WolfSSL`
  同类 contract
  首轮即 GREEN，
  证明 parser-backed extension metadata 路径可直接补齐：
  - `IsCA`
  - `SAN`
  - `KeyUsage`
  - `ExtendedKeyUsage`
  - `GetInfo` 结构化字段
- cross-check：
  - `tests/contract/test_backend_contract.pas`
    继续 `Failed: 0`
- 这批最终收口：
  - `TMbedTLSCertificate` /
    `TWolfSSLCertificate`
    都会通过 `TX509Certificate`
    发布真实扩展元数据
  - `GetInfo`
    已补齐：
    - `PublicKeySize`
    - `IsCA`
    - `PathLength`
    - `PathLenConstraint`
    - `KeyUsage`
    - `SubjectAltNames`
