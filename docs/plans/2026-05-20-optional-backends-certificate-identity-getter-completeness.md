# Optional Backends Certificate Identity Getter Completeness

## Goal

把 `MbedTLS` / `WolfSSL` 证书对象里
identity 语义最核心的 3 个 getter：

- `GetSubject`
- `GetIssuer`
- `GetSerialNumber`

从 placeholder / fragile text parsing / 明显错误值
收紧成与真实 X.509 内容一致的 public truth，
避免调用方在 optional backends 上继续拿到：

- `Subject` / `Issuer` 占位符
- 错误格式或错误语义的序列号
- 与 parser truth 分裂的 identity surface

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

- 不新开 store normalization family
- 不在这一批里补 `GetVersion`
- 不新扩 native X509 getter binding

## Architecture Truth

- `TX509Certificate`
  已经能稳定提供：
  - `Subject.ToString`
  - `Issuer.ToString`
  - `Subject.CommonName`
  - `SerialNumberAsHex`
- 当前可疑点：
  - `TWolfSSLCertificate.GetSerialNumber`
    仍把 serial 指针地址当作返回值
  - `TMbedTLSCertificate.GetSubject` /
    `GetIssuer` /
    `GetSerialNumber`
    仍依赖 `mbedtls_x509_crt_info(...)`
    文本切片
  - `TWolfSSLCertificate.GetSubject` /
    `GetIssuer`
    仍走 native oneline + placeholder fallback
- 当前 `GetVersion`
  虽然也值得继续审，
  但现有 repo 夹具全是 `Version: 3`，
  在没有新夹具前
  很难制造有意义的 RED；
  这批只把它记录成下一条证据缺口

## Fixture Truth

- `tests/certificate/test_certs/signer_ecdsa_cert.pem`
  - Subject / Issuer:
    - `CN=Test Signer ECDSA`
  - Serial:
    - `3C:E7:A2:77:AA:E4:DB:33:E1:23:ED:85:33:28:E5:D5:E2:1B:38:F4`

## Steps

1. 在 `MbedTLS` / `WolfSSL` framework tests 中制造 RED：
   - `GetSubject` 含 `CN=Test Signer ECDSA`
   - `GetIssuer` 含 `CN=Test Signer ECDSA`
   - `GetSubjectCN = Test Signer ECDSA`
   - `GetSerialNumber` 归一化后
     等于 fixture 真值
2. 在两组 certificate 实现中复用 parser truth：
   - `GetSubject -> LParser.Subject.ToString`
   - `GetIssuer -> LParser.Issuer.ToString`
   - `GetSerialNumber -> LParser.SerialNumberAsHex`
3. focused verification：
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
   - `./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas`
   - `./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
   - `git diff --check`

## Expected Result

- `MbedTLS` / `WolfSSL`
  的 subject / issuer / serial identity getter
  不再依赖 placeholder 或错误语义
- `signer_ecdsa_cert.pem`
  的 identity truth
  在两组 optional backends 上一致可见
- focused tests 通过并形成可复用台账

## Execution Result

- PASS
- `MbedTLS`
  新增 identity contract
  首轮即 GREEN，
  最终 `147 passed / 0 failed`
- `WolfSSL`
  同类 contract
  首轮暴露的不只是格式 drift，
  而是
  `GetSerialNumber`
  会触发 `EAccessViolation`
- 在把 serial 断言
  固化成 fail-closed contract 后，
  稳定 RED
  收敛成 1 个 serial failure；
  修复后最终 `172 passed / 0 failed`
- 本批最终收口：
  - `TMbedTLSCertificate`
    / `TWolfSSLCertificate`
    都会优先通过
    `TX509Certificate`
    发布：
    - `Subject.ToString`
    - `Issuer.ToString`
    - `SerialNumberAsHex`
  - native text / oneline 路径
    只保留为 parser 不可用时的 fallback
- 暂不扩
  `GetVersion`
  的原因已固化：
  - 当前仓库夹具全是 `Version: 3`
  - 在没有非 v3 fixture 前，
    很难打出有意义的 RED
- `git diff --check`
  通过
