# Optional Backends Certificate Verification Truth

## Goal

把 `WolfSSL` / `MbedTLS` 证书对象对外暴露的
`Verify` / `VerifyEx`
从当前的弱语义 / 假成功路径，
收紧成更接近真实证书验证语义的 public truth，
避免调用方继续遇到：

- `WolfSSL Verify`
  只靠 subject/issuer 文本匹配就把错误 CA 判成成功
- `WolfSSL VerifyEx`
  基本不填
  `TSSLCertVerifyResult`
- `MbedTLS VerifyEx`
  把 revocation / CRL / OCSP flags 直接忽略
  仍返回成功

## Scope

- 修改：
  - `src/fafafa.ssl.wolfssl.certificate.pas`
  - `src/fafafa.ssl.mbedtls.certificate.pas`
  - `tests/test_wolfssl_framework.pas`
  - `tests/test_mbedtls_framework.pas`
  - `tests/certs/ca-subject-imposter.pem`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

- 不做：
  - 不扩到 OpenSSL / WinSSL
  - 不重跑 broad compile-all / governance 脚本
  - 不在这一批里重构整个 `certchain` 公共层

## Architecture Notes

- `WolfSSL`
  当前没有现成
  `X509_STORE_CTX` / `X509_verify_cert`
  绑定，
  所以不能再假装“已经做了 native verify”。
- 这一批优先保证：
  - 不再出现 name-only false positive
  - `VerifyEx`
    的失败路径有真实错误信息
  - 请求 revocation / CRL / OCSP 时 fail-closed
- `MbedTLS`
  继续复用
  `mbedtls_x509_crt_verify`
  做基础链验证，
  再把 higher-level flags/result
  收紧成不再静默忽略。

## TDD Steps

1. 在 `tests/test_wolfssl_framework.pas` 制造 RED：
   - 用 `signer_cert.pem` + 真 `ca_cert.pem`
     验证成功
   - 用同 subject 但错误密钥的
     `tests/certs/ca-subject-imposter.pem`
     验证必须失败
   - `VerifyEx(nil, ...)`
     必须填错误信息
   - `VerifyEx(..., [sslCertVerifyCheckRevocation])`
     必须 fail-closed
     并填 `RevocationStatus = 2`
2. 在 `tests/test_mbedtls_framework.pas` 制造 RED：
   - `VerifyEx(nil, ...)`
     必须填错误信息
   - `VerifyEx(..., [sslCertVerifyCheckRevocation])`
     必须 fail-closed
     并填 `RevocationStatus = 2`
3. 最小修复：
   - `WolfSSL`
     去掉 subject/issuer 文本命中即成功的逻辑
   - `WolfSSL VerifyEx`
     补齐 result 结构和 fail-closed 语义
   - `MbedTLS VerifyEx`
     补齐 success/failure result 字段
     并在 unsupported revocation flags 上 fail-closed
4. Focused verification：
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas`
   - `./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
   - `./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
   - `git diff --check`

## Expected Result

- `WolfSSL`
  不再把同 subject 的错误 CA 错判为验证成功
- `WolfSSL` / `MbedTLS`
  在 `VerifyEx` 失败时
  不再返回空壳结果
- 请求 revocation / CRL / OCSP 时，
  optional backends
  不再静默成功，
  而是明确 fail-closed

## Execution Result

- PASS
- 新增
  `tests/certs/ca-subject-imposter.pem`
  把
  `WolfSSL`
  issuer-name-only
  false positive
  固化成可重复 RED
- `MbedTLS`
  这批顺手确认
  `TMbedTLSCertificateStore.AddCertificate`
  之前没有同步 native CA chain，
  已在实现里补齐
- 多 backend 的
  `VerifyEx`
  result 初始化
  之前还保留
  `FillChar`
  这类不安全模式，
  本批已同步切成显式字段重置
- focused verification：
  - `tests/test_wolfssl_framework.pas`
    - `217 passed / 0 failed`
  - `tests/test_mbedtls_framework.pas`
    - `201 passed / 0 failed`
  - `git diff --check`
    - PASS
