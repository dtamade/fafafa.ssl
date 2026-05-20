# Optional Backends Certificate Verify Flags Expiry/Self-Signed Parity

## Goal

把 `MbedTLS` / `WolfSSL` 证书对象的
`VerifyEx`
在两个已发布 flags 上的 live 语义收紧成一致的 public truth：

- `sslCertVerifyIgnoreExpiry`
- `sslCertVerifyAllowSelfSigned`

避免调用方继续遇到：

- `WolfSSL`
  已经能按 flag 放行，
  但 `MbedTLS`
  仍把相同请求直接 fail
- `MbedTLS VerifyEx`
  对外宣称接受这些 flags，
  实际上只做 round-trip，
  没有真正改变验证结果

## Scope

- 修改：
  - `src/fafafa.ssl.mbedtls.certificate.pas`
  - `tests/test_mbedtls_framework.pas`
  - `tests/test_wolfssl_framework.pas`
  - `tests/certs/expired-signer.pem`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

- 不做：
  - 不重构整个 verify pipeline
  - 不扩到 `OpenSSL` / `WinSSL` / `FreePascal`
  - 不在这一批里重开 OCSP/CRL 语义线

## Why This Batch

当前 live truth 已经明确：

- `WolfSSL VerifyEx`
  已经手工实现了
  time-check
  与
  allow-self-signed
  分支
- `MbedTLS VerifyEx`
  仍完全依赖
  `mbedtls_x509_crt_verify`
  的直接结果
- 本机
  `/usr/include/mbedtls/x509.h`
  已确认 native verify flags
  至少能区分：
  - `MBEDTLS_X509_BADCERT_EXPIRED`
  - `MBEDTLS_X509_BADCERT_FUTURE`
  - `MBEDTLS_X509_BADCERT_NOT_TRUSTED`

所以这批最小正确修法不是大改，
而是：

- 用真实过期 leaf fixture
  钉住
  `IgnoreExpiry`
- 用现有 self-signed fixture
  钉住
  `AllowSelfSigned`
- 在
  `MbedTLS VerifyEx`
  里只对对应 native failure bits
  做有边界的放行

## TDD Steps

1. 新增真实过期夹具：
   - `tests/certs/expired-signer.pem`
   - 由现有
     `ca_cert.pem`
     签发，
     `notAfter`
     固定落在过去
2. 在 `tests/test_wolfssl_framework.pas` 加 control proof：
   - 过期 leaf
     无 flag
     失败
   - 过期 leaf
     `sslCertVerifyIgnoreExpiry`
     成功
   - self-signed leaf
     无 flag
     失败
   - self-signed leaf
     `sslCertVerifyAllowSelfSigned`
     成功
3. 在 `tests/test_mbedtls_framework.pas` 加同样 contract：
   - 预期先 RED
4. 最小修复：
   - `MbedTLS VerifyEx`
     在 native verify 失败后：
     - 若只剩 expiry/future bits
       且请求了
       `IgnoreExpiry`
       则放行
     - 若只剩 not-trusted
       且 leaf 确实 self-signed，
       并请求了
       `AllowSelfSigned`
       则放行
5. Focused verification：
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas`
   - `./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
   - `./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
   - `git diff --check`

## Expected Result

- `WolfSSL`
  对这两条 flag
  继续保持正确 control truth
- `MbedTLS`
  不再只是“能设置 flag”，
  而是
  `VerifyEx`
  真正按 flag 改变结果
- optional backends
  在 expiry / self-signed
  这两个常见验证例外路径上
  语义更一致

## Execution Result

- PASS
- `WolfSSL`
  继续作为稳定 control group：
  - `expired` leaf
    无 flag
    失败
  - `IgnoreExpiry`
    成功
  - self-signed leaf
    无 flag
    失败
  - `AllowSelfSigned`
    成功
- `MbedTLS VerifyEx`
  现在不再只是接受这两个 flags，
  而是：
  - 对
    `MBEDTLS_X509_BADCERT_EXPIRED`
    /
    `MBEDTLS_X509_BADCERT_FUTURE`
    真正按
    `sslCertVerifyIgnoreExpiry`
    放行
  - 对 self-signed leaf 的
    `MBEDTLS_X509_BADCERT_NOT_TRUSTED`
    真正按
    `sslCertVerifyAllowSelfSigned`
    放行
- focused verification：
  - `tests/test_mbedtls_framework.pas`
    - `211 passed / 0 failed`
  - `tests/test_wolfssl_framework.pas`
    - `227 passed / 0 failed`
  - `git diff --check`
    - PASS
