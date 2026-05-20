# Optional Backends Certificate Version Truth

## Goal

把
`MbedTLS` / `WolfSSL`
证书对象里
`GetVersion`
这条已发布
`ISSLCertificate`
surface
从当前
“默认当成 v3”
的弱语义，
收口到和真实 X.509
版本一致的 public truth，
避免调用方在 optional backends 上继续遇到：

- 非 v3 证书
  仍被报告成
  `Version = 3`
- `GetInfo.Version`
  与真实证书版本不一致

## Scope

- 修改：
  - `src/fafafa.ssl.mbedtls.certificate.pas`
  - `tests/test_mbedtls_framework.pas`
  - `tests/test_wolfssl_framework.pas`
  - `tests/certs/version1-cert.pem`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不重开 broader certificate metadata redesign
- 不修改 `OpenSSL` / `WinSSL` / `FreePascal` 实现
- 不为这条批次补新的文档专题或 capability 文字

## Architecture Truth

- `OpenSSL` 当前 `GetVersion`
  已走
  `X509_get_version(...) + 1`
- `WinSSL` 当前 `GetVersion`
  已走
  `CERT_INFO.dwVersion + 1`
- `FreePascal` 当前 `GetVersion`
  已来自 parser truth
- `WolfSSL` 当前 `GetVersion`
  已走
  `wolfSSL_X509_get_version(...) + 1`
- 只有 `TMbedTLSCertificate.GetVersion`
  仍固定返回
  `3`
- 之前这条 lane
  没做掉的唯一原因
  不是实现无从修复，
  而是当时仓库里
  没有现成非 v3 fixture

## Fixture Strategy

- 新增一个真实
  X.509 v1
  自签名证书夹具：
  - `tests/certs/version1-cert.pem`
- 生成方式：
  - 用最小 OpenSSL config
    去掉自动扩展
  - 用
    `openssl req -new -x509 -x509v1`
    生成真实 v1 证书
- 这比 mock / field patch
  更接近 public runtime truth

## TDD Steps

1. 新增 v1 fixture
2. 在
   `tests/test_mbedtls_framework.pas`
   /
   `tests/test_wolfssl_framework.pas`
   增加 version contract：
   - `Load version1 fixture`
   - `GetVersion = 1`
   - `GetInfo.Version = 1`
3. 先观察 RED：
   - 预期
     `WolfSSL`
     通过
   - 预期
     `MbedTLS`
     因固定 `3`
     失败
4. 最小修复
   `TMbedTLSCertificate.GetVersion`
   到 parser-backed truth
5. Focused verification：
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
   - `./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas`
   - `./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
   - `git diff --check`

## Expected Result

- `MbedTLS`
  不再把
  v1 证书
  错报成 v3
- `WolfSSL`
  同一真实 v1 fixture
  继续保持正确版本真相
- optional backends
  的 certificate version surface
  有真实非 v3 证据覆盖，
  不再只是“默认 3 也看不出来”的弱证明
