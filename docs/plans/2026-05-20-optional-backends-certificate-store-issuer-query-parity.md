# Optional Backends Certificate Store Issuer Query Parity

## Goal

把 `MbedTLS` / `WolfSSL`
证书存储对象的
`FindByIssuer`
从原始字符串子串比较
收紧成与当前 store query family
一致的可用语义，
避免调用方在 optional backends 上继续遇到：

- 同一 issuer
  只因大小写 / 分隔符空格格式不同
  就查不到
- 用已有 distinct-issuer 夹具
  也无法稳定走通
  `FindByIssuer`

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

- 不在这一批里统一
  `FreePascal` / `OpenSSL` / `WinSSL`
  的全局 issuer-search contract
- 不新开 store index/cache family
- 不改 subject / serial 查询

## Architecture Truth

- `MbedTLS` / `WolfSSL`
  当前 `FindByIssuer`
  都还是：
  - `Pos(AIssuer, LCert.GetIssuer) > 0`
- `FindBySubject`
  刚刚已经收口到
  normalized query truth
- 所以当前更自然的 bounded 方向
  不是立刻重定义
  所有 backend 的 issuer-search 全局契约，
  而是先把 optional backends
  从“原始比较”
  提升到与自己同一家族 query surface
  一致的可用程度
- 夹具选择：
  - `tests/certificate/test_certs/signer_cert.pem`
    - subject:
      `C=CN, ST=Beijing, L=Beijing, O=Test Org, CN=Test Signer`
    - issuer:
      `C=CN, ST=Beijing, L=Beijing, O=Test CA, CN=Test CA`

## Steps

1. 在 `MbedTLS` / `WolfSSL` framework tests 中制造 RED：
   - `FindByIssuer`
     支持 normalized issuer query
   - 可选补一条空 issuer query
     fail-closed 断言
2. 在两组 store 实现中做最小修复：
   - 复用当前文件内
     已有的 normalized text helper
3. focused verification：
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
   - `./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas`
   - `./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
   - `git diff --check`

## Expected Result

- `MbedTLS` / `WolfSSL`
  能对 distinct-issuer fixture
  发布可用的 normalized issuer lookup truth
- optional backend store query family
  在 subject / serial / issuer
  三条高频查找面上
  更接近一致

## Execution Result

- PASS
- `MbedTLS`
  新增 issuer-query contract
  首轮 RED
  打出 1 个失败：
  - `FindByIssuer supports normalized query variant`
  修复后最终 `161 passed / 0 failed`
- `WolfSSL`
  同类 contract
  首轮 RED
  也打出 1 个失败：
  - `FindByIssuer supports normalized query variant`
  修复后最终 `180 passed / 0 failed`
- 本批最终收口：
  - `TMbedTLSCertificateStore`
    现在会对 issuer query
    做归一化匹配
  - `TWolfSSLCertificateStore`
    现在也会对 issuer query
    做归一化匹配
- 这批明确不扩：
  - 全仓库 `FindByIssuer` canonical contract 重定义
  - store index/cache family
  只收口 optional backends
  当前最直接的 issuer-query drift
- `git diff --check`
  通过
