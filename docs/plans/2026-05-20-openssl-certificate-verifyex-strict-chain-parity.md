# OpenSSL Certificate VerifyEx Strict-Chain Parity

## Goal

把 `OpenSSL` 证书对象的
`ISSLCertificate.VerifyEx`
在已发布 flag
`sslCertVerifyStrictChain`
上的 live 语义
收紧成与当前仓库其它 backend
一致的 public truth，
避免调用方继续遇到：

- `sslCertVerifyStrictChain`
  在
  `OpenSSL certificate.VerifyEx`
  上只是 API round-trip，
  实际没有改变验证结果
- leaf 证书缺失
  `serverAuth`
  EKU
  时，
  `VerifyEx`
  仍然返回成功

## Scope

- 修改：
  - `src/fafafa.ssl.openssl.certificate.pas`
  - `tests/openssl/test_openssl_verify_ex_strict_chain_contract.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

- 不做：
  - 不重开
    `sslCertVerifyIgnoreExpiry`
    /
    `sslCertVerifyAllowSelfSigned`
    的 store-flag scope 问题
  - 不扩到
    `WinSSL`
    /
    `FreePascal`
  - 不在这一批里重构 OpenSSL verify pipeline

## Why This Batch

当前源码 truth 已经明确：

- `WolfSSL`
  /
  `MbedTLS`
  /
  `FreePascal`
  的
  `certificate.VerifyEx`
  都有
  `sslCertVerifyStrictChain`
  分支
- `OpenSSL certificate.VerifyEx`
  目前只有：
  - `IgnoreExpiry`
  - `AllowSelfSigned`
  - `CheckRevocation`
  - `CheckCRL`
  - `CheckOCSP`
- 当前实现里
  没有
  `sslCertVerifyStrictChain`
  相关逻辑

同时，
现成 fixture
`tests/certificate/test_certs/signer_cert.pem`
已确认：

- 证书链本身有效
- 但 leaf 没有
  `serverAuth`
  EKU

这让它成为一个很干净的 strict-chain RED fixture。

## TDD Steps

1. 新增 focused OpenSSL contract：
   - `VerifyEx(..., [])`
     对
     `signer_cert.pem`
     +
     `ca_cert.pem`
     继续成功
   - `VerifyEx(..., [sslCertVerifyStrictChain])`
     必须失败
   - 失败消息必须提到：
     - `strict`
     - 或 `serverAuth`
     - 或 `extended key usage`
2. 运行 focused test，
   先得到 RED
3. 最小修复：
   - 在
     `TOpenSSLCertificate.VerifyEx`
     成功链验证后，
     增加与其它 backend 一致的
     `serverAuth`
     EKU 检查
4. Focused verification：
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_verify_ex_strict_chain_contract_units -FEtmp/test_openssl_verify_ex_strict_chain_contract_units -otmp/test_openssl_verify_ex_strict_chain_contract_units/test_openssl_verify_ex_strict_chain_contract tests/openssl/test_openssl_verify_ex_strict_chain_contract.pas`
   - `./tmp/test_openssl_verify_ex_strict_chain_contract_units/test_openssl_verify_ex_strict_chain_contract`
   - `git diff --check`

## Expected Result

- `OpenSSL certificate.VerifyEx`
  不再静默忽略
  `sslCertVerifyStrictChain`
- leaf 缺失
  `serverAuth`
  EKU
  时，
  `VerifyEx`
  会给出明确失败
- `OpenSSL`
  在 strict-chain 这条 certificate-surface 上
  与其它 backend 更一致

## Execution Result

- PASS
- `tests/certificate/test_certs/signer_cert.pem`
  已被证明是一个干净的
  strict-chain RED fixture：
  - 默认验证成功
  - 但 leaf 没有显式
    `extendedKeyUsage`
    /
    `serverAuth`
- `OpenSSL certificate.VerifyEx`
  现在不再静默忽略
  `sslCertVerifyStrictChain`，
  而是：
  - 先保留
    `X509_verify_cert`
    的基础链验证
  - 再要求 leaf
    显式带有
    `serverAuth`
    EKU
  - 不满足时
    fail-closed
    并返回明确错误
- focused verification：
  - `tests/openssl/test_openssl_verify_ex_strict_chain_contract.pas`
    - PASS
  - `git diff --check`
    - PASS
