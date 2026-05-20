# WinSSL Certificate VerifyEx Flag Parity

## Goal

把 `WinSSL`
证书对象的
`ISSLCertificate.VerifyEx`
在三条已发布 flags 上的 live 语义收紧成与其它 backend 一致的 public truth：

- `sslCertVerifyIgnoreExpiry`
- `sslCertVerifyAllowSelfSigned`
- `sslCertVerifyStrictChain`

避免调用方继续遇到：

- expired leaf
  明明显式请求了
  `IgnoreExpiry`
  仍然直接失败
- self-signed leaf
  明明显式请求了
  `AllowSelfSigned`
  仍然受
  unknown CA
  拒绝
- `StrictChain`
  在
  `WinSSL certificate.VerifyEx`
  上只是 API round-trip，
  实际没有改变验证结果

## Scope

- 修改：
  - `src/fafafa.ssl.winssl.certificate.pas`
  - `tests/winssl/test_winssl_cert_verify_ex.pas`
  - `tests/winssl/test_winssl_cert_verify_ex.lpi`
  - `tests/run_winssl_tests.ps1`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

- 不做：
  - 不扩到
    `WinSSL connection`
    hostname / SSL policy
    路径
  - 不重构整个
    `CertGetCertificateChain`
    pipeline
  - 不在这一批里重开
    `OCSP`
    /
    `CRL`
    更深的联网语义

## Why This Batch

当前静态 truth
已经明确：

- `WinSSL connection`
  路径
  已经通过
  `CERT_CHAIN_POLICY_SSL`
  兑现了：
  - `IgnoreExpiry`
  - `AllowSelfSigned`
  - `IgnoreHostname`
- 但
  `WinSSL certificate.VerifyEx`
  目前仍然：
  - 用
    `CERT_CHAIN_POLICY_BASE`
  - 没有把
    `IgnoreExpiry`
    映射到
    `CERT_CHAIN_POLICY_IGNORE_NOT_TIME_VALID_FLAG`
  - 没有把
    `AllowSelfSigned`
    映射到
    `CERT_CHAIN_POLICY_ALLOW_UNKNOWN_CA_FLAG`
  - 没有
    `StrictChain`
    fail-closed
    分支

同时，
当前仓库里已经有适合做 focused RED
的确定性夹具：

- `tests/certs/expired-signer.pem`
- `tests/certs/version1-cert.pem`
- `tests/certificate/test_certs/signer_cert.pem`
- `tests/certificate/test_certs/ca_cert.pem`

以及现成的 WinSSL memory-store helper
可以避免
`ROOT`
系统存储
把 self-signed 行为“掩盖成功”。

## TDD Steps

1. 把
   `tests/winssl/test_winssl_cert_verify_ex.pas`
   从常量烟雾测试升级成真实运行时契约：
   - 运行时生成
     expired self-signed leaf
     + empty memory store
     下：
     - `VerifyEx(..., [])`
       必须失败
     - `VerifyEx(..., [sslCertVerifyAllowSelfSigned])`
       仍必须因 expiry 失败
     - `VerifyEx(..., [sslCertVerifyAllowSelfSigned, sslCertVerifyIgnoreExpiry])`
       必须成功
   - `version1-cert.pem`
     +
     empty memory store
     下：
     - `VerifyEx(..., [])`
       必须失败
     - `VerifyEx(..., [sslCertVerifyAllowSelfSigned])`
       必须成功
   - `signer_cert.pem`
     +
     `ca_cert.pem`
     下：
     - `VerifyEx(..., [])`
       必须成功
     - `VerifyEx(..., [sslCertVerifyStrictChain])`
       必须失败
       且错误信息要提到
       `strict`
       /
       `serverAuth`
       /
       `extended key usage`
2. 把
   `test_winssl_cert_verify_ex.lpi`
   对齐现有运行中的 WinSSL `.lpi` 工程配置，
   去掉错误的
   `TargetOS=linux`
   固定目标，
   避免接入 runtime suite 后在 Windows runner 上先炸工程配置
3. 把
   `test_winssl_cert_verify_ex.lpi`
   接入
   `tests/run_winssl_tests.ps1`
4. 最小修复
   `src/fafafa.ssl.winssl.certificate.pas`：
   - `IgnoreExpiry`
     ->
     `CERT_CHAIN_POLICY_IGNORE_NOT_TIME_VALID_FLAG`
   - `AllowSelfSigned`
     ->
     `CERT_CHAIN_POLICY_ALLOW_UNKNOWN_CA_FLAG`
   - `StrictChain`
     ->
     leaf
     缺失
     `serverAuth`
     EKU
     时明确 fail-closed
5. Focused verification：
   - `git diff --check`
   - `git push`
   - GitHub Actions:
     - `WinSSL Runtime Gate`
     - `CI`

## Expected Result

- `WinSSL certificate.VerifyEx`
  不再静默忽略
  `IgnoreExpiry`
  /
  `AllowSelfSigned`
  /
  `StrictChain`
- 证书级
  `VerifyEx`
  与连接级已兑现的
  public flag truth
  不再割裂
- 新 runtime contract
  真正进入
  Windows suite，
  后续不必再靠静态阅读重复拉起这条问题线

## Execution Result

- IMPLEMENTED
- 当前已经完成：
  - `test_winssl_cert_verify_ex.pas`
    从常量烟雾测试升级为真实 runtime contract
  - `test_winssl_cert_verify_ex.lpi`
    去掉了错误固定的
    `TargetOS=linux`
  - `tests/run_winssl_tests.ps1`
    已接入这个 focused WinSSL test
  - `src/fafafa.ssl.winssl.certificate.pas`
    已补齐：
    - `sslCertVerifyIgnoreExpiry`
    - self-signed leaf
      `sslCertVerifyAllowSelfSigned`
    - `sslCertVerifyStrictChain`
      fail-closed
- 当前 Windows CI 首轮反馈又补充了一个重要真相：
  - `CERT_CHAIN_POLICY_BASE`
    下，
    `memory-backed additional store`
    可以参与建链，
    但不会自动把那张 CA 当成 trusted root
  - 因而
    `expired-signer.pem + ca_cert.pem`
    在 WinSSL cert-level `VerifyEx`
    上会先暴露
    `untrusted root`
    而不是 expiry
  - 所以 expiry contract
    已改成：
    - 先用
      self-signed leaf
      +
      `AllowSelfSigned`
      去掉 trust 干扰
    - 再验证
      `IgnoreExpiry`
      的 live 语义
- 当前本地 proof：
  - `git diff --check`
    - PASS
  - `xmllint --noout tests/winssl/test_winssl_cert_verify_ex.lpi`
    - PASS
- 当前本地限制：
  - Linux 环境没有
    `pwsh`
    / Windows runtime，
    所以真正的编译与运行证明
    交给 push 后的
    `WinSSL Runtime Gate`
