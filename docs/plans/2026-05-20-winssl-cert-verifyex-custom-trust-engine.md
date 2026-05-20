# WinSSL Cert VerifyEx Custom Trust Engine

## Goal

把 `WinSSL` 证书级
`Verify`
/
`VerifyEx`
对自定义 `ACAStore`
的真实语义补齐成 public truth：

- 调用方传入的 custom store
  不再只是
  `hAdditionalStore`
- store 里的 CA
  能真正作为 trust anchor
- `expired-signer.pem + ca_cert.pem`
  这组 memory-store fixture
  能直接证明
  `sslCertVerifyIgnoreExpiry`
  的 per-call 行为

## Scope

- 修改：
  - `src/fafafa.ssl.winssl.base.pas`
  - `src/fafafa.ssl.winssl.api.pas`
  - `src/fafafa.ssl.winssl.certificate.pas`
  - `tests/winssl/test_winssl_cert_verify_ex.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

- 不做：
  - 不扩大到 connection handshake / hostname lane
  - 不重开 generated self-signed `EAccessViolation` 深挖
  - 不在 Linux 本地伪造 Windows runtime 证明

## Why This Batch

上一批真实跑出来的两个 runtime truth
已经足够说明：

- `CERT_CHAIN_POLICY_BASE`
  下，
  只把 `ACAStore`
  当 `hAdditionalStore`
  不能把 CA 变成 trusted root
- 所以
  `CurrentUser\ROOT`
  workaround
  虽然能让测试暂时过掉，
  但会把 focused contract
  变成依赖系统状态的证明

所以这批最小正确修法不是继续改 fixture，
而是让 WinSSL 证书级验证自己兑现：

- custom store
  进入
  `CERT_CHAIN_ENGINE_CONFIG`
- 用
  `hExclusiveRoot`
  提供 trust anchor
- 同时把同一个 store
  放进
  `cAdditionalStore`
  供建链使用

## Expected Result

- `ISSLCertificate.Verify`
  与
  `VerifyEx`
  都能接受 memory-backed CA store
  作为真实信任来源
- `tests/winssl/test_winssl_cert_verify_ex.pas`
  不再写入
  `CurrentUser\ROOT`
- Windows CI
  后续若再失败，
  会更接近真正剩余的 WinSSL runtime 问题，
  而不是系统根存储变通造成的假象

## Execution Result

- IMPLEMENTED (local static proof)
- 已完成：
  - 新增
    `CERT_CHAIN_ENGINE_CONFIG`
    /
    `CertCreateCertificateChainEngine`
    /
    `CertFreeCertificateChainEngine`
    绑定
  - `TWinSSLCertificate.Verify`
    /
    `VerifyEx`
    现在会为 custom store
    创建专用 chain engine
  - focused WinSSL expiry contract
    已改回纯 memory-store fixture
  - focused test
    还补了一条
    `Verify(LStore)`
    契约，
    防止 `Verify`
    与 `VerifyEx`
    继续分叉
- 当前本地 proof：
  - `git diff --check`
    - PASS
- 当前本地限制：
  - Linux 无法本地编译/运行 WinSSL
  - 最终真值仍需看 push 后的
    `WinSSL Runtime Gate`
