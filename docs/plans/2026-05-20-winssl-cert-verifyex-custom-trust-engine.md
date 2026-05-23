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

- PASS
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
- 最新 Windows runtime truth
  已进一步收窄出一条更具体的实现残余：
  - `expired-signer.pem + ca_cert.pem`
    现在已经能先正确暴露
    expiry diagnostic
  - 但紧接着第二次
    `VerifyEx(..., [sslCertVerifyIgnoreExpiry], ...)`
    触发
    `EAccessViolation`
  - 结合当前实现，
    最可能的根因是：
    `CERT_CHAIN_ENGINE_CONFIG.rghAdditionalStore`
    被指向了 helper 栈上的临时数组
    ，而 chain engine 在后续调用里仍会读取该地址
- follow-up 修法：
  - chain engine
    只保留
    `hExclusiveRoot`
    trust-anchor 语义
  - 同一个 custom store
    改为在每次
    `CertGetCertificateChain(...)`
    调用时
    显式传入
    `hAdditionalStore`
  - 这样既保留建链来源，
    又消掉配置期临时指针的生命周期洞
- 再下一轮 Windows runtime truth
  又把问题继续收窄了一步：
  - 去掉
    `rghAdditionalStore`
    生命周期洞后，
    `EAccessViolation`
    仍然原地出现
  - 而且位置完全没变：
    - baseline
      `VerifyEx(..., [], ...)`
      正常返回
      `Certificate has expired`
    - 一进入
      `VerifyEx(..., [sslCertVerifyIgnoreExpiry], ...)`
      就崩
  - 这说明：
    - 不是 custom trust engine
      方向本身的问题
    - 更像是
      WinSSL cert-level
      `CERT_CHAIN_POLICY_BASE`
      + nonzero `dwFlags`
      这条 native policy-flag path
      本身不稳定
- 当前 follow-up 修法：
  - 不再把
    `sslCertVerifyIgnoreExpiry`
    /
    `sslCertVerifyAllowSelfSigned`
    直接映射到
    `CERT_CHAIN_POLICY_PARA.dwFlags`
  - 改成：
    - 先跑 zero-flag native baseline
    - 再按 public contract
      对
      `CERT_E_EXPIRED`
      /
      self-signed + `CERT_E_UNTRUSTEDROOT`
      做窄范围 success override
  - 这样：
    - `StrictChain`
      仍保持已有的 fail-closed EKU gate
    - per-call exception flags
      继续兑现，
      但不再依赖会崩的 native policy-flag lane
- 最终远端 closure：
  - commit
    `f0be85a`
    (`test(winssl): hold verify stores by interface`)
    把 focused WinSSL test
    里用于重复调用的 memory-backed store
    改成由
    `ISSLCertificateStore`
    接口直接持有
  - `WinSSL Runtime Gate`
    run
    `26159931322`
    随后完整通过：
    - quick smoke
    - Windows Wave B gate
    - broader WinSSL runtime suite
  - 这把最后一层残余性质定死了：
    - custom trust engine
      方向本身没有再出错
    - zero-flag native baseline
      + public-contract override
      方向也已站稳
    - 最后的
      `EAccessViolation`
      是 focused test
      对
      `TInterfacedObject`
      store 的生命周期持有方式错误，
      不是当前 cert-level trust-engine
      实现仍有未闭环 native 崩溃
