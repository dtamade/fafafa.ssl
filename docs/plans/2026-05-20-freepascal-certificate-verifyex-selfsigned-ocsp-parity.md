# FreePascal Certificate VerifyEx Self-Signed / OCSP Parity

## Goal

把 `FreePascal`
证书对象的
`ISSLCertificate.VerifyEx`
在两条已发布 flags 上的 live 语义收紧成与其它 backend 一致的 public truth：

- `sslCertVerifyAllowSelfSigned`
- `sslCertVerifyCheckOCSP`

避免调用方继续遇到：

- self-signed leaf
  明明显式请求了
  `AllowSelfSigned`
  仍然直接失败
- `CheckOCSP`
  在
  `FreePascal certificate.VerifyEx`
  上只是 API round-trip，
  实际没有改变结果，
  也没有 fail-closed

## Scope

- 修改：
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `tests/freepascal/test_freepascal_verify_ex_flag_parity_contract.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

- 不做：
  - 不扩到
    `FreePascal connection`
    runtime lane
  - 不在这一批里处理
    `WinSSL`
    residuals
  - 不重构整个
    `certchain`
    公共层

## Why This Batch

当前静态 truth
已经明确：

- `FreePascal certificate.VerifyEx`
  只显式处理了：
  - `IgnoreExpiry`
  - `StrictChain`
  - `CheckRevocation`
  - `CheckCRL`
- 现有实现里：
  - 没有
    `AllowSelfSigned`
    分支
  - 没有
    `CheckOCSP`
    fail-closed
    分支

同时，
现有其它 backend
已经给出了清晰 control truth：

- `OpenSSL`
  /
  `MbedTLS`
  /
  `WolfSSL`
  都不会再把
  `AllowSelfSigned`
  当成空壳
- `OpenSSL`
  /
  `MbedTLS`
  /
  `WolfSSL`
  都已经对
  `CheckOCSP`
  收紧成
  fail-closed
  语义

## TDD Steps

1. 新增 focused FreePascal contract：
   - `version1-cert.pem`
     + empty store
     下：
     - `VerifyEx(..., [])`
       必须失败
     - `VerifyEx(..., [sslCertVerifyAllowSelfSigned])`
       必须成功
   - `signer_cert.pem`
     + `ca_cert.pem`
     下：
     - `VerifyEx(..., [])`
       必须成功
     - `VerifyEx(..., [sslCertVerifyCheckOCSP])`
       必须失败
       且错误信息要提到
       `ocsp`
       或
       `revocation`
2. 运行 focused test，
   预期先 RED
3. 最小修复：
   - 仅在 leaf
     确认 self-signed
     且请求了
     `AllowSelfSigned`
     时，
     对当前调用放行
   - 当请求
     `CheckOCSP`
     时，
     比照其它 backend
     fail-closed
4. Focused verification：
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_verify_ex_flag_parity_contract_units -FEtmp/test_freepascal_verify_ex_flag_parity_contract_units -otmp/test_freepascal_verify_ex_flag_parity_contract_units/test_freepascal_verify_ex_flag_parity_contract tests/freepascal/test_freepascal_verify_ex_flag_parity_contract.pas`
   - `./tmp/test_freepascal_verify_ex_flag_parity_contract_units/test_freepascal_verify_ex_flag_parity_contract`
   - `git diff --check`

## Expected Result

- `FreePascal certificate.VerifyEx`
  不再静默忽略
  `AllowSelfSigned`
  /
  `CheckOCSP`
- self-signed leaf
  显式请求
  `AllowSelfSigned`
  时
  真正成功
- `CheckOCSP`
  在缺少对应能力时
  明确 fail-closed

## Execution Result

- PASS
- focused RED
  先证明了两个真实问题：
  - `FreePascal VerifyEx`
    对
    `sslCertVerifyAllowSelfSigned`
    确实是空壳，
    self-signed leaf
    + empty store
    在显式请求该 flag 后仍然失败
  - `sslCertVerifyCheckOCSP`
    在 cert-level
    `VerifyEx`
    上没有
    fail-closed
    分支
- 最小修复：
  - 在不放宽 expiry / chain / strict-chain
    其它错误的前提下，
    仅对
    self-signed leaf
    + `AllowSelfSigned`
    做当前调用级放行
  - 对
    `CheckOCSP`
    比照其它 backend
    收紧成明确 fail-closed
- focused verification：
  - `tests/freepascal/test_freepascal_verify_ex_flag_parity_contract.pas`
    - PASS
    - 同时覆盖：
      - `AllowSelfSigned`
        真正生效
      - `CheckOCSP`
        明确 fail-closed
  - `git diff --check`
    - PASS
- 当前批收口后的默认下一步：
  - 继续处理
    `WinSSL certificate.VerifyEx`
    当前静态 residual：
    - `IgnoreExpiry`
    - `AllowSelfSigned`
    - `StrictChain`
