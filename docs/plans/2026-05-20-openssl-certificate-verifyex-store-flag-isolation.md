# OpenSSL Certificate VerifyEx Store Flag Isolation

## Goal

把 `OpenSSL`
证书对象的
`ISSLCertificate.VerifyEx`
里两条按次调用 exception flags：

- `sslCertVerifyIgnoreExpiry`
- `sslCertVerifyAllowSelfSigned`

从当前可能污染 shared
`X509_STORE`
的实现，
收紧成真正的
per-call verification truth，
避免调用方继续遇到：

- 第一次带
  `IgnoreExpiry`
  的验证成功后，
  同一个 store
  上后续不带 flag 的验证
  也被“顺手放行”
- `AllowSelfSigned`
  /
  `IgnoreExpiry`
  本该只影响本次验证，
  却把可复用 store
  变成带状态副作用的对象

## Scope

- 修改：
  - `src/fafafa.ssl.openssl.certificate.pas`
  - `tests/openssl/test_openssl_verify_ex_store_flag_isolation_contract.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

- 不做：
  - 不扩到
    `WinSSL`
    /
    `FreePascal`
  - 不重开
    `OpenSSL`
    strict-chain
    lane
  - 不在这一批里重构整个 OpenSSL cert-store 实现

## Why This Batch

当前源码 truth 已经明确：

- `TOpenSSLCertificate.VerifyEx`
  直接对 shared
  `Store`
  调：
  - `X509_STORE_set_flags(Store, X509_V_FLAG_NO_CHECK_TIME)`
  - `X509_STORE_set_flags(Store, X509_V_FLAG_PARTIAL_CHAIN)`
- 这两个 flag
  本质都应该是
  本次验证上下文的例外策略，
  不应该永久污染
  调用方传进来的 store

当前还没有 focused contract
钉住这一点，
所以这批先用最小可重复 RED
把状态泄漏打出来。

## TDD Steps

1. 新增 focused OpenSSL contract：
   - 用现有
     `expired-signer.pem`
     +
     `ca_cert.pem`
     和同一个 store
   - 第一次
     `VerifyEx(..., [])`
     必须失败
   - 第二次
     `VerifyEx(..., [sslCertVerifyIgnoreExpiry])`
     必须成功
   - 第三次再用同一个 store
     `VerifyEx(..., [])`
     仍然必须失败
   - 再用
     `version1-cert.pem`
     + 同一个 empty store
     验证：
     - `VerifyEx(..., [])`
       必须失败
     - `VerifyEx(..., [sslCertVerifyAllowSelfSigned])`
       必须成功
     - 随后再次
       `VerifyEx(..., [])`
       仍然必须失败
2. 运行 focused test，
   预期先 RED
3. 最小修复：
   - 把
     `IgnoreExpiry`
     从 shared store flag
     移到 per-call
     `X509_STORE_CTX`
     参数
   - 把
     `AllowSelfSigned`
     收紧成：
     - 只在 leaf
       确认 self-signed
     - 且错误确实属于 trust/self-signed failure
       时，
       对当前调用放行
4. Focused verification：
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_verify_ex_store_flag_isolation_contract_units -FEtmp/test_openssl_verify_ex_store_flag_isolation_contract_units -otmp/test_openssl_verify_ex_store_flag_isolation_contract_units/test_openssl_verify_ex_store_flag_isolation_contract tests/openssl/test_openssl_verify_ex_store_flag_isolation_contract.pas`
   - `./tmp/test_openssl_verify_ex_store_flag_isolation_contract_units/test_openssl_verify_ex_store_flag_isolation_contract`
   - `git diff --check`

## Expected Result

- `OpenSSL certificate.VerifyEx`
  不再把
  `IgnoreExpiry`
  /
  `AllowSelfSigned`
  写进 shared store
- 同一个 store
  上连续调用的结果
  只受本次 flags 影响
- OpenSSL certificate surface
  的 per-call 语义
  更符合 public interface 预期

## Execution Result

- PASS
- 重新跑当前未提交实现后，先确认：
  - `IgnoreExpiry`
    已经不再污染同一个 store
    上后续不带 flag 的调用
  - 所以前一版“需要继续下钻 OpenSSL X509 param 绑定”的怀疑，
    在当前运行时上被证伪
- 新补的 self-signed 同类 contract
  打出了真正 residual：
  - `sslCertVerifyAllowSelfSigned`
    之前并不是“还在泄漏”
  - 而是
    `X509_V_FLAG_PARTIAL_CHAIN`
    没有兑现当前 public surface
    承诺的
    self-signed leaf 放行语义
- 最终修法：
  - 保留
    `IgnoreExpiry`
    的 per-call verify-param 路径
  - 不再依赖
    `PARTIAL_CHAIN`
    近似实现
    `AllowSelfSigned`
  - 仅在
    self-signed leaf
    且错误属于
    self-signed / trust failure
    时，
    对当前调用做窄范围 override
- focused verification：
  - `tests/openssl/test_openssl_verify_ex_store_flag_isolation_contract.pas`
    - PASS
    - 同时覆盖：
      - `IgnoreExpiry` 不泄漏
      - `AllowSelfSigned` 生效且不泄漏
  - `git diff --check`
    - PASS
