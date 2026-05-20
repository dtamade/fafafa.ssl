# MbedTLS CertStore Clone Fingerprint Parity

## Goal

把 `TMbedTLSCertificateStore` 的

- `Contains`
- `RemoveCertificate`
- duplicate `AddCertificate`

从当前只按对象身份判断，
收口到与当前仓库更稳定的 store contract 一致：

- 同一张证书的 clone
  应被视为同一 fingerprint truth
- duplicate clone
  不应再次加入 store
- clone
  应能命中 `Contains`
  并驱动 `RemoveCertificate`

## Scope

- 修改：
  - `src/fafafa.ssl.mbedtls.certificate.pas`
  - `tests/test_mbedtls_framework.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不扩到 `WolfSSL`
  已经存在的 fingerprint semantics
- 不重开 `BuildCertificateChain`
  dedup / loop family
- 不改当前刚落地的 DN query contract

## Architecture Truth

- `FreePascal`
  已支持：
  - clone `Contains`
  - clone `RemoveCertificate`
  - duplicate fingerprint reject
- `WolfSSL`
  也已支持：
  - clone `Contains`
  - clone `RemoveCertificate`
- `MbedTLS`
  当前还是：
  - `Contains` -> `FCertificates.IndexOf(ACert)`
  - `RemoveCertificate` -> `FCertificates.IndexOf(ACert)`
  - `AddCertificate` 只靠上面这条 identity 判断
- 这会导致：
  - 同一张证书 clone
    在 `MbedTLS` store
    被当成另一张证书
  - shared certstore contract
    在 optional backend 之间继续分裂

## Steps

1. 在 `tests/test_mbedtls_framework.pas`
   追加 RED：
   - `Contains clone should be true by fingerprint`
   - `Remove clone should remove by fingerprint`
   - `Add clone duplicate returns false`
2. 在 `TMbedTLSCertificateStore`
   做最小修复：
   - 复用 normalized fingerprint truth
   - `Contains` / `RemoveCertificate`
     增加 fingerprint fallback
   - `AddCertificate`
     拒绝 duplicate fingerprint
3. Focused verification：
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
   - `./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
   - `git diff --check`

## Expected Result

- `MbedTLS` certstore
  不再把 clone
  当作另一张证书
- optional backend store contract
  在 fingerprint semantics
  上更接近一致

## Execution Result

- PASS
- `tests/test_mbedtls_framework.pas`
  新增 RED：
  - `Contains clone should be true by fingerprint`
  - `Add clone duplicate returns false`
  - `Remove clone should remove by fingerprint`
- 首轮失败先暴露的是一个很窄的 test harness 问题：
  - 缺少 `LStoreClone` 局部变量声明
- 修复 test harness 后，
  `TMbedTLSCertificateStore`
  通过：
  - normalized fingerprint helper
  - `Contains` fingerprint fallback
  - `RemoveCertificate` fingerprint fallback
  - duplicate fingerprint reject
  最终收口
- focused verification：
  - `tests/test_mbedtls_framework.pas`
    `166 passed / 0 failed`
- 这批同时顺手修掉了上一批 push 后暴露的
  Windows workflow fallout：
  - `tests/winssl/test_winssl_certstore.lpi`
    删除了错误的
    `TargetOS=linux`
    硬编码
