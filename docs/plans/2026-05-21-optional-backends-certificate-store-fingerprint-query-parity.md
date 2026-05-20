# Optional Backends Certificate Store Fingerprint Query Parity

## Goal

把 `MbedTLS` / `WolfSSL` 证书存储对象的
`FindByFingerprint`
从当前 raw-string compare
收紧成与现有
`OpenSSL` / `FreePascal` / `WinSSL`
一致的 normalized query truth，
避免调用方在 optional backends 上继续遇到：

- 同一张证书
  只因指纹字符串大小写不同
  就查不到
- 同一张证书
  只因带不带 `:`
  或首尾空白不同
  就查不到

## Scope

- 修改：
  - `src/fafafa.ssl.mbedtls.certificate.pas`
  - `src/fafafa.ssl.wolfssl.certificate.pas`
  - `tests/test_mbedtls_framework.pas`
  - `tests/test_wolfssl_framework.pas`
  - `tests/test_freepascal_backend_basic.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

- 不做：
  - 不重开 `FindBySubject` / `FindByIssuer` / `FindBySerialNumber`
  - 不改 `WinSSL` runtime lane
  - 不在这一批里重构整个 store indexing/cache

## Architecture Truth

- 当前 shipped store truth 已经不一致：
  - `OpenSSL.FindByFingerprint`
    会去掉 `:` 并统一大写
  - `FreePascal.FindByFingerprint`
    会做指纹归一化
  - `WinSSL.FindByFingerprint`
    会去掉 `:` 并统一大写
  - `MbedTLS` / `WolfSSL`
    却仍是直接拿
    `GetFingerprintSHA1`
    /
    `GetFingerprintSHA256`
    做原样字符串比较
- 同两个 optional backends
  自己的
  `Contains`
  /
  `RemoveCertificate`
  /
  chain de-dup
  已经都在用
  `Normalize*Fingerprint(...)`
  helpers，
  所以 `FindByFingerprint`
  继续停留在 raw-string compare
  是明显的 public query residual，
  不是 helper 缺失

## Steps

1. 在现有 framework tests 里补
   normalized fingerprint query contract：
   - lower-case
   - 带 `:`
   - 首尾空白
2. 先运行
   `MbedTLS`
   /
   `WolfSSL`
   focused tests，
   观察 RED
3. 用现有
   `NormalizeMbedTLSCertFingerprint(...)`
   /
   `NormalizeWolfCertFingerprint(...)`
   收口实现
4. 再跑同一组 focused proof，
   确认 GREEN
5. 同步三本台账并提交

## Focused Proof

```bash
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_mbedtls_framework_units \
  -FEtmp/test_mbedtls_framework_units \
  -otmp/test_mbedtls_framework_units/test_mbedtls_framework \
  tests/test_mbedtls_framework.pas

./tmp/test_mbedtls_framework_units/test_mbedtls_framework

fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_wolfssl_framework_units \
  -FEtmp/test_wolfssl_framework_units \
  -otmp/test_wolfssl_framework_units/test_wolfssl_framework \
  tests/test_wolfssl_framework.pas

./tmp/test_wolfssl_framework_units/test_wolfssl_framework

fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_freepascal_backend_basic_units \
  -FEtmp/test_freepascal_backend_basic_units \
  -otmp/test_freepascal_backend_basic_units/test_freepascal_backend_basic \
  tests/test_freepascal_backend_basic.pas

./tmp/test_freepascal_backend_basic_units/test_freepascal_backend_basic

git diff --check
```
