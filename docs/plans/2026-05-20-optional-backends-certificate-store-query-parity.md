# Optional Backends Certificate Store Query Parity

## Goal

把 `MbedTLS` / `WolfSSL` 证书存储对象
对外发布的两条高频查询 surface：

- `FindBySubject`
- `FindBySerialNumber`

从 backend-specific / fragile string compare
收紧成与当前仓库既有 store contract
一致的 query truth，
避免调用方在 optional backends 上继续遇到：

- 同一 subject
  只因大小写 / 空格格式不同
  就查不到
- serial
  只因 `AA:BB` / `aabb` / 带空格
  这些展示格式不同
  就查不到
- 空查询
  意外命中第一张证书

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

- 不新开 store index/cache family
- 不在这一批里重写 `FindByIssuer`
- 不改 OpenSSL / WinSSL / FreePascal 的既有实现

## Architecture Truth

- `TFreePascalCertificateStore`
  已经有更稳的 query contract：
  - `FindBySubject`
    会做大小写与分隔符空格归一化
  - `FindBySerialNumber`
    会把查询和证书 serial
    都归一化成十六进制 truth
- `TWolfSSLCertificateStore`
  当前只补了
  `FindBySubject`
  的文本归一化，
  `FindBySerialNumber`
  仍是裸字符串比较
- `TMbedTLSCertificateStore`
  当前两条查询
  都还停留在最原始比较：
  - `FindBySubject`
    原样 `Pos(...)`
  - `FindBySerialNumber`
    原样 `=`
- 由于 `TMbedTLSCertificateStore.FindBySubject('')`
  当前走
  `Pos('', Subject) > 0`，
  所以空查询会错误命中第一张证书；
  这属于真实 fail-open bug

## Steps

1. 在 `MbedTLS` / `WolfSSL` framework tests 中制造 RED：
   - `FindBySubject`
     支持 normalized subject query
   - `FindBySubject('') = nil`
   - `FindBySerialNumber`
     支持 normalized serial query
2. 在两组 store 实现中做最小修复：
   - `MbedTLS`
     补 subject / serial 归一化
   - `WolfSSL`
     补 serial 归一化
3. focused verification：
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
   - `./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas`
   - `./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
   - `git diff --check`

## Expected Result

- `MbedTLS` / `WolfSSL`
  在 store 查询层
  不再因为 subject / serial 展示格式差异
  产生假阴性
- `MbedTLS`
  的空 subject 查询
  不再错误命中第一张证书
- focused tests 通过并形成可复用台账

## Execution Result

- PASS
- `MbedTLS`
  新增 store-query contract
  首轮 RED
  打出 2 个失败：
  - `FindBySubject supports normalized query variant`
  - `FindBySerialNumber supports normalized query variant`
  修复后最终 `155 passed / 0 failed`
- `WolfSSL`
  同类 contract
  首轮 RED
  打出 1 个失败：
  - `FindBySerialNumber supports normalized query variant`
  修复后最终 `174 passed / 0 failed`
- 本批最终收口：
  - `TMbedTLSCertificateStore`
    现在会对：
    - subject query
    - serial query
    做归一化匹配
  - `TWolfSSLCertificateStore`
    现在会对 serial query
    做归一化匹配
- 这批明确不扩：
  - `FindByIssuer`
  - store index/cache family
  只收口当前最直接的 public query drift
- `git diff --check`
  通过
