# Certificate VerifyHostname Fixture Parity

## Goal

把 `ISSLCertificate.VerifyHostname(...)`
在高风险夹具语义上的 focused proof
补齐到当前主要 backend，
避免仓库继续只靠：

- `san-test.pem` 的基础 SAN/IP 命中证明
- 或单一 `FreePascal` runtime fixture

而遗漏更容易回归的两条规则：

- SAN 存在时优先于 CN
- wildcard 只匹配单层子域

## Scope

- 修改：
  - `tests/test_mbedtls_framework.pas`
  - `tests/test_wolfssl_framework.pas`
  - `tests/openssl/test_openssl_certificate_hostname_contract.pas`
  - `tests/winssl/test_winssl_certificate_san.pas`
  - `tests/winssl/test_winssl_certificate_san.lpi`
  - `tests/run_winssl_tests.ps1`
  - `src/fafafa.ssl.mbedtls.certificate.pas`
  - `src/fafafa.ssl.wolfssl.certificate.pas`
  - `src/fafafa.ssl.winssl.certificate.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

- 不做：
  - 不重开 `FindByIssuer` lane
  - 不重开 facade supporting-type export lane
  - 不扩成新的证书 API redesign
  - 不在没有真实红灯前先改 backend 生产实现

## Why This Batch

当前已经确认：

- `tests/test_freepascal_backend_basic.pas`
  已经用 fixture 锁住：
  - `san_cn_conflict_cert.pem`
  - `san_wildcard_cert.pem`
- 通用 backend contract
  `tests/contract/test_backend_contract.pas`
  只覆盖：
  - `tests/certs/san-test.pem`
  - 基础 SAN DNS/IP 命中
  - unrelated hostname 拒绝
- `MbedTLS` / `WolfSSL` / `OpenSSL` / `WinSSL`
  虽然都有 `VerifyHostname(...)` 实现，
  但还缺同级 focused proof

另外，
`tests/winssl/test_winssl_certificate_san.pas`
当前还存在两处会阻碍真实接线的问题：

- 夹具路径按仓库根假设，和 `tests/run_winssl_tests.ps1`
  的工作目录不一致
- `test_winssl_certificate_san.lpi`
  仍硬编码 `TargetOS=linux`

所以这一批不只是“补断言”，
也是把 WinSSL 这份现成 SAN 测试真正接入 runtime lane 的最小闭环。

## TDD Steps

1. 先在：
   - `tests/test_mbedtls_framework.pas`
   - `tests/test_wolfssl_framework.pas`
   - `tests/openssl/test_openssl_certificate_hostname_contract.pas`
   - `tests/winssl/test_winssl_certificate_san.pas`
   追加 fixture parity 断言
2. 本地先跑 Linux 可执行的 focused tests，观察是否 RED：
   - `test_mbedtls_framework`
   - `test_wolfssl_framework`
   - `test_openssl_certificate_hostname_contract`
3. 若 Linux 侧有真实失败：
   - 只修对应 backend 的 `VerifyHostname(...)`
   - 不扩 scope
4. WinSSL 侧先做静态接线修复：
   - 统一 fixture path resolution
   - 去掉错误的 `TargetOS=linux`
   - 接入 `tests/run_winssl_tests.ps1`
5. 推送后看 GitHub `WinSSL Runtime Gate`
   是否帮我们完成 Windows proof

## Verification

```bash
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas
./tmp/test_mbedtls_framework_units/test_mbedtls_framework
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas
./tmp/test_wolfssl_framework_units/test_wolfssl_framework
fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_openssl_hostname_units -FEtmp/test_openssl_hostname_bin -otmp/test_openssl_hostname_bin/test_openssl_certificate_hostname_contract tests/openssl/test_openssl_certificate_hostname_contract.pas
./tmp/test_openssl_hostname_bin/test_openssl_certificate_hostname_contract
git diff --check
```

Windows / CI proof:

```text
GitHub Actions: WinSSL Runtime Gate
```

## Expected Outcome

- `FreePascal` 不再是唯一拥有
  SAN-vs-CN / wildcard 单层语义 fixture proof 的 backend
- `MbedTLS` / `WolfSSL` / `OpenSSL`
  本地都有 focused parity evidence
- `WinSSL` 的 SAN test 不再是脱离 runtime suite 的孤儿文件
- 如果某个 backend 实现真的不一致，
  我们会在这批里拿到第一手 RED，而不是继续靠静态猜测
