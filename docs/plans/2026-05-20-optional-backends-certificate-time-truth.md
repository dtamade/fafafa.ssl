# Optional Backends Certificate Time Truth

## Goal

把 `MbedTLS` / `WolfSSL` 证书对象对外发布的时间相关 surface：

- `GetNotBefore`
- `GetNotAfter`
- `IsExpired`
- `GetDaysUntilExpiry`

从当前的默认值壳 / PEM-only 解析路径，
收紧成与真实 X.509 validity 一致的 public truth，
避免调用方继续遇到：

- 空证书被报告成“默认一年有效”
- DER / native materialized 证书丢失有效期时间
- `DaysUntilExpiry` / `IsExpired` 基于伪造时间继续产生误导

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

- 不重开 broader certificate metadata redesign
- 不扩新的 native X509 API binding
- 不跑重型全仓 compile gate

## Architecture Truth

- 两个 optional backend 证书实现都已经有
  `TryLoadX509Parser(...)`
  可复用：
  - 优先读缓存 DER
  - 其次读 PEM
  - 否则从 native handle 导出 DER 再解析
- `TWolfSSLCertificate.GetNotBefore/GetNotAfter`
  当前仍只走
  `FPEMData`
  解析；
  这意味着：
  - `LoadFromDER(...)`
  - peer-cert / clone 等 native materialized 路径
  仍可能丢失时间真相
- `TMbedTLSCertificate.GetNotBefore/GetNotAfter`
  当前仍靠
  `mbedtls_x509_crt_info(...)`
  文本切片
  + 默认 `Now +/- 365`；
  空证书状态会被伪装成有有效期
- 仓库文档
  `docs/reference/ARCHITECTURE.md`
  已明确把 certificate validity
  作为跨 backend 应对齐的 public semantics

## Steps

1. 在 framework tests 中制造 RED：
   - `MbedTLS` 空证书：
     - `GetNotBefore = 0`
     - `GetNotAfter = 0`
     - `IsExpired = False`
     - `GetDaysUntilExpiry = 0`
   - `MbedTLS` / `WolfSSL`：
     - file-loaded fixture 导出 DER 后重新 `LoadFromDER`
     - `GetNotBefore/GetNotAfter` 仍非零
     - DER-loaded truth 与原始 loaded truth 保持一致
2. 在实现中做最小修复：
   - `MbedTLS` 时间 getter 优先复用 parser truth
   - `WolfSSL` 时间 getter 改成复用 parser truth
   - `MbedTLS.IsExpired/GetDaysUntilExpiry`
     对 unknown time fail-closed
3. Focused verification：
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_framework_units -FEtmp/test_mbedtls_framework_units -otmp/test_mbedtls_framework_units/test_mbedtls_framework tests/test_mbedtls_framework.pas`
   - `./tmp/test_mbedtls_framework_units/test_mbedtls_framework`
   - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_wolfssl_framework_units -FEtmp/test_wolfssl_framework_units -otmp/test_wolfssl_framework_units/test_wolfssl_framework tests/test_wolfssl_framework.pas`
   - `./tmp/test_wolfssl_framework_units/test_wolfssl_framework`
   - `git diff --check`

## Expected Result

- `MbedTLS` 空证书不再伪造 validity 时间
- `WolfSSL` DER/native 路径不再丢失 validity 时间
- optional backend 的 certificate time surface
  更接近统一的 parser-backed truth

## Execution Result

- PASS
- `gh run view 26143487129`
  已确认上一批
  `certificate version truth`
  的远端
  `CI`
  全绿，
  本批不需要先做回归救火
- `MbedTLS`
  新增 time-truth contract
  首轮 RED
  精确打出 3 个失败：
  - `Empty cert NotBefore is unknown`
  - `Empty cert NotAfter is unknown`
  - `Empty cert DaysUntilExpiry is 0`
- `WolfSSL`
  同批 DER-roundtrip control contract
  首轮即 GREEN，
  说明这次真正的实现缺口
  不在
  `WolfSSL`
  而在
  `MbedTLS`
  的 empty-state 默认值壳
- 最终收口：
  - `TMbedTLSCertificate.GetNotBefore/GetNotAfter`
    现在优先复用
    `TX509Certificate.Validity`
  - unknown time
    不再伪造成
    `Now +/- 365`
  - `IsExpired`
    /
    `GetDaysUntilExpiry`
    对 unknown time
    改成 fail-closed
- focused verification：
  - `tests/test_mbedtls_framework.pas`
    - `189 passed / 0 failed`
  - `tests/test_wolfssl_framework.pas`
    - `199 passed / 0 failed`
  - `git diff --check`
    - PASS
