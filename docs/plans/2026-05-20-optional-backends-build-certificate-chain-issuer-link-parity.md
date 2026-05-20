# Optional Backends BuildCertificateChain Issuer-Link Parity

## Goal

把 `MbedTLS` / `WolfSSL` 证书存储对象的
`BuildCertificateChain`
从当前只靠 store subject lookup 的简化实现，
收口到与现有 `FreePascal` 更一致的 public chain truth：

- 先尊重 `ISSLCertificate.GetIssuerCertificate()`
  已经携带的显式 issuer-link
- 当 store 里没有 issuer 时，
  也能构出 leaf -> issuer 的最小链
- 使用 fingerprint / object 级别的去重，
  避免引入显式 issuer-link 后更容易掉进循环

## Scope

- 修改：
  - `src/fafafa.ssl.mbedtls.certificate.pas`
  - `src/fafafa.ssl.wolfssl.certificate.pas`
  - `tests/test_freepascal_backend_basic.pas`
  - `tests/test_mbedtls_framework.pas`
  - `tests/test_wolfssl_framework.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不重开 `OpenSSL` / `WinSSL` 原生 chain engine 语义
- 不扩到完整 path validation / CRL / OCSP
- 不跑重型全仓 compile gate

## Architecture Truth

- `FreePascal`
  的 `BuildCertificateChain`
  已先读 `GetIssuerCertificate()`，
  再 fallback 到 `FindBySubject(GetIssuer)`，
  还做了 object / fingerprint 去重
- `MbedTLS` / `WolfSSL`
  当前还停在：
  - append current
  - `FindBySubject(GetIssuer)`
  - max depth break
- 这样会导致：
  - certificate 自己已经携带 issuer-link truth，
    但 optional backend 仍然看不见
  - earlier peer-cert / clone issuer-link 修复
    不能传导到 certstore chain building
  - 一旦未来开始消费显式 issuer-link，
    还必须补 loop suppression

## Steps

1. 把 `FreePascal` 现有那个被 self-signed fixture 掩盖的 chain 测试改成真正的 non-self-signed issuer-link contract
2. 在 `tests/test_mbedtls_framework.pas` / `tests/test_wolfssl_framework.pas`
   增加 RED：
   - store 不含 issuer 时仍能沿显式 issuer-link 返回两段链
   - returned issuer 保持同一 fingerprint truth
3. 最小修复：
   - `MbedTLS` / `WolfSSL` 先读 `GetIssuerCertificate()`
   - fallback 仍保留 `FindBySubject(GetIssuer)`
   - 引入 object / fingerprint 去重
4. Focused verification：
   - `tests/test_freepascal_backend_basic.pas`
   - `tests/test_mbedtls_framework.pas`
   - `tests/test_wolfssl_framework.pas`
   - `git diff --check`

## Expected Result

- 显式 issuer-link 不再只对 `FreePascal` 有效
- optional backend certstore chain truth 与已有 issuer-link public surface 更一致
- chain builder 在开始消费 issuer-link 后，不会因 clone / loop 更容易失控

## Execution Result

- PASS
- `tests/test_freepascal_backend_basic.pas`
  不再用会被 `IsSelfSigned`
  提前短路的 self-signed 夹具
  假装覆盖 chain dedup / issuer-link；
  现在改成真实的
  non-self-signed leaf + explicit issuer fixture
- `TMbedTLSCertificateStore`
  / `TWolfSSLCertificateStore`
  现在都会：
  - 先读取 `GetIssuerCertificate()`
  - 再 fallback 到 `FindBySubject(GetIssuer)`
  - 在追加下一跳前做 object / fingerprint 去重
- focused verification：
  - `tests/test_freepascal_backend_basic.pas`
    - PASS
  - `tests/test_mbedtls_framework.pas`
    - `171 passed / 0 failed`
  - `tests/test_wolfssl_framework.pas`
    - `185 passed / 0 failed`
  - `git diff --check`
    - PASS
