# 2026-05-19 C-Library Session Serialization Truth Alignment

## Goal

把 `MbedTLS` / `WolfSSL` 的 `ISSLSession.Serialize/Deserialize` 从“helper 缺失时也假成功”的漂移状态收紧成 fail-closed truth，确保 C-library backend 的 session surface 至少不再对外宣称一条并不存在的可恢复路径。

## Scope

- 不在本批承诺：
  - serialized payload 已可直接驱动 native resumed-handshake
  - session object 已能完整还原协议/套件/creation-time 等全部 native metadata
- 不重开：
  - WinSSL runtime / native probe / release closeout 旧 lane
  - macOS loader/path 旧 lane
- 只收以下实现缺口：
  1. helper 缺失时 `Deserialize(...)` 不能再返回假成功
  2. `Serialize(...)` 不能再只回放“之前随手塞进去的缓存字节”
  3. focused framework tests 与 cross-backend contract 必须继续保持 green

## Files

- `src/fafafa.ssl.mbedtls.api.pas`
- `src/fafafa.ssl.mbedtls.session.pas`
- `src/fafafa.ssl.wolfssl.session.pas`
- `tests/test_mbedtls_framework.pas`
- `tests/test_wolfssl_framework.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `MbedTLS` 当前官方 surface 已提供：
  - `mbedtls_ssl_session_load`
  - `mbedtls_ssl_session_save`
- 因而 `TMbedTLSSession` 不应继续把 `Deserialize(...)` 实现成“只缓存传入字节”。
- `WolfSSL` 若缺失 `wolfSSL_d2i_SSL_SESSION`，则当前 backend 并没有 native deserialize 能力。
- 对 public `ISSLSession` 而言，helper 缺失时的正确语义不是“缓存成功”，而是 `False`。

## Steps

1. 先在 framework tests 打出 RED，证明 helper 缺失时存在假成功。
2. 最小修复：
   - `MbedTLS` 接入 `session_load/save`
   - `MbedTLS` helper 缺失时 fail-closed
   - `WolfSSL` helper 缺失时 fail-closed
3. focused 运行：
   - `tests/test_mbedtls_framework.pas`
   - `tests/test_wolfssl_framework.pas`
4. cross-check：
   - `tests/contract/test_backend_contract.pas`
5. `git diff --check`

## Commands

```bash
mkdir -p tmp/test_mbedtls_framework_units && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_mbedtls_framework_units \
  -FEtmp/test_mbedtls_framework_units \
  -otmp/test_mbedtls_framework_units/test_mbedtls_framework \
  tests/test_mbedtls_framework.pas && \
./tmp/test_mbedtls_framework_units/test_mbedtls_framework

mkdir -p tmp/test_wolfssl_framework_units && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_wolfssl_framework_units \
  -FEtmp/test_wolfssl_framework_units \
  -otmp/test_wolfssl_framework_units/test_wolfssl_framework \
  tests/test_wolfssl_framework.pas && \
./tmp/test_wolfssl_framework_units/test_wolfssl_framework

mkdir -p tmp/backend_contract_units && \
fpc -B -Fu./src -Fu./tests \
  -FUtmp/backend_contract_units \
  -FEtmp/backend_contract_units \
  -otmp/backend_contract_units/test_backend_contract \
  tests/contract/test_backend_contract.pas && \
./tmp/backend_contract_units/test_backend_contract

git diff --check
```

## Execution Result

- PASS
- `TMbedTLSSession` 现在：
  - 绑定 `mbedtls_ssl_session_load/save`
  - `Deserialize(...)` 在 helper 缺失时 fail-closed
  - `Serialize(...)` 优先输出 helper 真实生成的 bytes
- `TWolfSSLSession` 现在：
  - `Deserialize(...)` 在 `wolfSSL_d2i_SSL_SESSION` 缺失时 fail-closed
  - 不再把原始输入字节误记成“已恢复 session”
- focused verification：
  - `tests/test_mbedtls_framework.pas`: `104 passed / 0 failed`
  - `tests/test_wolfssl_framework.pas`: `112 passed / 0 failed`
  - `tests/contract/test_backend_contract.pas`: `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
  - `git diff --check`: PASS

## Next

- 下一刀优先审 `MbedTLS/WolfSSL` 的 `ISSLSession.Clone()` 是否仍会复制出“metadata 在、native session 不在”的弱语义。
- 不再回头把本批 helper-less false success 当成未定位问题重复拉起。
