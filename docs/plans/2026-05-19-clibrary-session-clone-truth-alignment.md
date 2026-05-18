# 2026-05-19 C-Library Session Clone Truth Alignment

## Goal

把 `MbedTLS` / `WolfSSL` 的 `ISSLSession.Clone()` 从“复制 metadata 但丢掉 native session”收紧成真正可继续使用的 clone，避免 public session object 在 backend 间继续出现“同样叫 Clone，结果一个能复用、一个直接失效”的语义漂移。

## Scope

- 不在本批承诺：
  - `Clone()` 一定跨进程可用
  - clone 后已经完成真实 resumed-handshake
- 不重开：
  - WinSSL runtime / native-probe 旧 lane
  - macOS loader/path 旧 lane
  - helper-less fake deserialize 已收口 lane
- 只收以下缺口：
  1. valid/resumable session clone 后不能变成 invalid
  2. clone 后不能再丢 native handle
  3. `WolfSSL.Serialize()` 不再优先回放 stale cached bytes

## Files

- `src/fafafa.ssl.mbedtls.session.pas`
- `src/fafafa.ssl.wolfssl.session.pas`
- `tests/test_mbedtls_framework.pas`
- `tests/test_wolfssl_framework.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `OpenSSL` 当前 `Clone()` 已通过 `SSL_SESSION_up_ref` 保留 native session truth。
- `FreePascal` 当前 `Clone()` 已做完整深拷贝。
- `WinSSL` 当前 `Clone()` 至少对 metadata 是自洽的。
- 但 `MbedTLS/WolfSSL` 之前对 valid session 的 `Clone()` 只复制字段与缓存字节，却把 `FSession=nil`，导致 clone 后：
  - `IsValid=False`
  - `IsResumable=False`
  - native handle 丢失
- 这不是“轻量 clone”，而是 public `ISSLSession` contract drift。

## Steps

1. 在 framework tests 里给 deserialized session 增加 clone RED。
2. 最小修复：
   - `MbedTLS.Clone()` 通过 serialize/deserialize materialize native session
   - `WolfSSL.Clone()` 通过 serialize/deserialize materialize native session
   - `WolfSSL.Serialize()` 优先输出当前 native session 的 i2d bytes
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
- `TMbedTLSSession.Clone()` 现在会在 native session 存在时重新 materialize clone session。
- `TWolfSSLSession.Clone()` 现在也会保留 valid/resumable/native-handle truth。
- `TWolfSSLSession.Serialize()` 现在优先输出当前 native session 的真实 `i2d` 结果，不再先回放 stale cache bytes。
- focused verification：
  - `tests/test_mbedtls_framework.pas`: `108 passed / 0 failed`
  - `tests/test_wolfssl_framework.pas`: `120 passed / 0 failed`
  - `tests/contract/test_backend_contract.pas`: `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
  - `git diff --check`: PASS

## Next

- 下一刀优先审 `MbedTLS/WolfSSL` 的 session ownership / lifetime 边界：
  - `FromContext/FromConnection` 拿到的 native session 是否仍可能在源连接释放后悬空
  - `GetPeerCertificate` / timeout / protocol metadata 是否还有 backend-specific truth drift
- 不再把 `Clone()` 的 valid/native-handle 丢失问题当成未定位缺口重复拉起。
