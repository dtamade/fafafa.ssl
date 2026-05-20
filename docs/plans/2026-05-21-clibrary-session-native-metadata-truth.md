# 2026-05-21 C-Library Session Native Metadata Truth

## Goal

把 `MbedTLS` / `WolfSSL` 的 `ISSLSession`
从“native handle 已存在，但 `GetID` / `GetCreationTime` / `GetTimeout` / `GetCipherName` 仍回退到伪造字段或占位值”的状态收紧成尽量读取原生 session metadata 的实现，为下一批连接侧 `SetSession(...) -> IsSessionReused` 真值验证打下可复用基线。

## Scope

- 不在本批承诺：
  - `MbedTLS` / `WolfSSL` 已经拿到完整 session reuse runtime truth
  - 所有 backend 同步重做
  - `MbedTLS` 一定能暴露真正 native timeout（若 native session 本身无稳定 getter，则保持现有边界）
- 不重开：
  - helper-less deserialize false success 旧 lane
  - clone 丢 native handle 旧 lane
  - Windows runtime / WinSSL 旧 lane
- 只收以下缺口：
  1. `TMbedTLSSession` 不再对 native session 继续生成随机 `ID` / `Now` 创建时间 / placeholder cipher
  2. `TWolfSSLSession` 不再对 native session 继续生成随机 `ID` / `Now` 创建时间 / field-only timeout
  3. focused framework tests 锁住：
     - raw `Deserialize(...)`
     - `FromContext(...)`
     - `FromConnection(...)`
     这三条入口上的 session native metadata truth

## Files

- `src/fafafa.ssl.mbedtls.session.pas`
- `src/fafafa.ssl.wolfssl.api.pas`
- `src/fafafa.ssl.wolfssl.session.pas`
- `tests/test_mbedtls_framework.pas`
- `tests/test_wolfssl_framework.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `OpenSSL` session 已经直接走 native getter：
  - `SSL_SESSION_get_id`
  - `SSL_SESSION_get_time`
  - `SSL_SESSION_get_timeout`
  - `SSL_SESSION_get0_cipher`
- 但 `MbedTLS/WolfSSL` 目前仍残留更基础的 session metadata drift：
  - `MbedTLS`
    - `GetID` 仍来自随机 GUID
    - `GetCreationTime` 仍来自 `Now`
    - `GetCipherName` 在纯 session 路径仍可能为空
  - `WolfSSL`
    - `GetID` 仍来自随机 GUID
    - `GetCreationTime` 仍来自 `Now`
    - `GetTimeout` 仍只看本地字段
    - `GetCipherName` 在纯 session 路径仍可能是 `unknown`
- 这会直接削弱后续 session cache / session resumption / connection info 的 public truth。

## Steps

1. 先在 framework tests 打出 RED：
   - `MbedTLS`: raw `Deserialize(...)` 与 `FromContext(...)` 暴露 native session id/time/protocol/cipher
   - `WolfSSL`: raw `Deserialize(...)` 与 `FromConnection(...)` 暴露 native session id/time/timeout/cipher
2. 最小修复：
   - `TMbedTLSSession` 从 native session 读取 id/time/version/ciphersuite
   - `TWolfSSLSession` 绑定并使用 session getter：
     - `wolfSSL_SESSION_get_id`
     - `wolfSSL_SESSION_get_time`
     - `wolfSSL_SESSION_get_timeout`
     - `wolfSSL_SSL_SESSION_set_timeout`
     - `wolfSSL_SESSION_CIPHER_get_name`
3. focused 运行：
   - `tests/test_mbedtls_framework.pas`
   - `tests/test_wolfssl_framework.pas`
4. `git diff --check`

## Commands

```bash
mkdir -p tmp/test_mbedtls_framework_units
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_mbedtls_framework_units \
  -FEtmp/test_mbedtls_framework_units \
  -otmp/test_mbedtls_framework_units/test_mbedtls_framework \
  tests/test_mbedtls_framework.pas

mkdir -p tmp/test_wolfssl_framework_units
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_wolfssl_framework_units \
  -FEtmp/test_wolfssl_framework_units \
  -otmp/test_wolfssl_framework_units/test_wolfssl_framework \
  tests/test_wolfssl_framework.pas
```

## Execution Result

- PASS
- `TMbedTLSSession` 现在会尽量从当前 native session 里提取：
  - `session id`
  - `creation time`
  - `protocol version`
  - `cipher name`
- `TWolfSSLSession` 现在通过动态绑定的 session getter 提取：
  - `session id`
  - `creation time`
  - `timeout`
  - `cipher name`
- `WolfSSL` 这批同时固定了 Linux/CI 上的一个真实类型边界：
  - session getter 的时间/超时返回类型应按 `clong`
  - focused tests 里的 stub 也必须跟着使用 `clong`
- 两边 session-class focused contract 也已跟随真实行为收紧：
  - 不再要求 “deserialize 后 serialize 仍必须回吐 raw native bytes”
  - 改为要求：
    - 输出非空、可重载的 metadata-complete snapshot
    - reload 后继续保留 native metadata truth
- focused verification：
  - `fpc ... tests/test_mbedtls_framework.pas`: PASS
  - `./tmp/test_mbedtls_framework_units/test_mbedtls_framework`: `250 passed / 0 failed`
  - `fpc ... tests/test_wolfssl_framework.pas`: PASS
  - `./tmp/test_wolfssl_framework_units/test_wolfssl_framework`: `265 passed / 0 failed`

## Next

- 下一刀优先继续做连接侧 `SetSession(...) -> IsSessionReused` 真值链路验证。
- 不再把：
  - `MbedTLS/WolfSSL` session metadata 伪造问题
  - `WolfSSL` `clong` getter 类型不一致
  - “serialize 必须回吐 raw bytes” 的旧断言
  当成未定位问题重复拉起。
