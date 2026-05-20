# 2026-05-21 C-Library Session Deserialize Metadata Completeness

## Goal

把 `MbedTLS` / `WolfSSL` 的 `ISSLSession` 从“native deserialize 成功但 protocol/cipher metadata 丢回默认值”的状态收紧成可 round-trip 当前已知 metadata 的实现，避免 public session surface 在 `Serialize -> Deserialize -> Clone` 路径上继续出现 backend-specific truth drift。

## Scope

- 不在本批承诺：
  - serialized payload 可直接给外部 native API 当成稳定跨版本格式
  - peer certificate / resumed flag / 全部 session internals 都随 payload 一起恢复
  - WinSSL / OpenSSL / FreePascal 同批一起重构
- 不重开：
  - helper-less fake success 旧 lane
  - clone 丢 native handle 旧 lane
  - Windows runtime / WinSSL 本地证明旧 lane
- 只收以下缺口：
  1. `MbedTLS/WolfSSL` metadata-complete session 经 `Serialize -> Deserialize` 后仍要保住 `protocol/cipher` truth
  2. 反序列化后的 session 再 `Clone()` 不能把这条 truth 丢掉
  3. 旧 raw native payload 仍要保持可反序列化兼容

## Files

- `src/fafafa.ssl.mbedtls.session.pas`
- `src/fafafa.ssl.wolfssl.session.pas`
- `tests/test_mbedtls_framework.pas`
- `tests/test_wolfssl_framework.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `FromContext(...)` / `FromConnection(...)` 已经能从 live connection truth 提取：
  - protocol version
  - cipher name
  - 部分 peer certificate truth
- 但 `Deserialize(...)` 当前只恢复 native session handle，
  - `TMbedTLSSession.ExtractSessionInfo` 会退回 `TLS1.2 + empty cipher`
  - `TWolfSSLSession.ExtractSessionInfo` 会退回 `unknown + unknown`
- 这意味着：
  - 同一个 public `ISSLSession`
    在 live extraction 路径上有 metadata truth，
    到 serialize/deserialize 路径上却失真
- 本批正确收法不是继续假装 native session handle 自带这些 metadata，
  而是：
  - 对 metadata-complete session 输出带 envelope 的 serialized snapshot
  - `Deserialize(...)` 同时兼容：
    - 新 envelope payload
    - 旧 raw native payload

## Steps

1. 先在 backend framework tests 打出 RED：
   - `MbedTLS`: metadata-complete session roundtrip 后保持 protocol/cipher truth
   - `WolfSSL`: metadata-complete session roundtrip 后保持 protocol/cipher truth
2. 最小修复：
   - `Serialize(...)` 在 metadata-complete session 上输出 `native payload + metadata envelope`
   - `Deserialize(...)` 优先解析 envelope；若不是 envelope，则回退旧 raw payload 语义
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
bash ./tmp/test_mbedtls_framework_units/test_mbedtls_framework

mkdir -p tmp/test_wolfssl_framework_units
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_wolfssl_framework_units \
  -FEtmp/test_wolfssl_framework_units \
  -otmp/test_wolfssl_framework_units/test_wolfssl_framework \
  tests/test_wolfssl_framework.pas
bash ./tmp/test_wolfssl_framework_units/test_wolfssl_framework

git diff --check
```
