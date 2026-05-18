# 2026-05-19 C-Library Session Metadata And Peer-Certificate Completeness

## Goal

把 `MbedTLS` / `WolfSSL` 的 `ISSLSession` 提取路径从“只有 native session truth，metadata/peer cert 仍残缺”收紧成真正可复用的 public session surface，避免 `FromContext()` / `FromConnection()` 返回的 session 继续在协议版本、cipher 与 peer certificate 上弱于连接态真相。

## Scope

- 不在本批承诺：
  - `MbedTLS` / `WolfSSL` session 一定能跨进程恢复
  - `FreePascal` / `OpenSSL` / `WinSSL` 同批一起重构
  - `WolfSSL` 单连接 `DoGetPeerCertificate()` surface 全量重做
- 不重开：
  - WinSSL runtime / native-probe 旧 lane
  - `Clone()` native handle 丢失旧 lane
  - `FromConnection()` borrowed-session ownership 旧 lane
- 只收以下缺口：
  1. `TMbedTLSSession.FromContext(...)` 要补齐 version / cipher / peer cert
  2. `TWolfSSLSession.FromConnection(...)` 要补齐 peer cert，并让 clone 后仍保留该 truth
  3. helper 缺失时必须 fail-closed，而不是留 borrowed cert 壳对象

## Files

- `src/fafafa.ssl.mbedtls.session.pas`
- `src/fafafa.ssl.wolfssl.api.pas`
- `src/fafafa.ssl.wolfssl.certificate.pas`
- `src/fafafa.ssl.wolfssl.session.pas`
- `tests/test_mbedtls_framework.pas`
- `tests/test_wolfssl_framework.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `MbedTLS` 当前 `FromContext()` 已会安全拷出 native session，但仍把：
  - protocol 固定成 `TLS1.2`
  - cipher 留空
  - peer cert 固定为 `nil`
- `WolfSSL` 当前 `FromConnection()` 已解决 session ownership，且会补 version / cipher，
  但 peer cert 仍未 materialize。
- `mbedtls_ssl_get_peer_cert()` 的本机头文件明确要求：
  - 如果要跨后续 SSL API 调用继续使用证书，调用方必须自己复制。
- 本机 `wolfssl/test.h` 使用模式显示：
  - `wolfSSL_get_peer_certificate(ssl)` 取得的 `peer` 会在使用后显式 `wolfSSL_FreeX509(peer)`。
- 因而这批的安全边界应是：
  - `MbedTLS`: borrowed cert -> copy DER -> reload owned cert
  - `WolfSSL`: owned/native cert -> export DER -> reload owned cert

## Steps

1. 在 backend framework tests 里增加 RED：
   - `MbedTLS`: session extracted from context exposes version/cipher/peer cert truth
   - `WolfSSL`: session extracted from connection exposes peer cert truth and clone preserves it
   - 两边都验证 helper-loss fail-closed
2. 最小修复：
   - `TMbedTLSSession` 新增 peer cert 存储与 metadata 回填
   - `TWolfSSLCertificate.SaveToDER()` 支持直接从 native X509 导出
   - `TWolfSSLSession` 新增 peer cert materialization 与 clone copy
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
- `TMbedTLSSession.FromContext(...)` 现在会真实提取 protocol / cipher，并把 borrowed peer cert materialize 成可独立持有的 owned cert。
- `TWolfSSLSession.FromConnection(...)` 现在也会 materialize peer cert；clone 后继续保留这条 truth。
- `TMbedTLSCertificate.Clone()` 不再是 cached-field shell；会重新 materialize native cert。
- `TWolfSSLCertificate.SaveToDER()` 现在支持直接从 native `WOLFSSL_X509` 导出 DER。
- focused verification：
  - `tests/test_mbedtls_framework.pas`: `116 passed / 0 failed`
  - `tests/test_wolfssl_framework.pas`: `136 passed / 0 failed`
  - `tests/contract/test_backend_contract.pas`: `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
  - `git diff --check`: PASS
