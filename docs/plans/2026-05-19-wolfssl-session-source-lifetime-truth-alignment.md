# 2026-05-19 WolfSSL Session Source Lifetime Truth Alignment

## Goal

把 `TWolfSSLSession.FromConnection(...)` 从“直接包装连接内部 session 指针”的借用语义收紧成真正 secure ownership 的 session 提取路径，避免源连接释放后 public `ISSLSession` 变成悬空 native handle。

## Scope

- 不在本批承诺：
  - session extracted from connection 一定能命中 resumed-handshake
  - WinSSL / OpenSSL / MbedTLS 的 source-lifetime lane 同批重构
- 不重开：
  - helper-less deserialize 旧 lane
  - clone truth 旧 lane
  - WinSSL runtime / macOS loader 旧 lane
- 只收以下缺口：
  1. `WolfSSL.FromConnection()` 不再直接借用 `wolfSSL_get_session()` 返回的内部指针
  2. 无法 secure ownership 时 fail-closed，而不是继续把 borrowed handle 递出去
  3. focused framework test 与 cross-backend contract 继续保持 green

## Files

- `src/fafafa.ssl.wolfssl.api.pas`
- `src/fafafa.ssl.wolfssl.session.pas`
- `tests/test_wolfssl_framework.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `OpenSSL` 当前 `DoGetSession()` 已通过 `SSL_get1_session` 增加引用计数。
- `MbedTLS` 当前 `FromContext()` 会先分配独立 session，再用 `mbedtls_ssl_get_session()` 拷出内容。
- 但 `WolfSSL` 之前在 `FromConnection()` 里直接：
  - `LSession := wolfSSL_get_session(ASSL)`
  - `TWolfSSLSession.Create(LSession, False)`
- 这意味着返回给 public `ISSLSession` 的 native handle 仍然受源连接 lifetime 约束。

## Steps

1. 在 `tests/test_wolfssl_framework.pas` 增加 source-lifetime RED：
   - 有 duplication helper 时必须复制 borrowed session
   - 没有 ownership helper 时必须 fail-closed
2. 最小修复：
   - 绑定 `wolfSSL_SESSION_dup`
   - `FromConnection()` 先 secure ownership，再返回 session
   - 如果 `dup` 不可用，则退到 `i2d/d2i` duplication
3. focused 运行：
   - `tests/test_wolfssl_framework.pas`
4. cross-check：
   - `tests/contract/test_backend_contract.pas`
5. `git diff --check`

## Commands

```bash
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
- `TwolfSSL_SESSION_dup` 已接入动态绑定。
- `TWolfSSLSession.FromConnection()` 现在会先 secure ownership：
  - 优先 `wolfSSL_SESSION_dup`
  - 否则退到 `i2d/d2i` duplication
  - 都不可用时直接 `fail-closed`
- focused verification：
  - `tests/test_wolfssl_framework.pas`: `127 passed / 0 failed`
  - 新增 `WolfSSL Session Source Lifetime Contract` 全绿
  - `tests/contract/test_backend_contract.pas`: `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
  - `git diff --check`: PASS

## Next

- 下一刀优先继续查：
  - `GetPeerCertificate` / session metadata extraction 是否还有 source-lifetime 或 ownership 漂移
  - `MbedTLS/WolfSSL` session extraction 后的 metadata completeness 是否仍弱于 OpenSSL / FreePascal
- 不再把 `WolfSSL.FromConnection()` 的 borrowed-session lifetime gap 当成未定位问题重复拉起。
