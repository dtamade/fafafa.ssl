# Context ServerName Shared Compatibility Shim

## Goal

执行 `context-level ServerName` 迁移路线图中 Phase C 的第一刀：把五个 backend constructor 中分散的 context fallback 读取收成一条 shared compatibility shim，同时保持当前 runtime compatibility 行为不变。

## Architecture

- 只收拢 backend fallback seam：
  - `src/fafafa.ssl.openssl.connection.pas`
  - `src/fafafa.ssl.freepascal.connection.pas`
  - `src/fafafa.ssl.wolfssl.connection.pas`
  - `src/fafafa.ssl.mbedtls.connection.pas`
  - `src/fafafa.ssl.winssl.connection.pas`
- 新增一个共享 compatibility helper unit：
  - 统一封装 deprecated `ISSLContext.GetServerName` 读取
  - 统一封装 client-role gate
  - 第一刀只返回 string，不替 backend 做 setter/field side effect
- 不改 public/runtime semantics：
  - 不删除 context-level fallback
  - 不改变 connector / factory / builder 当前兼容行为
  - 不直接动最终 surface cleanup

## Files

- `src/fafafa.ssl.context.compat.pas`
- `src/fafafa.ssl.openssl.connection.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `src/fafafa.ssl.wolfssl.connection.pas`
- `src/fafafa.ssl.mbedtls.connection.pas`
- `src/fafafa.ssl.winssl.connection.pas`
- `tests/scripts/test_context_server_name_compat_shim_contract.sh`
- `tests/test_sslctxboth_client_capability_clarification.pas`
- `tests/test_factory_server_name_scope_clarification.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`
- `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`

## Steps

1. 先加 focused RED source contract：
   - 必须存在 shared helper
   - 五个 backend 必须调用 shared helper
   - 五个 backend 不再各自 direct read `AContext.GetServerName` / `FContext.GetServerName`
2. 生产修复：
   - 新增 shared helper unit
   - OpenSSL / MbedTLS 保持 `SetServerName(...)` side effect
   - FreePascal / WolfSSL / WinSSL 保持字段赋值路径
3. focused regressions：
   - `tests/scripts/test_context_server_name_compat_shim_contract.sh`
   - `tests/test_sslctxboth_client_capability_clarification.pas`
   - `tests/test_factory_server_name_scope_clarification.pas`
4. 收口：
   - 更新 `task_plan.md` / `findings.md` / `progress.md`
   - 更新 Phase C 路线图进度

## Expected Outputs

- backend constructor fallback 不再是五份分散的 direct deprecated reads
- deprecated warning suppression 收敛到一个共享 seam
- 当前兼容行为保持不变
- 下一批可以在共享 seam 上继续做最终迁移，而不是再次跨五 backend 散改
