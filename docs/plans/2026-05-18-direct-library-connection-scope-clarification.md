# Direct-Library Connection-Scope Clarification

## Goal

收口 `ISSLLibrary.SetDefaultConfig(...)` + `CreateContext(AType)` 这条 direct-library path 上的 `TSSLConfig.HandshakeTimeout` / `BufferSize` scope drift：它们当前是 connection-scoped 字段，但 backend library `CreateContext(...)` 仍会静默接受通过 default-config 注入的自定义值。

## Why This Batch

当前 `TSSLConfig` 路线已经明确：

- `HandshakeTimeout` / `BufferSize` 是 connection-scoped
- `TSSLFactory.CreateContext(const AConfig)` 会 reject 自定义值
- `TSSLFactory.CreateContext(AContextType, ALibType)` 也会 reject 通过 library default 注入的自定义值

但 direct-library path 还缺最后一刀：

- `ISSLLibrary.SetDefaultConfig(...)` 目前会保留自定义 `HandshakeTimeout` / `BufferSize`
- 五个 backend 的 `CreateContext(AType)` 都不会消费这两个字段
- 当前也没有 fail-fast reject

这会留下一个“default-config 看起来可写、CreateContext 实际静默忽略”的 live drift。

## Deliverables

1. 新增 focused RED runtime test，证明 FreePascal direct-library path 仍会静默接受这两个字段
2. 新增 source/docs contract，守住所有 backend library path 都要做 direct-library scope 校验
3. 在 shared config helper 上补最小 validator，并让五个 backend `CreateContext(AType)` 统一调用
4. 更新 API/reference truth，明确 direct-library path 也 reject 这两个 connection-scoped 字段

## Files

- `docs/plans/2026-05-18-direct-library-connection-scope-clarification.md`
- `src/fafafa.ssl.context.config.pas`
- `src/fafafa.ssl.openssl.backed.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `src/fafafa.ssl.winssl.lib.pas`
- `src/fafafa.ssl.mbedtls.lib.pas`
- `src/fafafa.ssl.wolfssl.lib.pas`
- `docs/reference/API_REFERENCE.md`
- `docs/reference/ARCHITECTURE.md`
- `tests/test_freepascal_library_default_config_connection_scope_clarification.pas`
- `tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification

```bash
bash -n tests/scripts/test_direct_library_connection_scope_clarification_contract.sh
bash tests/scripts/test_direct_library_connection_scope_clarification_contract.sh
mkdir -p tmp/test_freepascal_library_default_config_connection_scope_clarification && \
  fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_freepascal_library_default_config_connection_scope_clarification \
  -FEtmp/test_freepascal_library_default_config_connection_scope_clarification \
  -otmp/test_freepascal_library_default_config_connection_scope_clarification/test_freepascal_library_default_config_connection_scope_clarification \
  tests/test_freepascal_library_default_config_connection_scope_clarification.pas && \
  ./tmp/test_freepascal_library_default_config_connection_scope_clarification/test_freepascal_library_default_config_connection_scope_clarification
mkdir -p tmp/test_factory_connection_scope_clarification && \
  fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_factory_connection_scope_clarification \
  -FEtmp/test_factory_connection_scope_clarification \
  -otmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification \
  tests/test_factory_connection_scope_clarification.pas && \
  ./tmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification
git diff --check
```

## Expected Outcome

- direct-library default-config path 不再把 `HandshakeTimeout` / `BufferSize` 留成假可用入口
- backend library `CreateContext(AType)` 与 factory/context path 在 connection-scope truth 上说同一种话
- 后续继续做 `TSSLConfig` slimming 时，不需要再怀疑 direct-library path 是否偷偷保留这两个字段
