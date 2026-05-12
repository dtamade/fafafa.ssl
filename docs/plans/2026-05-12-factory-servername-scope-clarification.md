# Factory ServerName Scope Clarification

## Goal

修复 `TSSLFactory.CreateContext(...)` 对 `TSSLConfig.ServerName` 的 server-context 漂移，避免 factory/config 路径在没有 warning surface 的情况下静默接受一个已知会被 server-side connections 忽略的 client-only 字段。

## Architecture

- 保持 builder 兼容语义不动：
  - `WithSNI(...)` 在 server path 上继续 valid + warning
- 先写 focused RED 合同，证明：
  - client-context `ServerName` 继续是合法控制组
  - server-context `ServerName` 在 factory default-config path 和 one-shot config path 上目前仍被静默接受
- 最小修法只改 factory scope validation：
  - 对 server-context `ServerName` fail-fast
  - 不改变 client-context `ServerName` 行为

## Current Evidence

- before the fix:
  - builder already warned on server-side deprecated context-level SNI
  - factory had no warning surface and still applied `ServerName` unconditionally
- runtime proof before the fix:
  - client default-config path: accepted and inherited `ServerName`
  - client one-shot path: accepted and inherited `ServerName`
  - server default-config path: incorrectly accepted `ServerName`
  - server one-shot path: incorrectly accepted `ServerName`
- smallest safe change:
  - keep client compatibility
  - reject server factory/config `ServerName`
  - keep builder warning semantics untouched

## Files

- Add: `docs/plans/2026-05-12-factory-servername-scope-clarification.md`
- Add: `tests/test_factory_server_name_scope_clarification.pas`
- Modify: `src/fafafa.ssl.factory.pas`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Steps

1. 更新 working-memory，锁定范围为 factory ServerName scope truth。
2. 写 focused RED 合同：
   - client control: `ServerName` 在 client factory path 继续可用
   - default-config server case: 期望抛 `ESSLConfigurationException`
   - one-shot server case: 期望抛 `ESSLConfigurationException`
3. 最小实现：按 effective context type 收紧 factory scope validation。
4. 跑 focused 回归、相邻回归、diff hygiene、review 后提交。

## Verification

1. `fpc -Fu./src -Fu./tests tests/test_factory_server_name_scope_clarification.pas -otmp/test_factory_server_name_scope_clarification && ./tmp/test_factory_server_name_scope_clarification`
2. `fpc -Fu./src -Fu./tests tests/test_factory_config_server_name_isolation.pas -otmp/test_factory_config_server_name_isolation && ./tmp/test_factory_config_server_name_isolation`
3. `fpc -Fu./src -Fu./tests tests/config/test_config_validation.pas -otmp/test_config_validation && ./tmp/test_config_validation`
4. `git diff --check`
5. `git status --short`

## Risks

- 不要把 builder/server warning 语义误改成 hard failure；这批只收口 factory/config。
- 不要影响 client-context `ServerName` 兼容路径。
- 校验必须按 effective context type 判断，不能盲信 library default config 里旧的 `ContextType` 字段。
