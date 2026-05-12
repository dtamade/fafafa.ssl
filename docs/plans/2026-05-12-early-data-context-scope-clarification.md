# Early-Data Context Scope Clarification

## Goal
修复 builder / factory / public helper 在 early-data mixed-scope 上的错误下发：组合配置可以同时携带 client/server early-data 默认值，但创建具体 client/server context 时，只应把对应一侧的子集应用到该 context。

## Scope
- focused RED 覆盖：
  - builder `BuildClient` / `BuildServer`
  - factory default-config path
  - factory one-shot config path
  - `TSSLHelper.ConfigureClientEarlyData(...)`
  - `TSSLHelper.ConfigureServerEarlyData(...)`
- 相邻修正：
  - replay-store 隔离测试里的固定 session label 改成每次运行唯一，避免默认持久化 ledger 残留污染回归判断

## Architecture
- `TSSLContextBuilder` 与 `TSSLConfig` 继续允许同时承载：
  - `ClientEarlyDataEnabled`
  - `ServerEarlyDataPolicy`
  - `ServerMaxEarlyDataSize`
- 但具体 context build/create 时改为 scope-aware application：
  - `sslCtxClient` 只应用 `ClientEarlyDataEnabled`
  - `sslCtxServer` 只应用 `ServerEarlyDataPolicy` / `ServerMaxEarlyDataSize`
  - `sslCtxBoth` 应用两侧
- `TSSLHelper.ConfigureClientEarlyData(...)` / `ConfigureServerEarlyData(...)` 也改成 context-type aware：
  - wrong-scope context 返回 `False`
  - 不再跨 scope 修改 context 内部 early-data 状态

## Verification
1. `fpc -Fu./src -Fu./tests tests/test_early_data_context_scope_clarification.pas -otmp/test_early_data_context_scope_clarification && ./tmp/test_early_data_context_scope_clarification`
2. `fpc -Fu./src -Fu./tests tests/test_factory_config_early_data_isolation.pas -otmp/test_factory_config_early_data_isolation && ./tmp/test_factory_config_early_data_isolation`
3. `fpc -Fu./src -Fu./tests tests/config/test_context_builder_early_data_contract.pas -otmp/test_context_builder_early_data_contract && ./tmp/test_context_builder_early_data_contract`
4. `fpc -Fu./src -Fu./tests tests/test_early_data_public_api_contract.pas -otmp/test_early_data_public_api_contract && ./tmp/test_early_data_public_api_contract`
5. `fpc -Fu./src -Fu./tests tests/config/test_context_builder_try.pas -otmp/test_context_builder_try && ./tmp/test_context_builder_try`
6. `git diff --check`

## Expected Outcome
- client context 不再接收 server policy/max
- server context 不再接收 client early-data flag
- shared default config 仍可同时表达 client/server 默认值，但 build/create 时只下发对应子集
- public helper 不再跨 scope 修改 context
