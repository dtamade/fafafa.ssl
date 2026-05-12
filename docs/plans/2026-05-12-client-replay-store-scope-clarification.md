# Client Replay-Store Scope Clarification

## Goal
修复 `server_early_data_replay_store_file` / `server_early_data_replay_store_directory` 被 client builder/factory 路径静默接受的问题，让 server-only replay-store opt-in 不再在 client context creation 上变成无提示 no-op。

## Scope
- 写 focused RED，覆盖：
  - `ValidateClient`
  - `TryBuildClient`
  - `TSSLFactory.CreateContext(sslCtxClient, ALibType)`
  - `TSSLFactory.CreateContext(const AConfig)` 的 client path
- 仅在 builder/factory 边界加 fail-fast。
- 保持 server replay-store 安装链不变。

## Architecture
- `WithServerEarlyDataReplayStoreFile(...)` / `WithServerEarlyDataReplayStoreDirectory(...)` 继续是 server-only public opt-in。
- builder:
  - `ValidateClient` 对这两个字段报错
  - `BuildClient` / `TryBuildClient` 直接拒绝
- factory:
  - client context path 直接拒绝这两个字段
  - server-capable context path 继续允许
- 调试输出补充 server-scoped 文案，减少误用。

## Verification
1. `fpc -Fu./src -Fu./tests tests/test_early_data_replay_store_client_scope_clarification.pas -otmp/test_early_data_replay_store_client_scope_clarification && ./tmp/test_early_data_replay_store_client_scope_clarification`
2. `fpc -Fu./src -Fu./tests tests/test_factory_config_early_data_isolation.pas -otmp/test_factory_config_early_data_isolation && ./tmp/test_factory_config_early_data_isolation`
3. `fpc -Fu./src -Fu./tests tests/config/test_context_builder_try.pas -otmp/test_context_builder_try && ./tmp/test_context_builder_try`
4. `fpc -Fu./src -Fu./tests tests/config/test_config_validation.pas -otmp/test_config_validation && ./tmp/test_config_validation`
5. `git diff --check`

## Expected Outcome
- client builder/factory 不再静默吃掉 server replay-store 配置
- server replay-store 既有 contract 继续保持
- old isolation contract 更新成新的 fail-fast truth
