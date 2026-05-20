# C-Library Direct-Library Runtime Parity

## Goal

补齐 `OpenSSL` / `MbedTLS` / `WolfSSL` 这三条 C-library backend 在 direct-library default-config 路径上的 focused runtime proof，避免我们继续只靠 `FreePascal` 的证明去代表所有 backend。

这批不改 public API，也不重开 `TSSLConfig` 的字段分层设计，只补 runtime parity evidence：

- `LogLevel` / `LogCallback` 的 library-default ownership truth
- `HandshakeTimeout` / `BufferSize` 的 direct-library connection-scope reject truth

## Why This Batch

当前静态 truth 和部分 focused proof 已经比较稳定：

- factory 路径：
  - `tests/test_factory_logging_scope_clarification.pas`
  - `tests/test_factory_connection_scope_clarification.pas`
- direct-library 静态 contract：
  - `tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
  - `tests/scripts/test_library_default_logcallback_detachment_contract.sh`
- `FreePascal` direct-library runtime：
  - `tests/test_freepascal_library_default_config_connection_scope_clarification.pas`

但还留着一个明显的不对称：

- `OpenSSL` / `MbedTLS` / `WolfSSL`
  的 direct-library logging / connection-scope
  目前更多停留在源码与静态 contract 层
- runtime 级 focused proof
  还没有像 `ServerName` 那条线一样形成 backend parity

## Scope

### Add

- `docs/plans/2026-05-21-clibrary-direct-library-runtime-parity.md`
- `tests/test_clibrary_library_default_logging_scope_clarification.pas`
- `tests/test_clibrary_library_default_config_connection_scope_clarification.pas`
- `tests/scripts/test_clibrary_direct_library_runtime_parity_contract.sh`

### Update

- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification

1. `bash -n tests/scripts/test_clibrary_direct_library_runtime_parity_contract.sh`
2. `bash tests/scripts/test_clibrary_direct_library_runtime_parity_contract.sh`
3. `bash tests/scripts/test_library_default_logcallback_detachment_contract.sh`
4. `bash tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
5. `mkdir -p tmp/test_clibrary_library_default_logging_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_clibrary_library_default_logging_scope_clarification -FEtmp/test_clibrary_library_default_logging_scope_clarification -otmp/test_clibrary_library_default_logging_scope_clarification/test_clibrary_library_default_logging_scope_clarification tests/test_clibrary_library_default_logging_scope_clarification.pas && ./tmp/test_clibrary_library_default_logging_scope_clarification/test_clibrary_library_default_logging_scope_clarification`
6. `mkdir -p tmp/test_clibrary_library_default_config_connection_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_clibrary_library_default_config_connection_scope_clarification -FEtmp/test_clibrary_library_default_config_connection_scope_clarification -otmp/test_clibrary_library_default_config_connection_scope_clarification/test_clibrary_library_default_config_connection_scope_clarification tests/test_clibrary_library_default_config_connection_scope_clarification.pas && ./tmp/test_clibrary_library_default_config_connection_scope_clarification/test_clibrary_library_default_config_connection_scope_clarification`
7. `git diff --check`

## Expected Outputs

- `OpenSSL/MbedTLS/WolfSSL` direct-library logging ownership truth 不再只是静态 contract
- `OpenSSL/MbedTLS/WolfSSL` direct-library connection-scope reject truth 也有 focused runtime evidence
- 后续再审 `TSSLConfig` mixed-scope / backend parity 时，不必继续把 `FreePascal` 当成唯一 runtime 代表
