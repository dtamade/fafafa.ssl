# Direct-Library ServerName Backend Proof

## Goal

补齐 `TSSLConfig.ServerName` 在 direct-library `CreateContext(...)` 路径上的 backend proof，避免这条 compatibility-only 语义只在部分 backend 上有 runtime evidence。

这批不改 public API，也不重开 context-level SNI 设计讨论，只做两件事：

- 为 `MbedTLS/WolfSSL` 增加 direct-library default-config `ServerName` focused runtime proof
- 为 `OpenSSL/FreePascal/MbedTLS/WolfSSL/WinSSL` 增加一条 focused shell contract，固定源码中的 warning/reject truth 与测试覆盖面

## Why This Batch

当前源码已经把 direct-library `TSSLConfig.ServerName` 收成一致语义：

- client default-config: warning + ignore
- server default-config: reject

但 runtime 级 focused proof 目前主要集中在：

- `tests/test_openssl_library_default_config_server_name_clarification.pas`
- `tests/test_freepascal_library_default_config_server_name_clarification.pas`

这意味着：

- `MbedTLS/WolfSSL` 的 direct-library 语义更像“源码看起来一致”
- 还不是“backend implementation truth 有对称 evidence”
- `WinSSL` 在当前 Linux 环境下仍以静态 contract 守住源码 truth

## Scope

### Add

- `docs/plans/2026-05-21-direct-library-server-name-backend-proof.md`
- `tests/test_mbedtls_wolfssl_library_default_config_server_name_clarification.pas`
- `tests/scripts/test_direct_library_server_name_backend_contract.sh`

### Update

- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification

1. `bash -n tests/scripts/test_direct_library_server_name_backend_contract.sh`
2. `bash tests/scripts/test_direct_library_server_name_backend_contract.sh`
3. `mkdir -p tmp/test_mbedtls_wolfssl_library_default_config_server_name_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_mbedtls_wolfssl_library_default_config_server_name_clarification -FEtmp/test_mbedtls_wolfssl_library_default_config_server_name_clarification -otmp/test_mbedtls_wolfssl_library_default_config_server_name_clarification/test_mbedtls_wolfssl_library_default_config_server_name_clarification tests/test_mbedtls_wolfssl_library_default_config_server_name_clarification.pas && ./tmp/test_mbedtls_wolfssl_library_default_config_server_name_clarification/test_mbedtls_wolfssl_library_default_config_server_name_clarification`
4. `git diff --check`

## Expected Outputs

- direct-library `ServerName` compatibility truth 在所有源码 backend 上都有 focused 静态证据
- `MbedTLS/WolfSSL` 至少补齐 default-config `ServerName` 的 runtime warning/reject proof
- 后续如果再审 `TSSLConfig` mixed-scope / backend parity，不必重新考古这条 compatibility 路径
