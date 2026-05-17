# OpenSSL Library Default-Config ServerName Alignment

## Goal

把 `ISSLLibrary.SetDefaultConfig + TOpenSSLLibrary.CreateContext(...)` 这条 backend-specific 直接入口，和当前 generic factory 的 `context-level ServerName` 真相对齐：

- client path: `warning + ignore`
- server path: reject

避免 OpenSSL 独自继续把 deprecated `TSSLConfig.ServerName` 写回新建 context。

## Architecture

- 保留：
  - direct `ISSLContext.SetServerName/GetServerName` compatibility surface
  - OpenSSL library default-config 的其余正常 context defaults
- 收紧：
  - `TOpenSSLLibrary.CreateContext(sslCtxClient)` 不再把 `FDefaultConfig.ServerName` 写回 built context
  - `TOpenSSLLibrary.CreateContext(sslCtxServer)` 在 default-config 带 `ServerName` 时直接抛 `ESSLConfigurationException`
  - direct library path 若配置了 log callback，应发出明确 compatibility warning

## Files

- Add: `docs/plans/2026-05-18-openssl-library-default-config-servername-alignment.md`
- Add: `tests/test_openssl_library_default_config_server_name_clarification.pas`
- Update: `src/fafafa.ssl.openssl.backed.pas`
- Update: `docs/reference/API_REFERENCE.md`
- Update: `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
- Update: `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps

1. Add a focused OpenSSL library-level regression that proves the current drift:
   - direct library default-config client `ServerName` still lands on the built context
   - direct library server `ServerName` is not rejected yet
2. Observe RED on:
   - `tests/test_openssl_library_default_config_server_name_clarification.pas`
3. Update production/runtime truth in `src/fafafa.ssl.openssl.backed.pas`
   - client default-config `ServerName` becomes warning + ignore
   - server default-config `ServerName` becomes configuration error
4. Re-run focused verification and sync docs/plans.

## Expected Outputs

- OpenSSL backend-specific direct library entry no longer contradicts the generic factory path
- `TOpenSSLLibrary.CreateContext(...)` stops preserving deprecated context-level `ServerName` on new client contexts
- server default-config misuse is rejected consistently
- public-surface route summary can move from “high-level write surfaces fixed except direct OpenSSL library path” to “next focus is final compatibility API cleanup”
