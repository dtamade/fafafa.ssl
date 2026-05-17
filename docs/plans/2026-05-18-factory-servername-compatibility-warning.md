# Factory ServerName Compatibility Warning

## Goal

执行 `context-level ServerName` 迁移路线图中 Phase B 的第二刀：在不改变当前 runtime compatibility 的前提下，收窄 `TSSLConfig.ServerName` / `TSSLFactory.CreateContext(...)` 这条高层写入面，让它从“静默主路径”降格成“显式 warning 的兼容入口”。

## Architecture

- 只动 factory/config de-emphasis：
  - `src/fafafa.ssl.factory.pas`
  - `src/fafafa.ssl.base.pas`
  - `docs/reference/API_REFERENCE.md`
- 不碰 backend constructor fallback：
  - OpenSSL / FreePascal / WinSSL / MbedTLS / WolfSSL 的 context-to-connection 继承行为保持不变
- 用 focused factory tests 锁住 warning + compatibility 共存：
  - 新增 warning test
  - 邻接回归覆盖 scope、isolation、logging-scope、active docs guidance

## Files

- `src/fafafa.ssl.factory.pas`
- `src/fafafa.ssl.base.pas`
- `docs/reference/API_REFERENCE.md`
- `tests/test_factory_server_name_compatibility_warning.pas`
- `tests/test_factory_server_name_scope_clarification.pas`
- `tests/test_factory_config_server_name_isolation.pas`
- `tests/test_factory_logging_scope_clarification.pas`
- `tests/scripts/test_active_docs_no_context_level_sni_guidance_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 新增 focused RED：
   - default-config client path with `TSSLConfig.ServerName` 必须发 warning
   - one-shot `CreateContext(const AConfig)` client path 也必须发 warning
   - warning 必须点名 `TSSLConfig.ServerName`
   - warning 必须明确 `deprecated context-level SNI compatibility`
   - 没有 `ServerName` 时保持安静
2. 生产修复：
   - `TSSLFactory.CreateContext(AContextType, ALibType)` 与 `TSSLFactory.CreateContext(const AConfig)` 在 client-side compatibility write 前发 `TSecurityLog.Warning('Factory', ...)`
   - `TSSLConfig.ServerName` 注释与 API reference 一起降格成 compatibility-only
3. focused regressions：
   - `tests/test_factory_server_name_compatibility_warning.pas`
   - `tests/test_factory_server_name_scope_clarification.pas`
   - `tests/test_factory_config_server_name_isolation.pas`
   - `tests/test_factory_logging_scope_clarification.pas`
   - `tests/scripts/test_active_docs_no_context_level_sni_guidance_contract.sh`
4. 收口：
   - 更新 `task_plan.md` / `findings.md` / `progress.md`
   - 更新迁移路线图与接口/后端验证报告

## Expected Outputs

- factory/client `TSSLConfig.ServerName` 路径不再静默
- current compatibility behavior 保持不变
- active docs 与 public comments 明确把它定义为 compatibility-only
- 下一次继续时，可以直接进入 Phase C shared compatibility shim extraction
