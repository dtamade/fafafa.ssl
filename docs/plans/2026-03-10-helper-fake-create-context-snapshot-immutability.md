# 2026-03-10 helper/fake CreateContext snapshot immutability

## Goal
- 把 helper fixture 与 shared-config fake backend 在 `CreateContext(...)` 上的默认配置快照不可变语义显式合同化。
- 防止后续重构把 `CreateContext(sslCtxServer)` 的临时 `ContextType` 覆盖回写进 `GetDefaultConfig(...)`。

## Scope
- `tests/test_helper_create_context_default_config_consistency.pas`
- `tests/test_factory_shared_config_and_init_race.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 盘点 helper/fake backend `CreateContext` 快照边界
- [x] 新增 focused contract
- [x] 合同未暴露真缺口，无需生产修复
- [x] 跑 focused 回归
- [x] 回写 working memory 与月度汇总

## Verification
- `fpc -Fu./src tests/test_helper_create_context_default_config_consistency.pas -otmp/test_helper_create_context_default_config_consistency` => PASS
- `./tmp/test_helper_create_context_default_config_consistency` => PASS
- `fpc -Fu./src tests/test_factory_shared_config_and_init_race.pas -otmp/test_factory_shared_config_and_init_race` => PASS
- `./tmp/test_factory_shared_config_and_init_race` => PASS

## Result
- 新合同确认：
  - helper fixture 的 `CreateContext(sslCtxServer)` 会应用默认配置，但不会回写污染 `GetDefaultConfig.ContextType`
  - shared-config fake backend 也保持同样语义
  - `LibraryType` / `SessionTimeout` / `ALPNProtocols` 等默认快照在 server context 创建后仍保持不变
- 这波属于 contract codification，不需要生产代码改动。

## Next Queue
- helper / fake backend 这条线已经足够可信，可以切回主 backend/API 复审。
- 优先建议回到 `ISSLContext.ServerName` 迁移后的残余 API/clear-path 复审，或继续 linked-evidence/script 链的边界治理。
