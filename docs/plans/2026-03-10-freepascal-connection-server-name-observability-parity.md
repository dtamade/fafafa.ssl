# 2026-03-10 FreePascal connection ServerName observability parity

## Goal
- 收口 FreePascal backend 在 `GetConnectionInfo.ServerName` 上落后于 OpenSSL / MbedTLS / WolfSSL 的可观测性缺口。
- 让 `Create -> override -> clear` 三段 `ServerName` 状态在 FreePascal 连接信息里也可见。

## Scope
- `src/fafafa.ssl.connection.base.pas`
- `tests/test_freepascal_connection_server_name_observability.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 盘点 FreePascal `ServerName` observability 缺口
- [x] 新增 focused RED contract
- [x] 最小修复默认 `GetConnectionInfo` 逻辑
- [x] 跑 focused + inheritance + compile-all
- [x] 回写 working memory 与月度汇总

## Verification
- `fpc -Fu./src tests/test_freepascal_connection_server_name_observability.pas -otmp/test_freepascal_connection_server_name_observability && ./tmp/test_freepascal_connection_server_name_observability` => PASS
- `fpc -Fu./src -otmp/test_connection_context_server_name_inheritance tests/test_connection_context_server_name_inheritance.pas && ./tmp/test_connection_context_server_name_inheritance` => PASS
- `python3 -u scripts/compile_all_modules.py` => PASS (`231/231`)

## Result
- `TBaseSSLConnection.GetConnectionInfo` 现在会在连接对象支持 `ISSLClientConnection` 时回填 `ServerName`。
- 这让 FreePascal backend 无需再写一份专用 override，就能与现有 connection-level `GetServerName` 保持一致。
- 修复点落在 shared base，而不是 backend-specific 分叉，后续没有覆盖 `GetConnectionInfo` 的 client backend 也能直接受益。

## Next Queue
- 继续复审 `ISSLContext.ServerName` 迁移后的残余 API/clear-path 边界，优先看是否还需要把 high-visibility 示例/测试入口继续从 deprecated context setter 迁到 per-connection path。
- 或切回月度当前队列，继续 linked-evidence/script 链边界治理。
