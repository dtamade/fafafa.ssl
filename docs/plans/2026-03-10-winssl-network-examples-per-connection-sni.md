# 2026-03-10 WinSSL network examples per-connection SNI

## Goal
- 把高可见 WinSSL 联网示例从 deprecated context-level `SetServerName(...)` 迁到推荐的 per-connection SNI 路径。
- 让示例层与已落地的 `ISSLContext.ServerName` 迁移策略保持一致，不再继续示范旧用法。

## Scope
- `examples/winssl_https_downloader.pas`
- `examples/winssl_rest_client.pas`
- `examples/winssl_health_checker.pas`
- `tests/scripts/test_winssl_network_examples_per_connection_sni_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 盘点高可见 WinSSL 联网示例中的旧用法
- [x] 新增 focused shell contract
- [x] 最小迁移到 per-connection SNI
- [x] 跑 contract + Linux smoke compile
- [x] 回写 working memory 与月度汇总

## Verification
- `bash -n tests/scripts/test_winssl_network_examples_per_connection_sni_contract.sh && bash tests/scripts/test_winssl_network_examples_per_connection_sni_contract.sh` => PASS
- `fpc examples/winssl_https_downloader.pas -otmp/winssl_https_downloader_smoke` => PASS
- `fpc examples/winssl_rest_client.pas -otmp/winssl_rest_client_smoke` => PASS
- `fpc examples/winssl_health_checker.pas -otmp/winssl_health_checker_smoke` => PASS

## Result
- 这三份 WinSSL 联网示例现在都在 `CreateConnection(...)` 之后，通过 `ISSLClientConnection.SetServerName(...)` 设置 hostname。
- focused contract 同时锁住两件事：
  - 不再出现 `LCtx.SetServerName(...)` 这类 context-level 旧用法
  - 必须显式存在 per-connection `SetServerName(...)` 路径

## Next Queue
- 继续扫高可见示例/测试入口里还在示范 deprecated context-level `SetServerName(...)` 的点，例如 `examples/example_factory_usage.pas` 与 `tests/examples/test_basic.pas`，判断它们应保留为兼容示例还是迁到新路径。
- 或切回 linked-evidence/script 链继续做边界治理。
