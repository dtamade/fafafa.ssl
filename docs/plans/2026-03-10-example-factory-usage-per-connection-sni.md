# 2026-03-10 example_factory_usage per-connection SNI

## Goal
- 把 `examples/example_factory_usage.pas` 从 deprecated context-level `SetServerName(...)` 示例迁到推荐的 per-connection SNI 路径。
- 让最直接、最容易被复制的示例入口与 `ServerName` migration policy 保持一致。

## Scope
- `examples/example_factory_usage.pas`
- `tests/scripts/test_example_factory_usage_per_connection_sni_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 盘点 `example_factory_usage` 里的旧示范路径
- [x] 新增 focused shell contract
- [x] 最小迁移到 per-connection SNI 文案
- [x] 跑 contract + compile smoke
- [x] 回写 working memory 与月度汇总

## Verification
- `bash -n tests/scripts/test_example_factory_usage_per_connection_sni_contract.sh && bash tests/scripts/test_example_factory_usage_per_connection_sni_contract.sh` => PASS
- `fpc -Fu./src -Fi./src examples/example_factory_usage.pas -otmp/example_factory_usage_smoke` => PASS

## Result
- `example_factory_usage` 不再继续打印 `Ctx.SetServerName(...)` 这类 context-level 旧用法。
- 快速入门代码片段现在明确展示：先 `CreateConnection(...)`，再通过 `ISSLClientConnection.SetServerName(...)` 设置 hostname。

## Next Queue
- 继续扫 `tests/examples/test_basic.pas` / `tests/examples/test_lib_core_functionality.pas`，判断它们应保留为“兼容 API 覆盖测试”，还是也要迁到推荐路径。
- 或切回 linked-evidence/script 链继续做边界治理。
