# 2026-03-10 WinSSL example-style tests per-connection SNI

## Goal
- 把 `tests/examples/test_winssl*.pas` 加上 `test_performance.pas` / `test_certchain.pas` 这组高可见 WinSSL 连接示例测试从 deprecated context-level `SetServerName(...)` 迁到 per-connection SNI。
- 让示例测试层也与现有 `ServerName` migration policy 保持一致。

## Scope
- `tests/examples/test_winssl_simple.pas`
- `tests/examples/test_winssl_debug.pas`
- `tests/examples/test_winssl.pas`
- `tests/examples/test_performance.pas`
- `tests/examples/test_certchain.pas`
- `tests/scripts/test_winssl_example_tests_per_connection_sni_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
- [x] 盘点三份 WinSSL 示例测试里的旧用法
- [x] 新增 focused shell contract
- [x] 最小迁移到 per-connection SNI
- [x] 跑 focused 回归
- [x] 回写 working memory 与月度汇总

## Verification
- `bash -n tests/scripts/test_winssl_example_tests_per_connection_sni_contract.sh && bash tests/scripts/test_winssl_example_tests_per_connection_sni_contract.sh` => PASS

## Result
- 五份 WinSSL 示例测试现在都会在 `CreateConnection(...)` 之后，通过 `ISSLClientConnection.SetServerName(...)` 设置 hostname。
- 由于这些文件是 Windows-only 测试入口，本波在 Linux 环境下只跑了 focused shell contract，没有做编译验证。

## Next Queue
- 继续判断 `tests/examples/test_basic.pas` / `tests/examples/test_lib_core_functionality.pas` 这类高可见文件应保留为兼容 API 覆盖，还是也要迁到推荐路径。
- 或切回 linked-evidence/script 链继续做边界治理。
