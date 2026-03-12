# 2026-03-10 pure Pascal stream semantics

## Goal
- 收口 pure Pascal / FreePascal 连接在 `close_notify/EOF` 和 `WantRead/WantWrite` 上的运行时语义。
- 让 `TSSLStream.Read` 与 `ISSLConnection.Read` 的客户端流式行为更接近生产可用预期。

## Root Cause
- 纯 Pascal 连接之前有两条直接暴露到业务侧的语义缺口：
  - `close_notify` / 传输 EOF 会被当成错误，`TSSLStream.Read` 最终抛 `TLS read failed`
  - `SetBlocking(False)` 只改了基类布尔字段，没有真正落到 socket；因此 `WantRead/WantWrite` 永远不成立
- 这两个问题叠在一起，导致：
  - 流式读取无法稳定以 `0` 表达“对端优雅关闭”
  - 非阻塞调用方即便使用 `SetBlocking(False)`，也拿不到 `sslErrWantRead/sslErrWantWrite`

## Files
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/scripts/test_freepascal_stream_semantics_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification
- `bash tests/scripts/test_freepascal_stream_semantics_contract.sh`
- `fpc -Fu./src -Fu./examples tests/test_freepascal_local_session_resumption_roundtrip.pas -otmp/test_fp_local_resumption && ./tmp/test_fp_local_resumption`
- `python3 -u scripts/compile_all_modules.py`
