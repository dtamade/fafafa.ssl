# 2026-03-11 pure Pascal nonblocking write WantWrite

## Goal
- 收口 pure Pascal / FreePascal backend 在 nonblocking 写压下的 `WantWrite` 语义。
- 让业务侧在 `Write(...)` 被 backpressure 卡住时，拿到真实的 `sslErrWantWrite`，并且重试同一条写操作能够继续完成。

## Root Cause
- `SendData(...)` 已经能把 socket `EWOULDBLOCK` / `WSAEWOULDBLOCK` 映射成 `sslErrWantWrite`。
- 但 `SendApplicationDataFragment(...)` / `SendTLS13AlertRecord(...)` 在 `SendAll(...)` 失败后，会再覆盖成泛化的 `sslErrIO` + `Failed to send ...`。
- 这会让 nonblocking 写压下的调用方看不到 `WantWrite`，也给后续同一条写操作的 continuation 留出状态丢失窗口。

## Files
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/scripts/test_freepascal_nonblocking_write_wantwrite_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## TDD Plan
1. 先加 `tests/scripts/test_freepascal_nonblocking_write_wantwrite_contract.sh`
   - 本地 OpenSSL server + slow proxy 制造 client-side backpressure
   - 首次阻塞写必须给出 `WantWrite=True`
   - 重试同一条写必须最终 `RESUME_OK`
2. 观察 RED
   - 当前实现会把 blocked write 折叠成 generic IO，`WantWrite=False`
3. 最小 GREEN
   - 保留 retryable `WantWrite` 状态，不再在上层发送路径覆盖成 generic IO
   - 同时为待发送 TLS record 增加持久化 pending-send 状态，保证 retry 可继续完成
4. 跑 focused contract + 读侧/close_notify/resumption/compile 回归

## Verification
- `bash tests/scripts/test_freepascal_nonblocking_write_wantwrite_contract.sh`
- `bash tests/scripts/test_freepascal_nonblocking_partial_record_contract.sh`
- `bash tests/scripts/test_freepascal_stream_semantics_contract.sh`
- `bash tests/scripts/test_freepascal_shutdown_close_notify_contract.sh`
- `python3 -u scripts/compile_all_modules.py`
