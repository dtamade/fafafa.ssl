# 2026-03-11 pure Pascal blocking read timeout

## Goal
- 收口 pure Pascal 在 blocking socket + `SetTimeout(...)` 下的 read timeout 语义。
- 让 timeout 命中后返回 `sslErrTimeout`，而不是被误分类成 nonblocking `WantRead`。

## Root Cause
- 当前 `RecvData(...)` 会把 `EAGAIN/EWOULDBLOCK` 一律映射到 `sslErrWantRead`。
- 但在 blocking socket 上启用 `SO_RCVTIMEO` 后，这个 errno 很可能代表“读超时”，不是“非阻塞重试”。

## Files
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/scripts/test_freepascal_blocking_read_timeout_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
