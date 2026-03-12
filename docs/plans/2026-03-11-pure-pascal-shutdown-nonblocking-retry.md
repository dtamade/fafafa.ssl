# 2026-03-11 pure Pascal shutdown nonblocking retry

## Goal
- 固定 pure Pascal / FreePascal backend 在 nonblocking 模式下 `Shutdown` 的 retry contract。
- 明确当连接上还存在 pending TLS write 时，`Shutdown` 必须让位，不得静默丢掉 `WantWrite` 语义。

## Current Truth
- 经过 write-side pending-send 收口后，`close_notify` 已经与 application data 共享同一套 post-handshake send foundation。
- 因此当上一条 TLS record 还在 pending-send 队列中时：
  - `Shutdown` 不应伪装成功
  - 也不应退化成 generic IO
  - 而应显式要求调用方先续完同一条 pending write

## Files
- `tests/scripts/test_freepascal_shutdown_nonblocking_retry_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Contract
1. 本地 slow proxy 制造 client-side write pressure。
2. 先让一条 application-data write 进入 pending state。
3. 立即调用 `Shutdown`：
   - 预期 `False`
   - `WantWrite=True`
   - detail 指向 “previous TLS write is still pending; retry the same operation”
4. 续完同一条 pending write。
5. 再次 `Shutdown`：
   - 预期最终成功。

## Verification
- `bash tests/scripts/test_freepascal_shutdown_nonblocking_retry_contract.sh`
- `bash tests/scripts/test_freepascal_nonblocking_write_wantwrite_contract.sh`
- `bash tests/scripts/test_freepascal_shutdown_close_notify_contract.sh`
