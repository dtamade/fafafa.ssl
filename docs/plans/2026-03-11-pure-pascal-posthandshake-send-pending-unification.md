# 2026-03-11 pure Pascal post-handshake send pending unification

## Goal
- 把 pure Pascal 剩余的 post-handshake 发送面统一到 pending-send 语义。
- 当前重点是：
  - `SendPostHandshakeKeyUpdate(...)`
  - `SendInitialSessionTicket`

## Root Cause
- application data / `close_notify` 已经进入 pending-send。
- 但 `KeyUpdate` / `NewSessionTicket` 还直接走 `SendAll(...)`。
- 这会让 pure Pascal 在 post-handshake write model 上继续保留两套不一致的 nonblocking 语义。

## Files
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/scripts/test_freepascal_renegotiate_nonblocking_retry_contract.sh`
- `tests/scripts/test_freepascal_posthandshake_send_pending_structure_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## TDD Plan
1. 行为红测：
   - `Renegotiate` 在 pending application write 存在时应显式 `WantWrite`
   - 续完原 write 后，retry `Renegotiate` 应成功
2. 结构红测：
   - `KeyUpdate` / `NewSessionTicket` 必须显式接入 `SendBufferedRecord(...)`
3. 最小 GREEN：
   - 新增 dedicated pending-write kinds
   - `KeyUpdate` / `NewSessionTicket` 切到 pending-send
4. 回归：
   - write / shutdown / stream / compile
