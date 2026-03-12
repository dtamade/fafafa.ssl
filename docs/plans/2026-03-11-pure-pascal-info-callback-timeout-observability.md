# 2026-03-11 pure Pascal info callback timeout observability

## Goal
- 让 pure Pascal `SetInfoCallback(...)` 在握手超时场景下发出明确的 `timeout` state。
- 避免 timeout 被继续折叠成泛化的 `handshake_failed`。

## Root Cause
- 之前 info callback 只区分：
  - `handshake_start`
  - `handshake_done`
  - `verify_failed`
  - `handshake_failed`
- 但 timeout 已经在错误模型里有独立 `sslErrTimeout`，继续把它落成 `handshake_failed` 会丢失恢复动作信息。

## Files
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/scripts/test_freepascal_info_callback_timeout_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
