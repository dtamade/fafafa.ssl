# 2026-03-11 pure Pascal write and handshake timeout

## Goal
- 收口 pure Pascal 在 blocking write timeout 与 client handshake timeout 上的错误语义。
- 让 timeout 不再被上层路径压扁成 generic IO，并保证 write timeout 后 pending-send 仍能续完。

## Files
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/scripts/test_freepascal_blocking_write_timeout_contract.sh`
- `tests/scripts/test_freepascal_client_handshake_timeout_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
