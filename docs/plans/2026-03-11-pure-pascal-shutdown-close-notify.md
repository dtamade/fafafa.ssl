# 2026-03-11 pure Pascal shutdown close notify

## Goal
- 让 pure Pascal / FreePascal backend 的 `Shutdown` 真正发送 TLS 1.3 `close_notify`，不再只是成功返回的空实现。
- 为业务侧 `ISSLConnection.Shutdown` contract 补齐最基本的 wire-level graceful close 语义。

## Root Cause
- `DoShutdown` 之前是直接 `Result := True`，不会发送任何 alert record。
- 虽然 `RecvApplicationDataFragment(...)` 已经能把对端 `close_notify` 当作 graceful EOF，但本端 `Shutdown` 仍不对等。
- 最小正确修复是在已有 application-traffic key/nonce/AEAD 发送路径上新增一个 alert record sender，而不是改 `TSSLStream` 包装层。

## Files
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/scripts/test_freepascal_shutdown_close_notify_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification
- `bash tests/scripts/test_freepascal_shutdown_close_notify_contract.sh`
- `bash tests/scripts/test_freepascal_stream_semantics_contract.sh`
- `python3 -u scripts/compile_all_modules.py`
