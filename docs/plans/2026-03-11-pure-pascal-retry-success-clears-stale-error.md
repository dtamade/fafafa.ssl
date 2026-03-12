# 2026-03-11 pure Pascal retry success clears stale error

## Goal
- 清理 pure Pascal 在成功重试后的 stale `FLastErrorCode` / `FLastErrorString` 残留。
- 让调用方在 retry 成功后看到的 detail 回到干净状态，而不是继续挂着上一次的 `WantRead` / `WantWrite` 文案。

## Root Cause
- `SendData(...)` / `RecvData(...)` 在 would-block 时会记录错误。
- 但成功 flush / 成功发送之后，没有把当前错误状态清回 `sslErrNone` / 空字符串。
- 结果是 `GetError(0)` 虽然返回 success，但 `GetVerifyResultString` 仍会泄漏上一条旧错误 detail。

## Files
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/scripts/test_freepascal_retry_success_clears_stale_error_contract.sh`
- `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification
- `bash tests/scripts/test_freepascal_retry_success_clears_stale_error_contract.sh`
- `bash tests/scripts/test_freepascal_nonblocking_write_wantwrite_contract.sh`
- `bash tests/scripts/test_freepascal_renegotiate_nonblocking_retry_contract.sh`
- `python3 -u scripts/compile_all_modules.py`
