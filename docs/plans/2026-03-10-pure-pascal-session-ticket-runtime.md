# 2026-03-10 pure Pascal session ticket runtime

## Goal
- 为 pure Pascal / FreePascal 客户端补一条真实外网 `NewSessionTicket` 提取证据。
- 证明 `GetSession()` 不只是 snapshot surface，而是能在真实 TLS 1.3 握手后拿到 resumable session。

## Architecture
- network-gated integration：
  - `FAFAFA_RUN_NETWORK_TESTS=1` 时才真正连外网
  - 建立 `WithSystemRoots` + `WithVerifyPeer` 的 pure Pascal client
  - 发送最小 `HEAD /` 请求
  - 读若干片段，给 server 发送 post-handshake `NewSessionTicket` 的机会
  - 断言 `GetSession <> nil` 且 `IsResumable = True`

## Files
- `tests/integration/test_freepascal_session_ticket_runtime.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification
- `FAFAFA_RUN_NETWORK_TESTS=1 fpc -Fu./src -Fu./examples tests/integration/test_freepascal_session_ticket_runtime.pas -otmp/test_fp_session_ticket_runtime && FAFAFA_RUN_NETWORK_TESTS=1 ./tmp/test_fp_session_ticket_runtime`
- `fpc -Fu./src tests/test_freepascal_tls13_session_resumption_foundation.pas -otmp/test_fp_tls13_resumption && ./tmp/test_fp_tls13_resumption`
- `python3 -u scripts/compile_all_modules.py`
