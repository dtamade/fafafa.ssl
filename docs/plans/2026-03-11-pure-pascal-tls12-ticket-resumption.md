# 2026-03-11 pure Pascal TLS1.2 ticket resumption

## Goal
- 在已有 TLS1.2 `session-id` resumption 之上，再补齐 `ticket-based resumption`。
- 这波优先证明：
  - local OpenSSL `-no_cache` oracle 可 resumed
  - 高层 `TSSLConnector` 入口也可 resumed
  - 外网矩阵因此进一步收口

## Root Cause
- local `-no_cache` TLS1.2 server 会暴露更真实的 ticket-only 路径：
  - 第一次握手后，pure Pascal 之前并不会把 `NewSessionTicket` 变成 resumable session
  - 第二次握手即使进入 reused path，`GetSession` 也不会继续保持 resumable
- 这条线的关键缺口有四层：
  - `tls12.wire` 缺 `NewSessionTicket` / extension 35 常量
  - `tls12.serverhello.parser` 不会 parse `NewSessionTicket`
  - `tls12.clienthello` 不会发 session ticket extension
  - session snapshot / connection state 不保存 TLS1.2 ticket bytes

## Architecture
- `src/fafafa.ssl.tls12.wire.pas`
  - 新增 `TLS_HANDSHAKE_TYPE_NEW_SESSION_TICKET`
  - 新增 `TLS_EXTENSION_SESSION_TICKET`
- `src/fafafa.ssl.tls12.serverhello.parser.pas`
  - 新增 `TryParseTLS12NewSessionTicketFromHandshake(...)`
- `src/fafafa.ssl.tls12.clienthello.pas`
  - 新增带 ticket 的 ClientHello builder overload
- `src/fafafa.ssl.freepascal.session.pas`
  - TLS1.2 session snapshot 新增：
    - `TLS12SessionTicket`
    - `TLS12SessionTicketLifetimeHint`
- `src/fafafa.ssl.freepascal.connection.pas`
  - full handshake 后可消费 TLS1.2 `NewSessionTicket`
  - resumed path 若 server 不发新 ticket，则沿用 configured ticket 保持 session 继续 resumable
  - application phase 也允许接收 plaintext TLS1.2 `NewSessionTicket`

## Files
- `src/fafafa.ssl.tls12.wire.pas`
- `src/fafafa.ssl.tls12.serverhello.parser.pas`
- `src/fafafa.ssl.tls12.clienthello.pas`
- `src/fafafa.ssl.freepascal.session.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/scripts/test_freepascal_tls12_ticket_resumption_openssl_contract.sh`
- `tests/scripts/test_freepascal_tls12_builder_connector_ticket_resumption_contract.sh`
- `src/fafafa.ssl.freepascal.lib.pas`
- `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification
- `bash tests/scripts/test_freepascal_tls12_ticket_resumption_openssl_contract.sh`
- `bash tests/scripts/test_freepascal_tls12_builder_connector_ticket_resumption_contract.sh`
- `bash tests/scripts/test_freepascal_tls12_resumption_openssl_interop_contract.sh`
- `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- `python3 -u scripts/compile_all_modules.py`
