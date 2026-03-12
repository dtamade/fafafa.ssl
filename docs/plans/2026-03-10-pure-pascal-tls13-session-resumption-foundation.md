# 2026-03-10 pure Pascal TLS13 session resumption foundation

## Goal
- 为 pure Pascal / FreePascal TLS 1.3 路径补最小可运行的 PSK resumption 骨架。
- 让 `SetSession(...)` 不再只是保存 session snapshot，而是真能影响第二次握手。

## Architecture
- 先写 scripted client/server RED：
  - client / server 共享同一份 resumable session material
  - 第二次 client `Connect` 发送 PSK resumption 尝试
  - server 接受该 PSK，省略证书路径，返回 resumption-style server flight
  - client 完成握手并把 `IsSessionReused` 置 `True`
- 最小实现范围：
  - `pre_shared_key` ClientHello extension（单 identity）
  - binder 计算/校验（pure Pascal self-interoperability）
  - PSK-aware handshake key schedule（`psk_dhe_ke`）
  - ServerHello `pre_shared_key` selected-identity
- 明确不在这一波做：
  - 0-RTT
  - 多 identity 选择
  - 对外部 TLS 栈的互操作承诺

## Files
- `src/fafafa.ssl.tls13.wire.pas`
- `src/fafafa.ssl.tls13.clienthello.pas`
- `src/fafafa.ssl.tls13.clienthello.parser.pas`
- `src/fafafa.ssl.tls13.parser.pas`
- `src/fafafa.ssl.tls13.keyschedule.pas`
- `src/fafafa.ssl.tls13.appschedule.pas`
- `src/fafafa.ssl.freepascal.session.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_tls13_session_resumption_foundation.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## RED
- `fpc -Fu./src tests/test_freepascal_tls13_session_resumption_foundation.pas -otmp/test_fp_tls13_resumption && ./tmp/test_fp_tls13_resumption`
- Expected:
  - current pure Pascal still ignores session on the handshake path, so reused assertion fails

## GREEN
- 只做最小 single-identity `psk_dhe_ke` foundation。
- 不把这波扩大成 production-grade external interoperability claim。

## Verification
- `fpc -Fu./src tests/test_freepascal_tls13_session_resumption_foundation.pas -otmp/test_fp_tls13_resumption && ./tmp/test_fp_tls13_resumption`
- `fpc -Fu./src tests/test_freepascal_session_surface_foundation.pas -otmp/test_fp_session_surface && ./tmp/test_fp_session_surface`
- `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- `python3 -u scripts/compile_all_modules.py`
