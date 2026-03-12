# 2026-03-11 pure Pascal nonblocking partial-record buffering

## Goal
- 收口 pure Pascal / FreePascal backend 在 nonblocking 模式下的 partial TLS record continuation 语义。
- 让 `Conn.Read` 在第一次因为 transport 层只到达部分 record 而返回 `sslErrWantRead` 后，后续继续读能消费此前已到达的字节并最终返回明文，而不是把前半段丢掉。

## Root Cause
- 现有 `RecvExact(...)` / `RecvTLSRecord(...)` 只使用局部缓冲。
- 当 nonblocking `recv` 先读到一部分 header/payload，然后下一次返回 `would block` 时，已读字节会随局部变量一起丢失。
- 后续继续读时，socket 上只剩 record 的后半段；连接层再从“中间字节”重新按 5-byte TLS header 起始解析，造成永久 `WantRead` / 错位 / 潜在协议损坏。

## Files
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/scripts/test_freepascal_nonblocking_partial_record_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## TDD Plan
1. 先加 `tests/scripts/test_freepascal_nonblocking_partial_record_contract.sh`
   - 通过本地 TLS proxy 把 server->client transport 数据稳定拆成两段
   - 第一次 nonblocking `Read` 应返回 `sslErrWantRead`
   - 后续继续 `Read` 应最终得到 `HTTP/1.0...` 明文响应
2. 观察 RED
   - 当前实现会丢失 partial record 前缀，导致 payload 永远读不回来
3. 最小 GREEN
   - 为连接增加持久化 transport read buffer
   - `RecvTLSRecord(...)` 只在缓冲中已经拼够完整 record 时才消费字节
4. 跑 focused contract + stream/shutdown/regression + compile gate

## Verification
- `bash tests/scripts/test_freepascal_nonblocking_partial_record_contract.sh`
- `bash tests/scripts/test_freepascal_stream_semantics_contract.sh`
- `bash tests/scripts/test_freepascal_shutdown_close_notify_contract.sh`
- `python3 -u scripts/compile_all_modules.py`
