# 2026-03-10 pure Pascal client ALPN negotiation foundation

## Goal
- 为纯 Pascal / FreePascal TLS 1.3 路径补齐 ALPN 的基础协商骨架。
- 让 clienthello ALPN offer、server-side selection、EncryptedExtensions ALPN parse、以及连接状态可观测面形成最小闭环。

## Root Cause
- 纯 Pascal 路径已经能在 `ClientHello` 中携带 ALPN offer，但：
  - `ClientHello parser` 不读取 ALPN
  - `EncryptedExtensions` 没有独立 parser
  - `DoAccept` 不做 ALPN 选择
  - `ProcessEncryptedServerFlight` 不记录 negotiated ALPN
- 这让 `ALPN` 在 pure Pascal client M1 checklist 里只能算“缺失”，而不是“部分满足”。

## Files
- `src/fafafa.ssl.tls13.clienthello.parser.pas`
- `src/fafafa.ssl.tls13.encryptedextensions.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_tls13_clienthello_parser.pas`
- `tests/test_tls13_encrypted_extensions_parser.pas`
- `tests/test_freepascal_server_accept_skeleton.pas`
- `tests/test_freepascal_backend_basic.pas`
- `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification
- `fpc -Fu./src -otmp/test_tls13_clienthello_parser tests/test_tls13_clienthello_parser.pas && ./tmp/test_tls13_clienthello_parser` => PASS
- `fpc -Fu./src -otmp/test_tls13_encrypted_extensions_parser tests/test_tls13_encrypted_extensions_parser.pas && ./tmp/test_tls13_encrypted_extensions_parser` => PASS
- `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept` => PASS
- `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic` => PASS
- `python3 -u scripts/compile_all_modules.py` => PASS (`232/232`)

## Result
- 纯 Pascal 路径现在具备了 ALPN 的最小协商骨架，不再只是“字段存在但无协商证据”。
- 这还不足以把 ALPN 提升到“已满足”，但已经足以把 M1 checklist 的 ALPN 从“缺失”提升到“部分满足”。
