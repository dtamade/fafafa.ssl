# 2026-03-10 pure Pascal external session resumption interop

## Goal
- 把 pure Pascal TLS 1.3 session resumption 从“本地自洽 + 外网 ticket 提取”推进到“与标准 TLS 栈/真实站点的第二次 resumed handshake 也能通过”。
- 给 pure Pascal backend 的生产可用客户端路径补一条更强的互操作证据。

## Root Cause
- 外部互操作最初卡在两层：
  - wire 层：PSK binder 计算不符合 TLS 1.3 规范，具体体现在：
    - binder transcript 截断位置错到了 binder bytes 起点，而不是 binders list 起点
    - `res binder` 的 `Derive-Secret(..., "")` 把上下文当成空字节，而不是 `Hash("")`
  - 验证层：resumed handshake 不会重新发送证书链，但 pure Pascal session snapshot 之前只保存 leaf certificate，没有保存第一次握手得到的 intermediates，导致 `verify peer + system roots` 场景下第二次 resumed handshake 会在验证阶段失败。

## Result
- 现在已经有三条互相支撑的证据：
  - 本地 OpenSSL `s_server` interop 合同通过，第二次 resumed handshake `reused=True`
  - 真实外网站点 `www.google.com:443` 两次握手探针通过，第二次 `reused=True`
  - network-gated integration `test_freepascal_session_resumption_runtime.pas` 通过
- 这意味着 pure Pascal 的 session resumption 已经从“self-interoperability foundation”推进到“至少对标准 OpenSSL 栈与一个真实外网站点互操作通过”的阶段。

## Files
- `src/fafafa.ssl.tls13.clienthello.parser.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `src/fafafa.ssl.freepascal.session.pas`
- `tests/test_tls13_clienthello_parser.pas`
- `tests/test_freepascal_client_chain_verification_path.pas`
- `tests/integration/test_freepascal_session_resumption_runtime.pas`
- `tests/scripts/test_freepascal_tls13_resumption_openssl_interop_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification
- `fpc -Fu./src tests/test_tls13_clienthello_parser.pas -otmp/test_tls13_clienthello_parser && ./tmp/test_tls13_clienthello_parser`
- `fpc -Fu./src tests/test_freepascal_client_chain_verification_path.pas -otmp/test_fp_client_chain && ./tmp/test_fp_client_chain`
- `bash tests/scripts/test_freepascal_tls13_resumption_openssl_interop_contract.sh`
- `FAFAFA_RUN_NETWORK_TESTS=1 fpc -Fu./src -Fu./examples tests/integration/test_freepascal_session_resumption_runtime.pas -otmp/test_fp_session_resumption_runtime && FAFAFA_RUN_NETWORK_TESTS=1 ./tmp/test_fp_session_resumption_runtime`
- `python3 -u scripts/compile_all_modules.py`
