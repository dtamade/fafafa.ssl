# 2026-03-10 pure Pascal client peer certificate foundation

## Goal
- 为纯 Pascal / FreePascal TLS 1.3 客户端路径补齐“握手后可取到对端证书”的最小闭环。
- 让 `GetPeerCertificate` / `GetPeerCertificateChain` 不再停留在空实现，而是有真实 scripted client handshake 证据。

## Root Cause
- 纯 Pascal 客户端在 `ProcessEncryptedServerFlight(...)` 中此前只关心 `EncryptedExtensions` 和 `Finished`，没有把 `Certificate` 握手消息收集到连接状态。
- 这导致：
  - `DoGetPeerCertificate` / `DoGetPeerCertificateChain` 无法返回真实对端证书
  - pure Pascal client M1 checklist 中“对端证书信息”只有 parser / cert-object 级证据，没有真实客户端握手路径证据
- 最小正确修复不是提前做完整 hostname / CA verify，而是先把 peer certificate capture foundation 建起来，作为后续验证链的前置条件。

## Files
- `src/fafafa.ssl.tls13.servercertificate.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_client_peer_certificate_foundation.pas`
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
- `fpc -Fu./src tests/test_freepascal_client_peer_certificate_foundation.pas -otmp/test_fp_client_peer_cert && ./tmp/test_fp_client_peer_cert` => PASS
- `python3 -u scripts/compile_all_modules.py` => PASS (`232/232`)

## Result
- pure Pascal 客户端现在会在真实 TLS 1.3 encrypted server flight 中解析并保留 `Certificate` 握手消息。
- `GetPeerCertificate` / `GetPeerCertificateChain` 现在能从握手捕获的 DER 证书重建 `ISSLCertificate`。
- 这波没有把证书验证问题“假装完成”；它只把后续 `hostname verification` / `system roots` / `custom CA` 所依赖的对端证书输入面真正接上。

## Next Queue
- 继续 pure Pascal client M1：优先把 `hostname verification` 接到真实客户端握手路径。
- 然后补 `系统根证书 + 自定义 CA / CA bundle` 的生产级验证证据。
