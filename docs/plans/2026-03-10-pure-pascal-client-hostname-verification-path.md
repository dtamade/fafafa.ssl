# 2026-03-10 pure Pascal client hostname verification path

## Goal
- 把 pure Pascal / FreePascal 客户端的 `hostname verification` 真正接到客户端握手成功路径上。
- 让 `Connect` 在 `sslVerifyPeer` 开启时，不再只完成 TLS 1.3 握手，还会对 peer certificate 做 host 匹配判定。

## Architecture
- 先写 scripted client-handshake RED：
  - 生成带 SAN 的 self-signed server certificate
  - 用该证书建立 trusted store
  - 断言 `server_name=alt.example.com` 时 `Connect` 成功
  - 断言 `server_name=wrong.example.com` 时 `Connect` 失败且错误语义落在 verification
- 然后最小实现 pure Pascal client post-handshake validation：
  - 只在 `sslVerifyPeer` 开启时触发
  - 读取 context trust store
  - 校验证书信任关系
  - 在 client 路径执行 hostname verification
- 最后回归现有 pure Pascal ALPN / peer-certificate / backend basic / compile-all。

## Files
- `src/fafafa.ssl.freepascal.context.material.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_client_hostname_verification_path.pas`
- `tests/test_freepascal_client_peer_certificate_foundation.pas`
- `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## RED
- `fpc -Fu./src tests/test_freepascal_client_hostname_verification_path.pas -otmp/test_fp_client_hostname && ./tmp/test_fp_client_hostname`
- Expected:
  - positive trusted-host path passes
  - mismatch path currently still connects, so the new negative assertion fails

## GREEN
- 让 pure Pascal client 在握手完成但 `Connect` 返回前执行最小验证：
  - peer certificate required
  - trust store verification
  - hostname verification unless `sslCertVerifyIgnoreHostname`
- 同步让 peer-certificate foundation 测试显式提供 trust material，避免被新的默认验证语义击穿。

## Verification
- `fpc -Fu./src tests/test_freepascal_client_hostname_verification_path.pas -otmp/test_fp_client_hostname && ./tmp/test_fp_client_hostname`
- `fpc -Fu./src tests/test_freepascal_client_peer_certificate_foundation.pas -otmp/test_fp_client_peer_cert && ./tmp/test_fp_client_peer_cert`
- `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept`
- `python3 -u scripts/compile_all_modules.py`
