# 2026-03-10 pure Pascal client custom CA sources path

## Goal
- 让 pure Pascal / FreePascal 客户端在真实握手路径上真正消费 `LoadCAFile(...)` 与 `LoadCAPath(...)`。
- 把 custom CA / CA bundle 从“context 配置面存在”推进到“client `Connect` 路径确实生效”。

## Architecture
- 先写 scripted client-handshake RED：
  - 生成 self-signed server certificate
  - `LoadCAFile(server_cert.pem)` 时客户端握手应成功
  - `LoadCAPath(dir_with_server_cert.pem)` 时客户端握手也应成功
- 然后最小扩展 pure Pascal context trust-source access：
  - 在现有 trust-store accessor 上补 `CAFile` / `CAPath` getter
  - client post-handshake validation 构造 effective trust store：`context store + CAFile + CAPath`
- 最后回归 hostname / peer-cert / backend basic / compile-all。

## Files
- `src/fafafa.ssl.freepascal.context.material.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_client_custom_ca_sources_path.pas`
- `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## RED
- `fpc -Fu./src tests/test_freepascal_client_custom_ca_sources_path.pas -otmp/test_fp_client_custom_ca && ./tmp/test_fp_client_custom_ca`
- Expected:
  - `LoadCAFile(...)` / `LoadCAPath(...)` 路径当前都会因为 trust source 没进入握手验证而失败

## GREEN
- 复用当前 hostname verification 的 post-handshake validation 入口，不新增第二条验证主线。
- 只补 trust source 汇总，不改现有 hostname verification contract。

## Verification
- `fpc -Fu./src tests/test_freepascal_client_custom_ca_sources_path.pas -otmp/test_fp_client_custom_ca && ./tmp/test_fp_client_custom_ca`
- `fpc -Fu./src tests/test_freepascal_client_hostname_verification_path.pas -otmp/test_fp_client_hostname && ./tmp/test_fp_client_hostname`
- `fpc -Fu./src tests/test_freepascal_client_peer_certificate_foundation.pas -otmp/test_fp_client_peer_cert && ./tmp/test_fp_client_peer_cert`
- `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept`
- `python3 -u scripts/compile_all_modules.py`
