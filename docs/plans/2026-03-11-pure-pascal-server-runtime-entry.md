# 2026-03-11 pure Pascal server runtime entry

## Goal
- 把 pure Pascal 服务端从“skeleton / embedded roundtrip 证据”推进到更贴近框架入口的 runtime 合同。
- 这波先不扩协议面，而是固定一条最小可运行路径：
  - `TSSLContextBuilder.BuildServer`
  - `TSSLConnectionBuilder.BuildServer`
  - 真实 socket accept
  - `OpenSSL s_client` 互操作
  - app-data read/write
  - ALPN 结果可见

## Architecture
- 新增脚本合同：
  - `tests/scripts/test_freepascal_tls13_server_connection_builder_runtime_contract.sh`
- 服务器端 Pascal probe：
  - `TSSLContextBuilder.Create.WithBackend(sslFreePascal).WithTLS13...BuildServer`
  - `TSSLConnectionBuilder.Create.WithContext(...).WithSocket(...).BuildServer`
  - `Conn.Read` 读客户端 HTTP 请求
  - `Conn.Write` 回 `HTTP/1.0 200 OK`
- 客户端：
  - `openssl s_client -tls1_3 -alpn h2`

## Files
- `tests/scripts/test_freepascal_tls13_server_connection_builder_runtime_contract.sh`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification
- `bash tests/scripts/test_freepascal_tls13_server_connection_builder_runtime_contract.sh`
