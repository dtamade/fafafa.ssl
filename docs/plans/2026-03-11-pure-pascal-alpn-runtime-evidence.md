# 2026-03-11 pure Pascal ALPN runtime evidence

## Goal
- 为 pure Pascal / FreePascal 客户端补齐更接近业务主入口的 `ALPN` 真实证据面。
- 这波不改抽象，而是把已有 ALPN 协商骨架推进到：
  - 本地 OpenSSL 真实互操作
  - `TSSLContextBuilder` / `TSSLConnector` / `TSSLStream` 主入口证据
  - network-gated 的外网 runtime harness

## Architecture
- 本地合同分两层：
  - `tests/scripts/test_freepascal_tls13_builder_connector_stream_alpn_contract.sh`
    - 验证 `WithHTTP2` 会真的走到 TLS1.3 本地 OpenSSL 协商，并把 negotiated ALPN 投影到
      `GetSelectedALPNProtocol` / `GetConnectionInfo.ALPNProtocol`
  - `tests/scripts/test_freepascal_tls12_local_alpn_openssl_contract.sh`
    - 验证 pure Pascal TLS1.2 client path 能从真实 OpenSSL `ServerHello` 中解析 ALPN
- 外网 harness：
  - `tests/integration/test_freepascal_alpn_runtime.pas`
  - 只在 `FAFAFA_RUN_NETWORK_TESTS=1` 下运行
  - host 只从环境变量读取：
    - `FAFAFA_ALPN_RUNTIME_HOSTS`
    - `FAFAFA_ALPN_RUNTIME_HOST`
  - offered protocol 可用 `FAFAFA_ALPN_RUNTIME_PROTOCOLS` 覆盖，默认 `h2,http/1.1`
  - 若 host 未配置，则显式 `Skip`

## Files
- `tests/scripts/test_freepascal_tls13_builder_connector_stream_alpn_contract.sh`
- `tests/scripts/test_freepascal_tls12_local_alpn_openssl_contract.sh`
- `tests/integration/test_freepascal_alpn_runtime.pas`
- `tests/scripts/test_freepascal_alpn_runtime_contract.sh`
- `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification
- `bash tests/scripts/test_freepascal_tls13_builder_connector_stream_alpn_contract.sh`
- `bash tests/scripts/test_freepascal_tls12_local_alpn_openssl_contract.sh`
- `bash tests/scripts/test_freepascal_alpn_runtime_contract.sh`
- `fpc -Fu./src tests/integration/test_freepascal_alpn_runtime.pas -otmp/test_fp_alpn_runtime`
- `FAFAFA_RUN_NETWORK_TESTS=1 FAFAFA_ALPN_RUNTIME_HOST='example-host' ./tmp/test_fp_alpn_runtime`
