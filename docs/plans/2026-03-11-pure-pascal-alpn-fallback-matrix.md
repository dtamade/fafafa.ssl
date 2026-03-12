# 2026-03-11 pure Pascal ALPN fallback matrix

## Goal
- 继续收口 pure Pascal `ALPN` 的真实证据面。
- 这波重点不是再证明 `h2`，而是证明：
  - 当服务端只提供 `http/1.1` 时，客户端 offer `h2,http/1.1` 仍能稳定 fallback
  - 外网 runtime harness 可以做 multi-host matrix，并在需要时断言预期协商协议

## Architecture
- 新增本地 fallback 合同：
  - `tests/scripts/test_freepascal_tls13_builder_connector_stream_alpn_http11_fallback_contract.sh`
  - `tests/scripts/test_freepascal_tls12_local_alpn_http11_fallback_contract.sh`
- 扩展 `tests/integration/test_freepascal_alpn_runtime.pas`：
  - 支持 `FAFAFA_ALPN_RUNTIME_EXPECTED_PROTOCOL`
  - 输出 matrix-level success summary
- 新增结构合同：
  - `tests/scripts/test_freepascal_alpn_runtime_matrix_contract.sh`

## Files
- `tests/integration/test_freepascal_alpn_runtime.pas`
- `tests/scripts/test_freepascal_alpn_runtime_matrix_contract.sh`
- `tests/scripts/test_freepascal_tls13_builder_connector_stream_alpn_http11_fallback_contract.sh`
- `tests/scripts/test_freepascal_tls12_local_alpn_http11_fallback_contract.sh`
- `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification
- `bash tests/scripts/test_freepascal_alpn_runtime_matrix_contract.sh`
- `bash tests/scripts/test_freepascal_tls13_builder_connector_stream_alpn_http11_fallback_contract.sh`
- `bash tests/scripts/test_freepascal_tls12_local_alpn_http11_fallback_contract.sh`
- `fpc -Fu./src tests/integration/test_freepascal_alpn_runtime.pas -otmp/test_fp_alpn_runtime`
- `FAFAFA_RUN_NETWORK_TESTS=1 FAFAFA_ALPN_RUNTIME_HOSTS='host-a,host-b' FAFAFA_ALPN_RUNTIME_PROTOCOLS='http/1.1' FAFAFA_ALPN_RUNTIME_EXPECTED_PROTOCOL='http/1.1' ./tmp/test_fp_alpn_runtime`
