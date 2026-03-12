# 2026-03-11 pure Pascal ALPN empty negotiation

## Goal
- 为 pure Pascal `ALPN` 再补一块容易被忽略但很关键的 service-class 语义：
  - 当服务端不发送 ALPN 扩展时，握手仍成功
  - 公开 API 必须稳定暴露“空协商结果”，而不是残留旧值或伪造默认协议

## Architecture
- 新增本地 no-ALPN 合同：
  - `tests/scripts/test_freepascal_tls13_builder_connector_stream_no_alpn_contract.sh`
  - `tests/scripts/test_freepascal_tls12_local_no_alpn_contract.sh`
- 扩展 `tests/integration/test_freepascal_alpn_runtime.pas`：
  - 增加 `FAFAFA_ALPN_RUNTIME_ALLOW_EMPTY`
  - 保留严格非空断言路径，只有显式 allow-empty 时才接受空协商
- 新增结构合同：
  - `tests/scripts/test_freepascal_alpn_empty_negotiation_contract.sh`

## Files
- `tests/integration/test_freepascal_alpn_runtime.pas`
- `tests/scripts/test_freepascal_alpn_empty_negotiation_contract.sh`
- `tests/scripts/test_freepascal_tls13_builder_connector_stream_no_alpn_contract.sh`
- `tests/scripts/test_freepascal_tls12_local_no_alpn_contract.sh`
- `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification
- `bash tests/scripts/test_freepascal_alpn_empty_negotiation_contract.sh`
- `bash tests/scripts/test_freepascal_tls13_builder_connector_stream_no_alpn_contract.sh`
- `bash tests/scripts/test_freepascal_tls12_local_no_alpn_contract.sh`
- `fpc -Fu./src tests/integration/test_freepascal_alpn_runtime.pas -otmp/test_fp_alpn_runtime`
- `FAFAFA_RUN_NETWORK_TESTS=1 FAFAFA_ALPN_RUNTIME_HOST='example-host' FAFAFA_ALPN_RUNTIME_ALLOW_EMPTY=1 ./tmp/test_fp_alpn_runtime`
