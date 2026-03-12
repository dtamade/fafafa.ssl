# 2026-03-11 pure Pascal ALPN client no-offer

## Goal
- 补齐 pure Pascal `ALPN` service-class matrix 的另一半：
  - 客户端不发送 ALPN 扩展
  - 服务端支持并提供 `h2`
  - 握手仍成功且公开 API 暴露空协商结果

## Architecture
- 新增本地 client-no-offer 合同：
  - `tests/scripts/test_freepascal_tls13_builder_connector_stream_client_no_alpn_offer_contract.sh`
  - `tests/scripts/test_freepascal_tls12_local_client_no_alpn_offer_contract.sh`
- 新增结构合同：
  - `tests/scripts/test_freepascal_alpn_client_no_offer_contract.sh`

## Files
- `tests/scripts/test_freepascal_alpn_client_no_offer_contract.sh`
- `tests/scripts/test_freepascal_tls13_builder_connector_stream_client_no_alpn_offer_contract.sh`
- `tests/scripts/test_freepascal_tls12_local_client_no_alpn_offer_contract.sh`
- `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification
- `bash tests/scripts/test_freepascal_alpn_client_no_offer_contract.sh`
- `bash tests/scripts/test_freepascal_tls13_builder_connector_stream_client_no_alpn_offer_contract.sh`
- `bash tests/scripts/test_freepascal_tls12_local_client_no_alpn_offer_contract.sh`
