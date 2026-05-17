# Cross-Backend Network Contracts Per-Connection SNI

## Goal

把跨后端网络合同从 deprecated context-level SNI 指导语义迁到 per-connection SNI，避免这些“结果一致性 / 错误归一化”合同继续被误归类为 intentional compatibility coverage。

## Architecture

- 目标文件：
  - `tests/integration/test_cross_backend_consistency_contract.pas`
  - `tests/integration/test_cross_backend_errors_contract.pas`
- 这两份合同的核心是：
  - 跨 backend 的探测结果是否一致
  - 跨 backend 的错误归一化是否一致
- 它们不应再依赖：
  - `Ctx.SetServerName(...)`
  - intentional compatibility label 集合

## Files

- `tests/scripts/test_cross_backend_network_contracts_no_context_level_sni_guidance.sh`
- `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
- `tests/integration/test_cross_backend_consistency_contract.pas`
- `tests/integration/test_cross_backend_errors_contract.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. focused RED contract:
   - fail if the two cross-backend network contracts still use `Ctx.SetServerName(...)`
2. production-equivalent test cleanup:
   - move them to `ISSLClientConnection.SetServerName(...)`
   - remove them from intentional-compat label contract
3. focused verification:
   - `bash tests/scripts/test_cross_backend_network_contracts_no_context_level_sni_guidance.sh`
   - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
   - compile/run both integration contracts
4. closeout:
   - update roadmap and working memory so next batch starts from the remaining `sslCtxClient` inherited fallback surface

## Expected Outputs

- cross-backend network contracts stop teaching deprecated context-level SNI
- intentional-compat label set shrinks again
- next `sslCtxClient` behavior migration can focus on real fallback-bearing production surfaces
