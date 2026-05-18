# Backend Connection-Surface Completion Audit Revalidation

## Goal

把仍然缺 execution receipt 的连接层旧计划补成当前可复用证据，确认这些 `ISSLConnection` 相关 optional/public surface 在现状下确实成立，而不是只停留在“当时做过一次”。

## Scope

本批只做 focused completion-audit revalidation，不预设生产代码改动，范围限于这些已经存在的 contract / 历史 plan：

- Contract 8: Client connection SNI interface alignment
- Contract 10: Client connection OCSP interface alignment
- Contract 11: Connection native-handle interface alignment

对应的历史 plan：

- `docs/plans/2026-05-04-backend-client-connection-sni-interface-alignment.md`
- `docs/plans/2026-05-04-backend-connection-native-handle-interface-alignment.md`
- `docs/plans/2026-05-04-backend-ocsp-connection-interface-alignment.md`

## Why This Batch

当前总目标不只是“接口设计看起来合理”，还要求：

- 接口设计完整
- 各 backend 实现完整
- 测试和文档完整

本轮继续往 `ISSLConnection` 主线推进时，重新核对发现：

- `ISSLConnectionInfo` / `ISSLSessionResumption` / `ISSLCertificateVerification` 已经有 execution result
- 但上面这 3 份连接层旧 plan 仍缺当前 execution receipt
- 它们又恰好都落在 `ISSLConnection` connection-surface 本体上

因此这批最合适的动作不是重开实现，而是先把“已经被 `test_backend_contract` 覆盖的连接层真相”补成当前证据。

## Deliverables

1. focused 运行 `tests/contract/test_backend_contract.pas`
2. 确认 Contracts 8 / 10 / 11 当前全部通过
3. 为 3 份缺结果的 plan 补上 `Focused Revalidation Result (2026-05-18)`
4. 同步 `task_plan.md` / `findings.md` / `progress.md`

## Verification

```bash
mkdir -p tmp/backend_contract_units && \
  fpc -B -Fu./src -Fu./tests \
  -FUtmp/backend_contract_units \
  -FEtmp/backend_contract_units \
  -otmp/backend_contract_units/test_backend_contract \
  tests/contract/test_backend_contract.pas && \
  ./tmp/backend_contract_units/test_backend_contract

git diff --check
```

## Expected Outcome

- 连接层 SNI / native-handle / OCSP surface 不再停留在“老计划有内容、当前缺结果”的状态
- `ISSLConnection` 主线上剩余的审查阻力从“receipt 缺口”进一步收成“真实设计债”
- 下一批可以更明确地进入第一条真正的 slimming slice，而不是继续补历史 execution receipt
