# Backend Optional-Surface Completion Audit Revalidation

## Goal

把已经写进 `tests/contract/test_backend_contract.pas`、但相关 plan 文档仍缺 execution result 的 backend optional public surface 补成当前可复用证据，确认这些接口层 contract 在现状下确实成立，而不是只停留在“计划过”。

## Scope

本批只做 focused completion-audit revalidation，不预设生产代码改动，范围限于这些已经存在的 contract：

- Contract 12: Context optional interface alignment
- Contract 13: Context native-handle interface alignment
- Contract 14: Context HTTP hooks interface alignment
- Contract 15: Session native-handle interface alignment
- Contract 17: Certificate-store native-handle interface alignment
- Contract 18: Diagnostics interface alignment

对应的历史 plan：

- `docs/plans/2026-05-04-backend-context-optional-interface-completion-audit.md`
- `docs/plans/2026-05-04-backend-context-native-handle-completion-audit.md`
- `docs/plans/2026-05-04-backend-http-hooks-interface-completion-audit.md`
- `docs/plans/2026-05-04-backend-session-native-handle-completion-audit.md`
- `docs/plans/2026-05-04-backend-certificate-store-native-handle-completion-audit.md`
- `docs/plans/2026-05-04-backend-diagnostics-interface-completion-audit.md`

## Why This Batch

当前总目标要求：

- 接口设计完整
- 各 backend 实现完整
- 测试和文档完整

上述 6 份 plan 文档虽然都已经有明确目标和约束，但仍缺少 execution result。与此同时，这些 surface 的 contract 实际已经存在于 `tests/contract/test_backend_contract.pas` 中，因此最合适的下一步不是重复开新设计，而是先把“已有 contract 是否真的成立”补成当前证据。

## Deliverables

1. focused 运行 `tests/contract/test_backend_contract.pas`
2. 确认 Contracts 12-18 当前全部通过
3. 为 6 份缺结果的 plan 补上 focused revalidation result
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

- backend optional public surface 的“计划存在但结果缺席”状态被消掉
- 相关文档不再给人“这些接口也许还没真的验证过”的信号
- 总路线图可以更安心地抬回 broader interface debt，而不是继续怀疑这些已写入 contract 的 optional surface

## Focused Revalidation Result (2026-05-22)

- `fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas`
  - PASS
  - compiled `tests/contract/test_backend_contract.pas`
  - compiler summary:
    - `94263 lines compiled`
    - `138 warning(s) issued`
    - `31 note(s) issued`
- `./tmp/backend_contract_units/test_backend_contract`
  - PASS
  - summary:
    - `Total Tests: 135`
    - `Passed: 111`
    - `Failed: 0`
    - `Skipped: 24`
- Contracts 12-18 current truth:
  - OpenSSL / WolfSSL / MbedTLS / FreePascal all passed the optional public-surface checks covered by this plan
  - WinSSL rows followed the current platform-skip path in this local focused contract run
  - no production implementation drift was found

This revalidation wrapper now has its own execution receipt; it should no longer be treated as a receipt-missing plan.
