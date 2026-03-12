# 2026-03-11 pure Pascal client trust source runtime matrix

## Goal
- 把 pure Pascal 客户端的 trust-source 证据从 scripted 推进到本地真实 socket runtime。
- 覆盖最小高价值矩阵：
  - 无信任源 -> 失败
  - `SetCertificateStore` -> 成功
  - `LoadCAFile` -> 成功
  - `LoadCAPath` -> 成功

## Files
- `tests/test_freepascal_client_trust_source_runtime_matrix.pas`
- `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`
