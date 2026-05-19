# Early-Data Owner-Surface Reasoning（2026-05-20）

## Goal
- 把 `docs/guides/EARLY_DATA_GUIDE.md` 中 direct context/connection owner path 的“使用原因”写得足够明确，避免读者把 early-data 示例误解成 generic facade 主路径。
- 当前需要锁住的 truth：
  - 这页之所以直接使用 `CreateConnection(...)`，是因为
    `ISSLEarlyDataContext` / `ISSLEarlyDataConnection`
    这组 early-data owner surface 分别挂在 context / connection 对象上
  - 如果调用方只需要普通握手/收发，而不需要 early-data owner surface，
    仍可把握手入口保持在 `TSSLConnector` / `TSSLStream`

## Why now
- generic guides、landing quickstarts、backend quickstarts、diagnostics guides、
  高频专题页、以及 OCSP/CT specialized owner-surface 页面的 direct-path 语义
  已经逐步收口。
- `EARLY_DATA_GUIDE` 虽然已经在用正确的 optional interface，
  但还缺一句“为什么这里必须回到 context/connection owner path，以及 generic main path 仍是什么”。

## Scope
- `docs/guides/EARLY_DATA_GUIDE.md`
- `tests/scripts/test_early_data_owner_surface_reasoning_contract.sh`
- `docs/plans/2026-05-20-early-data-owner-surface-reasoning.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals
- 不修改 Pascal source。
- 不改 early-data capability 设计或 backend 行为真值。
- 不重做既有 replay-store / anti-replay / runtime 验证批次。

## Approach
1. 新增 focused shell contract，冻结：
   - `EARLY_DATA_GUIDE`
     必须明确：
       - 这里直接走 `CreateConnection(...)`，是因为
         `ISSLEarlyDataContext` / `ISSLEarlyDataConnection`
         这组 owner surface 分别挂在 context / connection 对象上
       - 不需要 early-data owner surface 时，普通 client 仍可把握手入口保持在
         `TSSLConnector` / `TSSLStream`
2. 先跑合同拿到 RED。
3. 做最小文档修复。
4. 跑 focused 合同与相关旧合同。
5. 更新 planning files 后提交推送。

## Commands
```bash
bash -n tests/scripts/test_early_data_owner_surface_reasoning_contract.sh
bash tests/scripts/test_early_data_owner_surface_reasoning_contract.sh
bash tests/scripts/test_early_data_docs_truth_contract.sh
git diff --check
git status --short
```

## Expected Outputs
- `EARLY_DATA_GUIDE` 不再让 early-data owner-surface 示例看起来像 generic main entry
- 读者可以清楚知道为什么这页必须下到 context/connection owner path
- 将来如果这页又回漂，focused contract 会立即报警
