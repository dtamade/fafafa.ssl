# Specialized Owner-Surface Reasoning（2026-05-20）

## Goal
- 把 specialized optional-interface guides 中 direct connection owner path 的“使用原因”写得足够明确，避免读者把这些 owner-surface 示例误解成 generic facade 主路径。
- 当前需要锁住的 truth：
  - `OCSP_USAGE_GUIDE` 之所以回到 `CreateConnection(...)`，是因为
    `ISSLOCSPStapling` / `ISSLCertificateVerification` 这组 runtime owner surface
    挂在连接对象上
  - `CT_IMPLEMENTATION_GUIDE` 之所以回到 `CreateConnection(...)`，是因为
    `ISSLCertificateTransparency` / `ISSLCertificateTransparencyValidation`
    挂在连接对象上
  - 如果调用方只需要普通握手/收发，而不需要这些 owner surface，
    仍可把握手入口保持在 `TSSLConnector` / `TSSLStream`

## Why now
- generic guides、landing quickstarts、backend quickstarts、diagnostics guides、
  以及高频专题页的 direct-path 语义已经逐步收口。
- specialized owner-surface guides 虽然已经在用正确的 optional interface，
  但还缺一句“为什么这里必须回到 connection owner path，以及 generic main path 仍是什么”。

## Scope
- `docs/guides/OCSP_USAGE_GUIDE.md`
- `docs/guides/CT_IMPLEMENTATION_GUIDE.md`
- `tests/scripts/test_specialized_owner_surface_reasoning_contract.sh`
- `docs/plans/2026-05-20-specialized-owner-surface-reasoning.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals
- 不修改 Pascal source。
- 不改 optional interface 设计或 capability 语义。
- 不重做既有 OCSP/CT runtime boundary 收口。

## Approach
1. 新增 focused shell contract，冻结：
   - `OCSP_USAGE_GUIDE`
     必须明确：
       - 这里直接走 `CreateConnection(...)` 是因为 `ISSLOCSPStapling` /
         `ISSLCertificateVerification` 属于连接 owner surface
       - 不需要这些 owner surface 时，普通 client 仍可把握手入口保持在
         `TSSLConnector` / `TSSLStream`
   - `CT_IMPLEMENTATION_GUIDE`
     必须明确：
       - 这里直接走 `CreateConnection(...)` 是因为 CT runtime owner surface
         挂在连接对象上
       - 不需要 CT owner surface 时，普通 client 仍可把握手入口保持在
         `TSSLConnector` / `TSSLStream`
2. 先跑合同拿到 RED。
3. 做最小文档修复。
4. 跑 focused 合同与相关旧合同。
5. 更新 planning files 后提交推送。

## Commands
```bash
bash -n tests/scripts/test_specialized_owner_surface_reasoning_contract.sh
bash tests/scripts/test_specialized_owner_surface_reasoning_contract.sh
git diff --check
git status --short
```

## Expected Outputs
- OCSP/CT specialized guides 不再让 owner-surface 示例看起来像 generic main entry
- 读者可以清楚知道为什么这些页必须下到 connection owner path
- 将来如果这两页又回漂，focused contract 会立即报警

