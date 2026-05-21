# ARCHITECTURE Optional Owner-Surface Truth

## Goal

收口顶层 `docs/ARCHITECTURE.md`
对 `ISSLConnection`
相关 optional owner surface
与 backend capability-gated interface 暴露的残余漂移，
让这份高可见度架构总览
重新和当前 shipped source / active reference
说同一张图。

## Why This Batch

在 `INTERFACE_DESIGN_V2`
的 base-owner truth 收口后，
当前更高可见度的总览页
还留着两个会继续讲歪路线的点：

1. `接口继承关系`
   仍只画出
   `ISSLClientConnection`
   ，
   没把当前已经正式发布的
   connection-side owner surfaces
   讲清楚
2. `后端接口实现`
   仍写成：
   - “每个后端实现所有核心接口 + 可选接口”
   这会误导成
   optional interface
   是统一全挂载，
   而不是按 capability / runtime truth 暴露

这页是顶层架构总览，
如果这里不先收口，
读者即使看过 canonical reference，
也还是会在回到总览页时把心智带偏。

## Scope

- `docs/ARCHITECTURE.md`
- `tests/scripts/test_architecture_optional_owner_surface_truth_contract.sh`
- `docs/plans/2026-05-21-architecture-optional-owner-surface-truth.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals

- 不修改生产源码
- 不重开 backend capability runtime 实现
- 不重写整份架构文档

## Minimal Fix

1. 新增 focused contract，锁住：
   - `docs/ARCHITECTURE.md`
     明确列出
     `ISSLConnectionControl`
     /
     `ISSLConnectionInfo`
     /
     `ISSLDiagnostics`
     /
     `ISSLSessionResumption`
     /
     `ISSLCertificateVerification`
     /
     `ISSLOCSPStapling`
   - 明确 optional surfaces
     是按 capability / runtime truth 暴露
   - 不再出现
     “每个后端实现所有核心接口 + 可选接口”
     这种统一全挂载表述
2. 运行合同拿到 RED
3. 最小修改 `docs/ARCHITECTURE.md`
4. 跑 focused contracts
5. 更新台账并提交

## Verification

```bash
bash -n tests/scripts/test_architecture_optional_owner_surface_truth_contract.sh
bash tests/scripts/test_architecture_optional_owner_surface_truth_contract.sh
bash tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh
git diff --check
```

## Expected Outcome

- 顶层 `ARCHITECTURE`
  不再把
  `ISSLConnection`
  的 optional surfaces
  讲成隐形或统一全挂载
- 总览页与 `API_REFERENCE`
  /
  `INTERFACE_DESIGN_V2`
  的 current truth
  重新同步
- 后续做 broader interface completeness
  时，
  不会再被这页总览的旧心智拉偏
