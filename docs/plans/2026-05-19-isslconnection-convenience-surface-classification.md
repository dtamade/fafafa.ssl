# ISSLConnection Convenience Surface Classification

## Goal

在“接口设计 + 各 backend 实现完整性”总目标下，收口 `ISSLConnection` 上这组 convenience/core 混层接口的路线真相，明确：

- 当前 shipped source 仍然保留：
  - `ReadString` / `WriteString`
  - `SetTimeout` / `GetTimeout`
  - `SetBlocking` / `GetBlocking`
- 这些方法在 `v1.5.0` 不是“已经移除”的接口，而是仍在 public surface 中的 convenience-core / connection-adjacent surface
- `API_REFERENCE`、`INTERFACE_DESIGN_V2`、`ARCHITECTURE`、设计审计报告必须对这件事说同一张图，避免后续继续按错误路线反复拉起

## Architecture

- 不做 public Pascal surface 拆接口，也不改各 backend 实现签名。
- 这批只修：
  - source comment classification
  - active canonical doc truth
  - design/audit doc route truth
  - focused shell contract
- 当前真相分三层：
  - source truth：方法仍存在，builder/runtime 仍实际依赖
  - canonical usage truth：新代码的推荐入口应优先写清 builder-first / transport-first 路径
  - v2 route truth：最小 core 仍可作为未来目标，但不能再被写成“当前源码已经移除”

## Current Evidence

- `src/fafafa.ssl.base.pas` 仍正式声明上述 6 个方法。
- `src/fafafa.ssl.connection.builder.pas` 仍通过 `AConnection.SetTimeout(...)` / `SetBlocking(...)` 应用构建期设置。
- `docs/reference/API_REFERENCE.md` 当前仍把这些方法列在 `ISSLConnection` shipped source truth 中。
- 多份活跃 guide 仍直接使用这些方法，说明它们不是 archive 残影。
- 但 `docs/reference/INTERFACE_DESIGN_V2.md` 仍把这组方法写成 `**移除**`。
- `docs/ARCHITECTURE.md` 仍用过窄的最小 snippet 讲 `ISSLConnection`，没有注明这是 conceptual slice 而非 current source truth。
- `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md` 也把 convenience 方法与已开始 deprecate 的 mirror methods 混在一起，容易被误读成“当前实现应该立刻删掉它们”。

## Files

- Add: `docs/plans/2026-05-19-isslconnection-convenience-surface-classification.md`
- Add: `tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
- Update: `src/fafafa.ssl.base.pas`
- Update: `docs/reference/API_REFERENCE.md`
- Update: `docs/reference/INTERFACE_DESIGN_V2.md`
- Update: `docs/ARCHITECTURE.md`
- Update: `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps

1. 记录本批计划，明确这是“分类/路线真相收口”，不是 public API surgery。
2. 先写 focused shell contract，要求：
   - source 仍声明这组 convenience 方法
   - source comments 明确它们的 current recommended path / convenience classification
   - `API_REFERENCE` 明确：
     - `ReadString` / `WriteString` 是 convenience text helpers
     - timeout/blocking 更推荐 builder-first，但连接侧仍保留 convenience override
   - `INTERFACE_DESIGN_V2` 不再把这组方法写成“当前已移除”
   - `ARCHITECTURE` / 设计审计报告补上 current shipped truth 说明
3. 先跑合同拿到 RED。
4. 做最小文档/注释修复。
5. 重新跑 focused 合同与 diff hygiene。
6. 同步 `task_plan.md` / `findings.md` / `progress.md` 后提交推送。

## Verification

1. `bash -n tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
2. `bash tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
3. `git diff --check`
4. `git status --short`

## Risks

- 不要把 scope 扩大成一次真实的 `ISSLConnection` 公共接口拆分；这会波及全部 backend 与大量测试。
- 不要继续让设计文档把“未来最小 core 目标”冒充成“当前源码真相”。
- 不要误伤已经收口的 mirrors / SNI / native-handle 线路。
