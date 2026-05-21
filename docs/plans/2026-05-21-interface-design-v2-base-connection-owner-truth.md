# INTERFACE_DESIGN_V2 Base-Connection Owner Truth

## Goal

收口 `docs/reference/INTERFACE_DESIGN_V2.md`
里关于 `TBaseSSLConnection`
实现边界的残余设计漂移，
让这份 v2 设计锚点重新回到当前 shipped source truth：

- base class 只承载 shared owner / mirror surfaces
- `ISSLClientConnection` / `ISSLNativeHandleAccess` / `ISSLOCSPStapling`
  由 backend-specific subclasses 按 capability / runtime truth 显式挂载

## Why This Batch

当前更高层的 `ISSLConnection` 路线已经不缺：

- owner-path 基本真相
- compiler-deprecated mirror truth
- backend optional interface execution receipt

但 `INTERFACE_DESIGN_V2`
里仍残留一个会直接把 optional 分层讲胖的错误锚点：

- “实现类”示意还把
  `TBaseSSLConnection`
  写成直接实现：
  - `ISSLClientConnection`
  - `ISSLOCSPStapling`

这和当前 source truth 不符：

- `src/fafafa.ssl.connection.base.pas`
  的 base class
  当前只实现：
  - `ISSLConnection`
  - `ISSLConnectionControl`
  - `ISSLDiagnostics`
  - `ISSLSessionResumption`
  - `ISSLCertificateVerification`
  - `ISSLConnectionInfo`
- `ISSLClientConnection`
  / `ISSLNativeHandleAccess`
  / `ISSLOCSPStapling`
  当前都由 backend connection subclasses
  显式追加

如果这块不修，
后续继续推进 `ISSLConnection` slimming 时，
设计文档会继续给出一个比源码更胖的错误实现心智。

## Scope

- `docs/reference/INTERFACE_DESIGN_V2.md`
- `tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
- `docs/plans/2026-05-21-interface-design-v2-base-connection-owner-truth.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals

- 不修改任何 production source
- 不重开 backend optional interface 实现
- 不直接做 `ISSLConnection` public API surgery

## Minimal Fix

1. 扩 focused contract，
   让它同时守住：
   - source base class 声明
   - `INTERFACE_DESIGN_V2`
     的 base-class 代码块
   - “backend subclasses 按 capability 挂载 optional surfaces”
     这条设计说明
2. 运行合同拿到 RED
3. 最小修改 `INTERFACE_DESIGN_V2.md`：
   - base-class 示例改回当前源码真相
   - 增加一段 subclass-layer 说明与代表性示例
   - 把实施计划改成当前更准确的阶段切分
4. 重新运行 focused contracts
5. 更新台账并提交

## Verification

```bash
bash -n tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh
bash tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh
bash tests/scripts/test_native_handle_owner_surface_truth_contract.sh
bash tests/scripts/test_isslocspstapling_compiler_deprecated_contract.sh
git diff --check
```

## Expected Outcome

- `INTERFACE_DESIGN_V2`
  不再把 `TBaseSSLConnection`
  画得比当前源码更胖
- `ISSLConnection`
  主线的设计锚点重新和 source truth 同步
- 后续继续做 broader slimming 时，
  不会再被这份设计文档的旧实现心智带偏
