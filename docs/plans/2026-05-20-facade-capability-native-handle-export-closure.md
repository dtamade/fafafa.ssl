# Facade Capability And Native-Handle Export Closure

## Goal

修复 `fafafa.ssl` 主门面未完整 re-export capability / native-handle public surface 的真实编译缺口，让调用方在只写：

```pascal
uses
  fafafa.ssl;
```

时，也能直接访问当前已发布的：

- `TSSLBackendCapabilities`
- `TSSLBackendImplType`
- `TSSLFeatureSupportLevel`
- `ISSLNativeHandleAccess`
- capability helper functions / constants

而不必被迫退回 `fafafa.ssl.base` 拆分入口。

## Scope

- Modify: `src/fafafa.ssl.pas`
- Modify: `docs/reference/API_REFERENCE.md`
- Add: `tests/contract/test_facade_capability_native_handle_entry.pas`
- Add: `tests/scripts/test_facade_capability_native_handle_export_contract.sh`
- Add: `docs/plans/2026-05-20-facade-capability-native-handle-export-closure.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

- `src/fafafa.ssl.pas` 顶部注释当前仍宣称它“导出所有公共接口和类型”。
- 但基于当前源码，主门面并没有完整 re-export：
  - `ISSLNativeHandleAccess`
  - `TSSLBackendCapabilities`
  - `TSSLBackendImplType`
  - `TSSLFeatureSupportLevel`
  - 以及 capability helper functions / constants
- 这不是单纯文档漂移，而是一个真实 public façade compile gap：
  - 只 `uses fafafa.ssl`
  - 再写 capability / native-handle 基础调用
  - 当前就会直接编译失败

## Minimal Fix

1. 在主门面补齐 capability / native-handle 相关 type/interface/const re-export。
2. 在主门面补齐 capability helper function forwarding：
   - `IsCipherSupported`
   - `IsHashSupported`
   - `IsKeyExchangeSupported`
   - `IsFeatureStable`
   - `IsFeatureUsable`
   - `IsFeatureDeprecated`
   - `NormalizeLegacyCapabilityBooleans`
   - `IsNativeBackend`
   - `IsCLibraryBackend`
   - `RequiresExternalDependencies`
   - `GetSecurityScore`
   - `GetPerformanceScore`
   - `GetCapabilitiesDescription`
3. 在 `API_REFERENCE` 记录：
   - 主门面 `fafafa.ssl` 当前也 re-export 这组 capability / native-handle public surface
4. 用 compile-based focused contract 锁住：
   - source re-export truth
   - `uses fafafa.ssl` 的最小 capability/native-handle probe 可编译并运行

## Verification

```bash
bash -n tests/scripts/test_facade_capability_native_handle_export_contract.sh
bash tests/scripts/test_facade_capability_native_handle_export_contract.sh
git diff --check
```

## Expected Outcome

- `fafafa.ssl` 作为主门面对 capability / native-handle 这组已发布 public surface 不再缺口
- 调用方不需要再因为这组基础 capability 查询被迫 split `uses fafafa.ssl.base`
- 这条完整性修复被 focused compile contract 持续守住
