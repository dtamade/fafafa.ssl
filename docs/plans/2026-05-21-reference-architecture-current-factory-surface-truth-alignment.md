# 2026-05-21 reference/ARCHITECTURE 当前 factory surface 真相对齐

## Goal

修复 `docs/reference/ARCHITECTURE.md`
仍在发布旧工厂签名、
旧类型名、
以及旧模块名的高入口 reference drift，
让这份 reference 架构页重新对齐到当前 `v1.5.0` public truth。

## Why Now

当前这页仍残留几类直接误导实现者的旧 truth：

1. 工厂签名仍写成旧参数顺序：
   `CreateContext(ALibType, AContextType)`
2. 可用库集合类型仍写成旧名字：
   `TSSLLibraryTypeSet`
3. 模块名仍写：
   - `fafafa.ssl.types`
   - `fafafa.ssl.intf`
   但当前 source truth 已集中在
   `fafafa.ssl.base`
4. 这页也还没把当前推荐入口说清楚：
   - 普通新代码：
     `fafafa.ssl`
     +
     `TSSLContextBuilder`
     /
     `TSSLConnector`
   - fixed-backend / core factory：
     `TSSLFactory.GetLibraryInstance(...)`
     /
     `TSSLFactory.CreateContext(...)`

## Scope

- Add:
  - `docs/plans/2026-05-21-reference-architecture-current-factory-surface-truth-alignment.md`
  - `tests/scripts/test_reference_architecture_current_factory_surface_truth_contract.sh`
- Update:
  - `docs/reference/ARCHITECTURE.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. 把模块名切回当前 truth：
   - `fafafa.ssl.base`
   - `fafafa.ssl.factory`
   - `fafafa.ssl`
2. 把工厂签名切回当前 source truth：
   - `CreateContext(AContextType, ALibType)`
   - `GetAvailableLibraries: TSSLLibraryTypes`
3. 补回当前入口说明：
   - `TSSLContextBuilder`
   - `TSSLConnector`
   - `TSSLFactory.GetLibraryInstance(...)`
4. 不扩张到 broader runtime / backend completeness，
   只修 reference 架构页的 current-truth drift

## Verification

```bash
bash -n tests/scripts/test_reference_architecture_current_factory_surface_truth_contract.sh
bash tests/scripts/test_reference_architecture_current_factory_surface_truth_contract.sh
git diff --check
```

## Expected Result

- reference 架构页不再继续发布旧工厂签名
- 当前模块名 / 类型名 / 工厂 surface
  与源码和活跃文档保持一致
