# 2026-05-21 facade safety surface export closure

## Goal

验证并修复 `fafafa.ssl` 主门面对 `type-safety` public surface 的实际接入缺口，
让调用方在只写：

```pascal
uses
  fafafa.ssl;
```

时，也能直接使用当前仍保留在源码和测试中的 non-generic type-safety surface：

- `TSSLVersion`
- `TKeyType`
- `TCertificateFormat`
- `TKeySize`
- `TTimeoutDuration`
- `TBufferSize`

以及对应的基础转换 helper。

同时把当前更真实的边界说清楚：

- `TSecureData<T>`
- `TResult<T, E>`

这两组 generic pattern
当前继续保留在
`fafafa.ssl.safety`
窄入口，
不把它们误写成已经稳定吸收到
`fafafa.ssl`
主门面。

## Why Now

- `src/fafafa.ssl.safety.pas`
  和
  `tests/test_type_safety.pas`
  说明这套 type-safety surface
  仍然是 shipped code，
  不是 archive 残留。
- 但当前主门面 `src/fafafa.ssl.pas`
  还没有显式 re-export 这组 surface，
  活跃文档也几乎没把它们说明成当前 public API。
- 这会形成一类真实 compile gap：
  - feature 还在
  - 测试也还在
  - 但调用方只走主门面时，
    不能稳定拿到这组类型安全入口

## Scope

- Add:
  - `docs/plans/2026-05-21-facade-safety-surface-export-closure.md`
  - `tests/contract/test_facade_safety_surface_entry.pas`
  - `tests/scripts/test_facade_safety_surface_export_contract.sh`
- Update:
  - `src/fafafa.ssl.pas`
  - `docs/reference/API_REFERENCE.md`
  - `README.md`
  - `docs/guides/MIGRATION_GUIDE.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. 在主门面补齐 safety type / const / helper re-export：
   - 枚举与 unit types
   - `SSLVersionToString(...)`
     / `StringToSSLVersion(...)`
     / `KeyTypeToString(...)`
     / `CertificateFormatToString(...)`
2. 在活跃文档里补一句，
   明确：
   - `fafafa.ssl` 当前也 re-export 这组 non-generic type-safety public surface
   - `TSecureData<T>` / `TResult<T, E>`
     当前继续保留在
     `fafafa.ssl.safety`
3. 用 compile-based focused contract 锁住：
   - source re-export truth
   - `uses fafafa.ssl` 的最小 non-generic safety-surface probe 可编译并运行

## Verification

```bash
bash -n tests/scripts/test_facade_safety_surface_export_contract.sh
bash tests/scripts/test_facade_safety_surface_export_contract.sh
git diff --check
```

## Expected Outcome

- `fafafa.ssl` 对仍然存活的 non-generic type-safety surface
  不再留 public entry compile gap
- API reference / README / migration guide
  对这条 supporting surface 的说明回到当前真相
- generic `TSecureData<T>` / `TResult<T, E>`
  的当前窄入口边界
  也被明确记录下来
- 这条完整性修复被 focused compile contract 持续守住
