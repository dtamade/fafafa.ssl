# Facade Option Surface Export Closure

## Goal

修复 `fafafa.ssl` 主门面仍未完整 re-export option public surface 的真实编译缺口，让调用方在只写：

```pascal
uses
  fafafa.ssl;
```

时，也能直接使用当前已发布并由 `ISSLContext` / `TSSLConfig` / builder surface 共同依赖的：

- `TSSLOption`
- `TSSLOptions`
- `sso*` option 常量

而不必仅仅因为上下文选项配置就被迫回退 `fafafa.ssl.base`。

## Scope

- Modify: `src/fafafa.ssl.pas`
- Modify: `docs/reference/API_REFERENCE.md`
- Add: `tests/contract/test_facade_option_surface_entry.pas`
- Add: `tests/scripts/test_facade_option_surface_export_contract.sh`
- Add: `docs/plans/2026-05-21-facade-option-surface-export-closure.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

- 当前 `src/fafafa.ssl.pas` 顶部注释仍宣称它“导出所有公共接口和类型”。
- 但基于当前 shipped source：
  - `ISSLContext.SetOptions/GetOptions`
  - `TSSLConfig.Options`
  - `TSSLContextBuilder.WithOption/WithOptions`
  都依赖 `TSSLOption` / `TSSLOptions`
- 主门面当前却还没有 re-export：
  - `TSSLOption`
  - `TSSLOptions`
  - `ssoEnableSNI` 等 option 常量
- 这不是文档措辞问题，而是一个真实 public facade compile gap：
  - 只 `uses fafafa.ssl`
  - 再声明 `TSSLOptions` 或使用 `[ssoEnableSNI]`
  - 当前就会直接编译失败

## Minimal Fix

1. 在主门面补齐：
   - `TSSLOption`
   - `TSSLOptions`
   - `sso*` option 常量
   的 type/const re-export。
2. 在 `API_REFERENCE.md` 补一句，明确主门面 `fafafa.ssl`
   当前也 re-export 这组 context option public surface。
3. 用 compile-based focused contract 锁住：
   - source re-export truth
   - `uses fafafa.ssl` 的最小 option-surface probe 可编译并运行

## Verification

```bash
bash -n tests/scripts/test_facade_option_surface_export_contract.sh
bash tests/scripts/test_facade_option_surface_export_contract.sh
git diff --check
```

## Expected Outcome

- `fafafa.ssl` 作为主门面对 context option surface 不再留缺口
- 调用方不需要再因为 `TSSLOption` / `TSSLOptions` / `sso*` 常量
  被迫 split `uses fafafa.ssl.base`
- 这条完整性修复被 focused compile contract 持续守住

## Execution Result

- PASS
- focused contract 首轮 RED 直接证明：
  - `src/fafafa.ssl.pas`
    还没有
    `TSSLOption = fafafa.ssl.base.TSSLOption;`
- 最小修复后：
  - `src/fafafa.ssl.pas`
    现已补齐：
    - `TSSLOption`
    - `TSSLOptions`
    - 全量 `sso*` option 常量
  - `docs/reference/API_REFERENCE.md`
    现已明确记录：
    - 主门面 `fafafa.ssl`
      当前也 re-export 这组 option public surface
- focused verification：
  - `bash -n tests/scripts/test_facade_option_surface_export_contract.sh`
    - PASS
  - `bash tests/scripts/test_facade_option_surface_export_contract.sh`
    - PASS
  - 邻接门面 / API reference 合同继续保持绿色：
    - `tests/scripts/test_facade_optional_owner_surface_export_contract.sh`
    - `tests/scripts/test_facade_certificate_supporting_types_export_contract.sh`
    - `tests/scripts/test_api_reference_library_context_surface_truth_contract.sh`
  - `git diff --check`
    - PASS
