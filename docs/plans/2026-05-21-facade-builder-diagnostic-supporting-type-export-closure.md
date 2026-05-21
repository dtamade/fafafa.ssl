# Facade Builder And Diagnostic Supporting-Type Export Closure

## Goal

修复 `fafafa.ssl` 主门面仍未完整 re-export builder / diagnostics supporting types 的真实编译缺口，让调用方在写：

```pascal
uses
  fafafa.ssl,
  fafafa.ssl.context.builder;
```

时，也能直接使用当前已发布并由 active public surface 明确依赖的：

- `TBuildValidationResult`
- `TSSLErrorRecord`

而不必仅仅因为 builder validation 结果或 diagnostics 错误历史，就被迫回退 `fafafa.ssl.base`。

## Scope

- Modify: `src/fafafa.ssl.pas`
- Modify: `docs/reference/API_REFERENCE.md`
- Add: `tests/contract/test_facade_builder_diagnostic_supporting_types_entry.pas`
- Add: `tests/scripts/test_facade_builder_diagnostic_supporting_types_export_contract.sh`
- Add: `docs/plans/2026-05-21-facade-builder-diagnostic-supporting-type-export-closure.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

- 当前 `src/fafafa.ssl.pas` 顶部注释仍宣称它“导出所有公共接口和类型”。
- 但基于当前 shipped source：
  - `fafafa.ssl.context.builder`
    的
    `Validate*`
    /
    `Build*WithValidation(...)`
    都依赖
    `TBuildValidationResult`
  - `TSSLDiagnosticInfo.ErrorHistory`
    则明确依赖
    `TSSLErrorRecord`
- `API_REFERENCE.md`
  也已经把：
  - builder validation 语义
  - `TSSLErrorRecord`
  - `TSSLDiagnosticInfo`
  当成当前 active canonical truth 讲解
- 但主门面当前还没有 re-export：
  - `TBuildValidationResult`
  - `TSSLErrorRecord`
- 这不是文档措辞问题，而是两个真实 public facade compile gap：
  - `uses fafafa.ssl, fafafa.ssl.context.builder`
  - 再声明 `TBuildValidationResult`
  - 当前就会直接编译失败
  - `uses fafafa.ssl`
  - 再声明 `TSSLErrorRecord`
  - 当前同样会编译失败

## Minimal Fix

1. 在主门面补齐：
   - `TBuildValidationResult`
   - `TSSLErrorRecord`
   的 type re-export。
2. 在 `API_REFERENCE.md` 补两句，明确主门面 `fafafa.ssl`
   当前也 re-export：
   - builder validation supporting type
   - diagnostics error-history supporting type
3. 用 compile-based focused contract 锁住：
   - source re-export truth
   - `uses fafafa.ssl` / `uses fafafa.ssl, fafafa.ssl.context.builder`
     的最小 supporting-type probe 可编译并运行

## Verification

```bash
bash -n tests/scripts/test_facade_builder_diagnostic_supporting_types_export_contract.sh
bash tests/scripts/test_facade_builder_diagnostic_supporting_types_export_contract.sh
git diff --check
```

## Expected Outcome

- `fafafa.ssl` 作为主门面对 builder / diagnostics supporting types 不再留缺口
- 调用方不需要再因为 `TBuildValidationResult` / `TSSLErrorRecord`
  被迫 split `uses fafafa.ssl.base`
- 这条完整性修复被 focused compile contract 持续守住

## Execution Result

- PASS
- focused contract 首轮 RED 直接证明：
  - `src/fafafa.ssl.pas`
    还没有
    `TBuildValidationResult = fafafa.ssl.base.TBuildValidationResult;`
- 最小修复后：
  - `src/fafafa.ssl.pas`
    现已补齐：
    - `TBuildValidationResult`
    - `TSSLErrorRecord`
  - `docs/reference/API_REFERENCE.md`
    现已明确记录：
    - 主门面 `fafafa.ssl`
      也 re-export
      builder validation
      与
      diagnostics error-history
      supporting types
- focused verification：
  - `bash -n tests/scripts/test_facade_builder_diagnostic_supporting_types_export_contract.sh`
    - PASS
  - `bash tests/scripts/test_facade_builder_diagnostic_supporting_types_export_contract.sh`
    - PASS
  - 邻接门面 contract 继续保持绿色：
    - `tests/scripts/test_facade_option_surface_export_contract.sh`
    - `tests/scripts/test_facade_optional_owner_surface_export_contract.sh`
    - `tests/scripts/test_facade_certificate_supporting_types_export_contract.sh`
  - `git diff --check`
    - PASS
