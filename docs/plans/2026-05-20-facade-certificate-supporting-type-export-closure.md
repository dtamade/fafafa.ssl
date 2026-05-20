# Facade Certificate Supporting-Type Export Closure

## Goal

修复 `fafafa.ssl` 主门面仍未完整 re-export 证书 public surface 常用 supporting types 的真实编译缺口，让调用方在只写：

```pascal
uses
  fafafa.ssl;
```

时，也能直接使用当前已发布并在活跃 API 文档中出现的：

- `TSSLStringArray`
- `TSSLCertVerifyResult`

而不必因为证书 SAN / verify-result 这类基础类型退回 `fafafa.ssl.base`。

## Scope

- Modify: `src/fafafa.ssl.pas`
- Modify: `docs/reference/API_REFERENCE.md`
- Add: `tests/contract/test_facade_certificate_supporting_types_entry.pas`
- Add: `tests/scripts/test_facade_certificate_supporting_types_export_contract.sh`
- Add: `docs/plans/2026-05-20-facade-certificate-supporting-type-export-closure.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

- `src/fafafa.ssl.pas` 顶部注释当前仍宣称它“导出所有公共接口和类型”。
- 但基于当前源码，主门面还没有完整 re-export：
  - `TSSLStringArray`
  - `TSSLCertVerifyResult`
- 这两种类型都已经是当前 shipped public surface 的一部分：
  - `ISSLCertificate.GetSubjectAltNames/GetKeyUsage/GetExtendedKeyUsage`
    使用 `TSSLStringArray`
  - `ISSLCertificate.VerifyEx(...)`
    使用 `TSSLCertVerifyResult`
- `API_REFERENCE.md` 也已经把这两种类型作为活跃 canonical truth 直接展示出来。
- 所以这不是文档措辞问题，而是一个真实 public facade compile gap：
  - 只 `uses fafafa.ssl`
  - 再声明 `TSSLStringArray` 或 `TSSLCertVerifyResult`
  - 当前就会直接编译失败

## Minimal Fix

1. 在主门面补齐：
   - `TSSLStringArray`
   - `TSSLCertVerifyResult`
   的 type re-export。
2. 在 `API_REFERENCE.md` 补一句，明确主门面 `fafafa.ssl`
   当前也 re-export 证书 public surface 常用 supporting types。
3. 用 compile-based focused contract 锁住：
   - source re-export truth
   - `uses fafafa.ssl` 的最小 certificate-supporting-type probe 可编译并运行

## Verification

```bash
bash -n tests/scripts/test_facade_certificate_supporting_types_export_contract.sh
bash tests/scripts/test_facade_certificate_supporting_types_export_contract.sh
git diff --check
```

## Expected Outcome

- `fafafa.ssl` 作为主门面对证书 public surface 的 supporting types 不再留缺口
- 调用方不需要再因为 `TSSLStringArray` / `TSSLCertVerifyResult`
  被迫 split `uses fafafa.ssl.base`
- 这条完整性修复被 focused compile contract 持续守住

## Execution Result

- PASS
- focused contract 首轮 RED 直接证明：
  - `src/fafafa.ssl.pas`
    还没有
    `TSSLStringArray = fafafa.ssl.base.TSSLStringArray;`
- 最小修复后：
  - `src/fafafa.ssl.pas`
    现已补齐：
    - `TSSLStringArray`
    - `TSSLCertVerifyResult`
  - `docs/reference/API_REFERENCE.md`
    现已明确记录：
    - 主门面 `fafafa.ssl`
      也 re-export 这组证书 supporting types
- focused verification：
  - `bash -n tests/scripts/test_facade_certificate_supporting_types_export_contract.sh`
    - PASS
  - `bash tests/scripts/test_facade_certificate_supporting_types_export_contract.sh`
    - PASS
  - 邻接门面 contract 继续保持绿色：
    - `tests/scripts/test_facade_optional_owner_surface_export_contract.sh`
    - `tests/scripts/test_facade_capability_native_handle_export_contract.sh`
  - `git diff --check`
    - PASS
