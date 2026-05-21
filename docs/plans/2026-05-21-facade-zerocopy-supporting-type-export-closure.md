# Facade Zero-Copy Supporting-Type Export Closure

## Goal

修复 `fafafa.ssl` 主门面仍未完整 re-export zero-copy supporting type 的真实编译缺口，让调用方在写：

```pascal
uses
  fafafa.ssl,
  fafafa.ssl.encoding,
  fafafa.ssl.crypto.utils;
```

时，也能直接使用当前已发布并由 active utility public surface 明确依赖的：

- `TBytesView`

而不必仅仅因为 zero-copy 编码 / 哈希入口，就被迫回退 `fafafa.ssl.base`。

## Scope

- Modify: `src/fafafa.ssl.pas`
- Modify: `docs/reference/API_REFERENCE.md`
- Add: `tests/contract/test_facade_zerocopy_supporting_type_entry.pas`
- Add: `tests/scripts/test_facade_zerocopy_supporting_type_export_contract.sh`
- Add: `docs/plans/2026-05-21-facade-zerocopy-supporting-type-export-closure.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

- 当前 `src/fafafa.ssl.pas` 顶部注释仍宣称它“导出所有公共接口和类型”。
- 但基于当前 shipped source：
  - `fafafa.ssl.encoding`
    的
    `Base64EncodeView(...)`
    公开依赖
    `TBytesView`
  - `fafafa.ssl.crypto.utils`
    的
    `SHA256View(...)`
    /
    `SHA512View(...)`
    /
    `UpdateView(...)`
    等 zero-copy 入口
    也公开依赖
    `TBytesView`
- 主门面当前却还没有 re-export：
  - `TBytesView`
- 这不是文档措辞问题，而是一个真实 public facade compile gap：
  - `uses fafafa.ssl, fafafa.ssl.encoding`
  - 再声明 `TBytesView`
  - 当前就会直接编译失败

## Minimal Fix

1. 在主门面补齐：
   - `TBytesView`
   的 type re-export。
2. 在 `API_REFERENCE.md` 补一句，明确主门面 `fafafa.ssl`
   当前也 re-export zero-copy supporting type。
3. 用 compile-based focused contract 锁住：
   - source re-export truth
   - `uses fafafa.ssl, fafafa.ssl.encoding, fafafa.ssl.crypto.utils`
     的最小 zero-copy probe 可编译并运行

## Verification

```bash
bash -n tests/scripts/test_facade_zerocopy_supporting_type_export_contract.sh
bash tests/scripts/test_facade_zerocopy_supporting_type_export_contract.sh
git diff --check
```

## Expected Outcome

- `fafafa.ssl` 作为主门面对 zero-copy utility supporting type 不再留缺口
- 调用方不需要再因为 `TBytesView`
  被迫 split `uses fafafa.ssl.base`
- 这条完整性修复被 focused compile contract 持续守住

## Execution Result

- PASS
- focused contract 首轮 RED 直接证明：
  - `src/fafafa.ssl.pas`
    还没有
    `TBytesView = fafafa.ssl.base.TBytesView;`
- 最小修复后：
  - `src/fafafa.ssl.pas`
    现已补齐
    `TBytesView`
    的主门面 re-export
  - `docs/reference/API_REFERENCE.md`
    现已明确记录：
    - 主门面 `fafafa.ssl`
      也 re-export
      zero-copy supporting type
      `TBytesView`
- focused verification：
  - `bash -n tests/scripts/test_facade_zerocopy_supporting_type_export_contract.sh`
    - PASS
  - `bash tests/scripts/test_facade_zerocopy_supporting_type_export_contract.sh`
    - PASS
  - 邻接门面 contract 继续保持绿色：
    - `tests/scripts/test_facade_certificate_supporting_types_export_contract.sh`
    - `tests/scripts/test_facade_builder_diagnostic_supporting_types_export_contract.sh`
  - `git diff --check`
    - PASS
