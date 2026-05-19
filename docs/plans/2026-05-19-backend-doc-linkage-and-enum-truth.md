# 2026-05-19 Backend Doc Linkage And Enum Truth

## Goal

收掉一批会直接误导后续 backend 审查与文档导航的活跃真相漂移：

1. `docs/BACKEND_CAPABILITY_MATRIX.md` 仍引用不存在的 backend 文档链接
2. `docs/reference/API_REFERENCE.md` 的 `TSSLLibraryType` 示例已经落后于源码
3. `src/fafafa.ssl.base.pas` 里仍把 `sslFreePascal` 注释成“未来”，与当前已实现状态不符

## Scope

- 只修活跃文档与源码注释真相
- 不改 backend 行为实现
- 不重开更大范围的文档索引重写

## Files

- `docs/BACKEND_CAPABILITY_MATRIX.md`
- `docs/reference/API_REFERENCE.md`
- `src/fafafa.ssl.base.pas`
- `tests/scripts/test_backend_doc_linkage_and_enum_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- 顶层 backend capability matrix 必须只链接到实际存在、当前活跃的 backend 参考文档
- `API_REFERENCE` 中的 `TSSLLibraryType` 示例应与源码真实枚举值同步
- `sslFreePascal` 已是当前已实现 backend，不应继续在源码/文档里保留“未来/计划中”表述

## Steps

1. 新增 focused contract，先 RED 命中坏链接与 stale enum truth
2. 最小修改主能力矩阵、API 参考、源码注释
3. 跑 focused contract 与 `git diff --check`

## Commands

```bash
bash -n tests/scripts/test_backend_doc_linkage_and_enum_truth_contract.sh
bash tests/scripts/test_backend_doc_linkage_and_enum_truth_contract.sh
git diff --check
```

## Expected Result

- 主能力矩阵不再引用不存在的 backend 文档
- `TSSLLibraryType` 示例与源码对齐
- `sslFreePascal` 不再被描述为“未来”
