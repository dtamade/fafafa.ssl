# 2026-05-19 Implemented Backend Future Truth Sweep

## Goal

收掉活跃文档中把已实现 backend 仍写成“计划中/未来”的过期表述，避免继续误导接口设计与 backend completeness 路线判断。

## Scope

- 只修当前活跃文档中的 stale future/planned truth：
  - `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md`
  - `docs/guides/USER_GUIDE.md`
  - `docs/MIGRATION_GUIDE_V1.1.md`
  - `docs/ARCHITECTURE.md`
  - `docs/NATIVE_HANDLE_QUICK_REF.md`
- 不触碰 archive / historical reports
- 不重写真正的长期 roadmap 段落

## Files

- `docs/reference/BACKEND_ABSTRACTION_LAYER_DESIGN.md`
- `docs/guides/USER_GUIDE.md`
- `docs/MIGRATION_GUIDE_V1.1.md`
- `docs/ARCHITECTURE.md`
- `docs/NATIVE_HANDLE_QUICK_REF.md`
- `tests/scripts/test_implemented_backend_future_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `sslFreePascal` 已是当前源码与工厂可用 backend，不应继续被活跃文档说成“未来”
- `sslMbedTLS` 也已是当前活跃 backend，不应在用户导向文档中继续挂“未来”尾巴
- `GetNativeHandle` 的 optional-boundary 价值不再依赖“未来某个纯 Pascal backend 才会出现”，而是已服务于当前 `sslFreePascal` truth

## Steps

1. 新增 focused contract，对 5 处 stale future/planned wording 先做 RED
2. 最小修改活跃文档，用当前 implemented-backend truth 替换过期说法
3. 跑 focused contract 与 `git diff --check`

## Commands

```bash
bash -n tests/scripts/test_implemented_backend_future_truth_contract.sh
bash tests/scripts/test_implemented_backend_future_truth_contract.sh
git diff --check
```

## Expected Result

- 活跃文档不再把 `sslFreePascal` / `sslMbedTLS` 当成 future-only backend
- optional native-handle 相关文档示例重新锚定当前 backend 枚举真相
