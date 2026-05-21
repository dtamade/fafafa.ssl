# ARCHITECTURE Current Route Truth Alignment

## Goal

把根层 `docs/ARCHITECTURE.md` 从“当前架构总览 + 已经混入历史 future-version bucket”的状态，
收回到真正可继续作为当前总览入口的 truth：

- 保留当前 public entrypoint / owner-surface / backend layout 真相
- 去掉会误导路线判断的旧阶段快照
- 明确当前执行顺序、release 状态与下一条 completeness 主线应回到哪里看

## Why This Batch

当前 `docs/ARCHITECTURE.md` 虽然头部已经写成：

- `v1.5.0`
- `当前架构总览（已对齐 v1.5.0 public truth）`

但尾部仍保留：

- `### 短期（v1.2-v1.3）`
- `### 中期（v2.0）`
- `### 长期（v3.0）`

这类旧 future-version 路线桶。

这会直接制造两层误导：

1. 让读者以为这页同时还是当前 roadmap
2. 把已发布项目重新讲回“未来版本规划草稿”

## Scope

- Add:
  - `docs/plans/2026-05-21-architecture-current-route-truth-alignment.md`
  - `tests/scripts/test_architecture_current_route_truth_contract.sh`
- Update:
  - `docs/ARCHITECTURE.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Non-Goals

- 不修改生产源码
- 不重开 `docs/reference/ARCHITECTURE.md` 那条 reference-level truth lane
- 不重写整份架构文档的分层与接口主体
- 不把范围扩大到新的 runtime/backend gate

## Minimal Fix

1. 新增 focused contract：
   - 根层 `ARCHITECTURE.md` 必须包含“当前路线与演进边界”
   - 必须把执行顺序指回 `ROADMAP.md` 与当前 completeness 主线
   - 不得继续保留 `v1.2-v1.3 / v2.0 / v3.0` 这组旧路线桶
2. 最小修改根层 `ARCHITECTURE.md`：
   - 保留当前架构主体
   - 把旧 future-version section 改成 current-route guidance
3. 跑 focused contract 与现有 ARCHITECTURE 合同，确认没有打回已收口 truth

## Verification

```bash
bash -n tests/scripts/test_architecture_current_route_truth_contract.sh
bash tests/scripts/test_architecture_current_route_truth_contract.sh
bash tests/scripts/test_architecture_current_public_entrypoint_truth_contract.sh
bash tests/scripts/test_architecture_optional_owner_surface_truth_contract.sh
git diff --check
```

## Expected Outcome

- 根层 `ARCHITECTURE.md` 不再继续承担过时的版本阶段计划
- 读者会被明确导向当前路线图与现行 completeness 主线
- 架构总览重新只负责“当前分层真相 + 当前路线指针”
