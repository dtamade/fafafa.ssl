# Architecture Backend Status Truth

## Goal

把 `docs/reference/ARCHITECTURE.md` 里的 backend 状态表从“生产就绪 / 100% 完成”式 release wording 收紧回当前架构页应表达的 active backend / bounded runtime truth。

## Architecture

这批保持 docs-only：

- 新增 focused shell contract，冻结 `ARCHITECTURE.md` 的 backend-status truth 边界
- 只修改 `docs/reference/ARCHITECTURE.md`
- 不改生产实现
- 不重写整份架构文档的历史 phase 小节

## Files

- Add: `docs/plans/2026-05-19-architecture-backend-status-truth.md`
- Add: `tests/scripts/test_architecture_backend_status_truth_contract.sh`
- Modify: `docs/reference/ARCHITECTURE.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

`ARCHITECTURE.md` 当前仍把 backend 状态表写成：

- `OpenSSL ... ✅ 生产就绪`
- `WinSSL ... 100% 完成`

但这页的角色应该是解释架构分层与模块组织，不是承担当前 release/runtime 状态公告牌。

## Verification

```bash
bash -n tests/scripts/test_architecture_backend_status_truth_contract.sh
bash tests/scripts/test_architecture_backend_status_truth_contract.sh
npx prettier --write docs/reference/ARCHITECTURE.md
git diff --check
```

## Expected Outcome

- `ARCHITECTURE.md` 保留：
  - backend 模块组织
  - active/default/optional 的架构角色
  - current truth source 指向
- 但不再把：
  - `生产就绪`
  - `100% 完成`
  作为 backend 状态表的当前 truth
