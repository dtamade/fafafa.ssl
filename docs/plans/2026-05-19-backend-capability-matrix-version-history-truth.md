# Backend Capability Matrix Version History Truth

## Goal

收紧 `docs/BACKEND_CAPABILITY_MATRIX.md` 底部版本口径，避免根入口继续把 `v1.4.x`
历史 capability 里程碑误读成当前发布真相；先明确当前 `v1.5.0` 权威入口，再保留历史
capability milestone 作为附录信息。

## Architecture

这批保持 docs-only：

- 新增 focused shell contract，冻结根入口能力矩阵的当前版本口径
- 只修改 `docs/BACKEND_CAPABILITY_MATRIX.md`
- 不改 release 实现
- 不重写 `RELEASE_NOTES.md` / `ROADMAP.md`

## Files

- Add: `docs/plans/2026-05-19-backend-capability-matrix-version-history-truth.md`
- Add: `tests/scripts/test_backend_capability_matrix_version_history_truth_contract.sh`
- Modify: `docs/BACKEND_CAPABILITY_MATRIX.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

当前 `docs/BACKEND_CAPABILITY_MATRIX.md` 底部还存在一个很容易被忽略、但会直接误导
发布路线判断的 summary drift：

- source 版本真相已经是：
  - `FAFAFA_SSL_VERSION_STRING = '1.5.0'`
- `docs/ROADMAP.md` / `docs/RELEASE_NOTES.md`
  已经明确：
  - 当前 stable release 是 `v1.5.0`
- 但 `docs/BACKEND_CAPABILITY_MATRIX.md`
  的底部仍直接从 `v1.4.1` / `v1.4.0` / `v1.3.0` 开始列“版本历史”

这会让读者在根入口形成错误心智：

- 以为这页自己已经覆盖当前 release truth
- 或把旧 capability 里程碑错看成当前 `v1.5.0` 发布结论

## Verification

```bash
bash -n tests/scripts/test_backend_capability_matrix_version_history_truth_contract.sh
bash tests/scripts/test_backend_capability_matrix_version_history_truth_contract.sh
npx prettier --write docs/BACKEND_CAPABILITY_MATRIX.md
git diff --check
```

## Expected Outcome

- 根入口能力矩阵先指向当前 `v1.5.0` 权威入口
- 历史条目明确降级为 capability milestone
- 读者不会再把 `v1.4.x` 列表误读成当前 release/runtime truth
