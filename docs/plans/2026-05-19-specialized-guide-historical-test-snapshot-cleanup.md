# Specialized Guide Historical Test Snapshot Cleanup

## Goal

把 `CMS_USER_GUIDE.md` 和 `PKCS12_USER_GUIDE.md` 里的历史测试快照从“当前指南正文 truth”降级掉，只保留可执行验证入口与当前 surface 边界，避免这些 specialized guides 继续硬编码会漂移的测试统计与通过率。

## Architecture

这批保持 docs-only：

- 新增 focused shell contract，冻结 CMS / PKCS12 specialized guide 的“去快照化”边界
- 只改两份 specialized guides
- 不动生产实现
- 不扩大到性能类文档

## Files

- Add: `docs/plans/2026-05-19-specialized-guide-historical-test-snapshot-cleanup.md`
- Add: `tests/scripts/test_specialized_guides_historical_test_snapshot_contract.sh`
- Modify: `docs/guides/CMS_USER_GUIDE.md`
- Modify: `docs/guides/PKCS12_USER_GUIDE.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

`CMS_USER_GUIDE` / `PKCS12_USER_GUIDE` 当前虽然总体功能叙事没有明显错位，但仍把下列历史快照直接写进当前指南正文：

- 固定的测试通过率
- 固定的总测试数
- 捕获式“预期输出”区块
- 带具体百分比/数量的历史更新日志

这些内容会不断漂移，且会把“当前应该怎么用”与“某个历史时点的测试结果”混在一起。

## Verification

```bash
bash -n tests/scripts/test_specialized_guides_historical_test_snapshot_contract.sh
bash tests/scripts/test_specialized_guides_historical_test_snapshot_contract.sh
npx prettier --write docs/guides/CMS_USER_GUIDE.md docs/guides/PKCS12_USER_GUIDE.md
git diff --check
```

## Expected Outcome

- CMS / PKCS12 specialized guides 仍保留：
  - 当前 surface 边界
  - 测试入口文件/命令
  - 使用示例
- 但不再把：
  - `43/43`
  - `34/34`
  - `100.0%`
  - `总测试数`
  等历史快照写成当前正文 truth
