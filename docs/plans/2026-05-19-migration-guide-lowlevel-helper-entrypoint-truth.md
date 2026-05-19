# 2026-05-19 Migration Guide Low-Level Helper Entrypoint Truth

## Goal

继续沿着 migration / specialized guide completeness 主线推进，收口 `docs/guides/MIGRATION_GUIDE.md` 中 OpenSSL low-level error helper 片段仍残留旧工厂调用的问题：

- `TSSLFactory.GetLibrary(sslOpenSSL)`

当前高入口 public library-entrypoint 已统一回到：

- `TSSLFactory.GetLibraryInstance(...)`

即使示例是在讲 OpenSSL-specific low-level helper，也不应再把旧 `GetLibrary(...)` 教成当前普通迁移心智。

## Scope

- 只修 `MIGRATION_GUIDE` 中 low-level helper 片段的工厂调用真相
- 收紧现有 `tests/scripts/test_migration_guide_active_truth_contract.sh`
  让它继续覆盖整份 active migration guide 的当前 truth，同时新增：
  - low-level helper 片段使用 `GetLibraryInstance(...)`
  - 不再回流 `GetLibrary(...)`
- 不修改 runtime 实现
- 不重开整份迁移指南的大范围改写

## Files

- `docs/guides/MIGRATION_GUIDE.md`
- `tests/scripts/test_migration_guide_active_truth_contract.sh`
- `docs/plans/2026-05-19-migration-guide-lowlevel-helper-entrypoint-truth.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `TSSLFactory.GetLibraryInstance(...)` 当前是高入口 public library-entrypoint
- `GetFriendlyErrorMessage(...)` / `GetOpenSSLErrorCategory(...)`
  当前来自：
  - `fafafa.ssl.openssl.api.err`
  它们是 OpenSSL-specific low-level helper
- 即使在 low-level helper 语境里，示例也不应再回流旧：
  - `TSSLFactory.GetLibrary(...)`

## Steps

1. 扩展现有 migration guide contract，让旧 `GetLibrary(...)` 示例先 RED。
2. 修正 `MIGRATION_GUIDE` 中 low-level helper 片段，回到 `GetLibraryInstance(...)`。
3. 同步台账，跑轻量验证并提交。

## Commands

```bash
bash -n tests/scripts/test_migration_guide_active_truth_contract.sh
bash tests/scripts/test_migration_guide_active_truth_contract.sh
git diff --check
```

## Expected Result

- `MIGRATION_GUIDE` 不再在 low-level helper 片段里回流旧 `GetLibrary(...)`
- 迁移指南的高入口工厂心智进一步统一
- 现有 migration guide contract 能覆盖这条单点残余，后续不会反复回流

## Result

- 已完成。
- `docs/guides/MIGRATION_GUIDE.md` 中 OpenSSL low-level helper 片段现在已回到：
  - `TSSLFactory.GetLibraryInstance(sslOpenSSL)`
- 现有 `tests/scripts/test_migration_guide_active_truth_contract.sh`
  现在已额外锁住：
  - low-level helper 片段必须使用 `GetLibraryInstance(...)`
  - 不再回流 `GetLibrary(...)`

## Verification

```bash
bash -n tests/scripts/test_migration_guide_active_truth_contract.sh
bash tests/scripts/test_migration_guide_active_truth_contract.sh
git diff --check
```

- 结果：全部通过
