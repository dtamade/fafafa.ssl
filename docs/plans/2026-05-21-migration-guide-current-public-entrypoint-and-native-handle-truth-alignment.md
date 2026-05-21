# 2026-05-21 MIGRATION_GUIDE 当前 public entrypoint 与 native-handle 真相对齐

## Goal

修复 `docs/MIGRATION_GUIDE_V1.1.md` 中仍会把读者带去旧工厂/旧入口心智的内容，
让这份迁移文档继续保留 v1.1 native-handle 设计背景，
但不再发布已经不存在或不再推荐的 public entrypoint。

## Why Now

当前这页仍残留几类高风险漂移：

1. “不受影响（99% 用户）” 段仍展示
   `TSSLFactory.CreateLibrary(...)`
2. 多处 native-handle 迁移示例仍写
   `Factory.CreateContext(...)`
3. 智能后端选择示例仍写
   `TSSLFactory.GetLibrary(...)`
4. 这会让迁移文档继续把调用方带离当前公开推荐面：
   - 普通新代码：
     `uses fafafa.ssl;` +
     `TSSLContextBuilder` /
     `TSSLConnector`
   - 高级 fixed-backend / native-handle 场景：
     `TSSLFactory.GetLibraryInstance(...)`
     + `Lib.CreateContext(...)`

## Scope

- Add:
  - `docs/plans/2026-05-21-migration-guide-current-public-entrypoint-and-native-handle-truth-alignment.md`
  - `tests/scripts/test_migration_guide_current_public_entrypoint_truth_contract.sh`
- Update:
  - `docs/MIGRATION_GUIDE_V1.1.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. 把“99% 用户”示例切回当前统一入口：
   `fafafa.ssl` +
   `TSSLContextBuilder` /
   `TSSLConnector`
2. 把 native-handle 高级场景示例切回：
   `TSSLFactory.GetLibraryInstance(...)`
   + `Lib.CreateContext(...)`
3. 去掉迁移文档中的旧 public entrypoint 漂移：
   - `TSSLFactory.CreateLibrary(...)`
   - `Factory.CreateContext(...)`
   - `TSSLFactory.GetLibrary(...)`
4. 保留并强化当前 backend 真相：
   `sslFreePascal`
   已是 shipped backend，
   不是未来态占位符

## Verification

```bash
bash -n tests/scripts/test_migration_guide_current_public_entrypoint_truth_contract.sh
bash tests/scripts/test_migration_guide_current_public_entrypoint_truth_contract.sh
git diff --check
```

## Expected Result

- 迁移文档继续解释 native-handle optional boundary
- 但不再继续教授不存在或不再推荐的 public factory entrypoint
- 普通入口 / 高级入口 / shipped backend 叙事
  与当前源码和活跃文档保持一致
