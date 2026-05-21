# Dependencies Current Backend And Public Entrypoint Truth Alignment

## Goal

修复
`docs/DEPENDENCIES.md`
里的 backend-specific 依赖真相与旧 public helper 漂移，
让依赖文档重新对齐当前
`v1.5.0`
源码 / 平台 / FAQ / release truth：

- 编译器版本要求回到当前 shipped baseline
- runtime 依赖按 backend 区分，
  不再静默漏掉
  `FreePascal`
  的无外部 SSL 依赖路径
- 切换 backend 示例回到当前
  `TSSLFactory.GetLibraryInstance(...)`
  真相
- WinSSL 兼容信息与底部链接/版本尾注不再漂移

这批不改 runtime，
只做：

- active docs truth repair
- 一个静态 contract，
  防止依赖文档再漂回旧 helper / 旧 backend 依赖叙事
- 账本同步

## Why This Batch

继续沿高入口活跃文档往下扫时，
`docs/DEPENDENCIES.md`
命中了 4 组真实 drift：

- 编译依赖仍写
  `Free Pascal >= 3.3.1`
  但当前活跃文档/README 真相是
  `3.2.0+`
- Windows / Linux / macOS 的 runtime 依赖叙事
  几乎都只围绕
  `WinSSL`
  /
  `OpenSSL`
  展开，
  静默漏掉了
  `FreePascal`
  的无外部 SSL 依赖路径
- FAQ 末尾“如何切换后端”
  仍在教学
  `CreateSSLLibrary(...)`
- WinSSL 版本兼容表里还写
  `Windows 10 (20348+)`
  这种旧口径，
  与当前源码 / WinSSL 文档的
  `18362+ / 1903+`
  真相不一致

## Scope

- Add:
  - `docs/plans/2026-05-21-dependencies-current-backend-and-public-entrypoint-truth-alignment.md`
  - `tests/scripts/test_dependencies_current_backend_and_entrypoint_truth_contract.sh`
- Update:
  - `docs/DEPENDENCIES.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. 把 FPC baseline 改回
   `3.2.0+`
   /
   推荐
   `3.2.2+`
2. 在 Windows / Linux / macOS runtime 依赖段
   补回
   `FreePascal`
   backend 的 zero-external-SSL truth
3. 把“如何切换后端”示例统一改成：
   `TSSLFactory.GetLibraryInstance(...)`
4. 把 WinSSL 兼容表改回当前口径：
   `Windows 10 (>= 18362)`
5. 修正文档底部链接与版本尾注

## Verification

```bash
bash -n tests/scripts/test_dependencies_current_backend_and_entrypoint_truth_contract.sh
bash tests/scripts/test_dependencies_current_backend_and_entrypoint_truth_contract.sh
git diff --check
```

## Expected Result

- 依赖文档不再发布旧 helper 或过窄 backend 依赖叙事
- `FreePascal`
  重新进入 runtime dependency 视图
- FPC / WinSSL version truth
  与当前活跃文档保持一致
