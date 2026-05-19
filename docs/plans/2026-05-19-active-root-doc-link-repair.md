# 2026-05-19 Active Root Doc Link Repair

## Goal

修复当前最常作为入口被打开的活跃文档中的 broken links，避免继续把 backend / platform / WinSSL 用户指引导向旧路径或不存在的文件。

## Scope

- 只修 5 个高入口活跃文档：
  - `docs/PLATFORM_SUPPORT.md`
  - `docs/RELEASE_NOTES.md`
  - `docs/TOOLS.md`
  - `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
  - `docs/guides/WINSSL_USER_GUIDE.md`
- 不重写大段叙事
- 不触碰 archive 文档

## Files

- `docs/PLATFORM_SUPPORT.md`
- `docs/RELEASE_NOTES.md`
- `docs/TOOLS.md`
- `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
- `docs/guides/WINSSL_USER_GUIDE.md`
- `tests/scripts/test_active_root_doc_link_repair_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- 根入口文档必须优先指向当前仍存在的活跃 docs/guides/reference/test_reports 文件
- WinSSL guide 不应继续把用户导向已消失的 `WINSSL_HTTPS_TEST_REPORT.md` 或旧 phase reports
- 当前 release/platform/navigation truth 应优先走 `guides/`、`reference/`、`ROADMAP.md`、`test_reports/WINSSL_BACKEND_STATUS_REPORT.md`

## Steps

1. 新增 focused contract，对这 5 个活跃入口文档做 link-target truth 检查
2. 最小修正旧路径、占位路径和已失效 WinSSL report links
3. 跑 focused contract 与 `git diff --check`

## Commands

```bash
bash -n tests/scripts/test_active_root_doc_link_repair_contract.sh
bash tests/scripts/test_active_root_doc_link_repair_contract.sh
git diff --check
```

## Expected Result

- 这 5 个活跃入口文档不再包含已失效的文档链接
- 根入口统一指向当前真实存在的 guides/reference/test_reports 页面
