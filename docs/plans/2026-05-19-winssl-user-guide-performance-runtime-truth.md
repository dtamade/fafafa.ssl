# WinSSL User Guide Performance Runtime Truth

## Goal

把 `docs/guides/WINSSL_USER_GUIDE.md` 里的固定性能/稳定性快照从当前正文 truth 中移除，并把这部分内容重新锚回当前 WinSSL runtime baseline、Windows validation bundle 与 GitHub `windows-gate` 证据链。

## Architecture

这批保持 docs-only：

- 新增 focused shell contract，冻结 `WINSSL_USER_GUIDE` 的性能/runtime truth 边界
- 只修改 `docs/guides/WINSSL_USER_GUIDE.md`
- 不改 WinSSL 生产实现
- 不把 scope 扩到 `WINSSL_QUICKSTART.md`、`ARCHITECTURE.md` 或性能类总文档

## Files

- Add: `docs/plans/2026-05-19-winssl-user-guide-performance-runtime-truth.md`
- Add: `tests/scripts/test_winssl_user_guide_performance_truth_contract.sh`
- Modify: `docs/guides/WINSSL_USER_GUIDE.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

`WINSSL_USER_GUIDE.md` 当前仍把一张固定性能表直接写成正文 truth：

- `436.94 ms`
- `204.52 ms`
- `2.41 conn/s`
- `100%`
- `30/30 成功`

这类数字只代表某次历史运行，不代表当前长期 WinSSL runtime truth。

当前仓库已经有更准确的权威入口：

- `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
- `tests/windows/VALIDATION_BUNDLE.md`
- `.github/workflows/wave-b-b2-manual.yml`

因此用户指南应该保留“去哪里验证”和“如何判断当前 baseline 是否成立”，而不是继续内嵌过期跑数。

## Verification

```bash
bash -n tests/scripts/test_winssl_user_guide_performance_truth_contract.sh
bash tests/scripts/test_winssl_user_guide_performance_truth_contract.sh
bash tests/scripts/test_active_release_platform_truth_contract.sh
bash tests/scripts/test_active_connection_api_docs_truth_contract.sh
npx prettier --write docs/guides/WINSSL_USER_GUIDE.md
git diff --check
```

## Expected Outcome

- `WINSSL_USER_GUIDE.md` 继续保留：
  - 当前 WinSSL public surface 边界
  - 当前 session runtime truth
  - 当前权威入口
- 但不再把固定 latency / rate / stability 数字写成当前正文 truth
- 性能与稳定性段落改成：
  - 当前 runtime baseline 来源
  - 当前 Windows validation bundle
  - 当前成功标准
  - “以当前运行产物为准”的规则
