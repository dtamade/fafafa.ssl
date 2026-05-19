# 2026-05-19 Active Release / Platform Truth Sweep

## Goal

收掉当前高入口活跃文档里的 release/platform/WinSSL 真相漂移，确保：

- `RELEASE_NOTES.md` 不再把 `v1.0.0` 历史快照冒充当前稳定发布入口
- `PLATFORM_SUPPORT.md` 回到当前 `v1.5.0` 已发布 + 跨平台 runtime workflow 已绿的真实口径
- WinSSL 用户/部署文档不再把 `session resumption / tickets` 写成已完整 runtime-proven 的能力
- 活跃文档不再保留 `yourusername` / `your-repo` / `your.email@example.com` 这类错误占位入口

## Scope

- 只修当前高入口活跃文档：
  - `docs/RELEASE_NOTES.md`
  - `docs/PLATFORM_SUPPORT.md`
  - `docs/guides/WINSSL_USER_GUIDE.md`
  - `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
- 只新增一个 focused contract
- 不重写历史发布内容本身；历史内容可以保留，但必须显式降级成历史快照
- 不重开 WinSSL 实现线或 runtime 调查线

## Files

- `docs/RELEASE_NOTES.md`
- `docs/PLATFORM_SUPPORT.md`
- `docs/guides/WINSSL_USER_GUIDE.md`
- `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`
- `docs/plans/2026-05-19-active-release-platform-truth-sweep.md`
- `tests/scripts/test_active_release_platform_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Truth Sources

- 当前发布真相：
  - `docs/ROADMAP.md`
  - `docs/test_reports/RELEASE_READINESS_V1.5.0.md`
- 当前 WinSSL 真相：
  - `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
  - `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
  - `docs/reference/WINSSL_DESIGN.md`

## Architecture Truth

- `v1.5.0` 已发布，`RELEASE_READINESS_V1.5.0.md` 是当前发布权威入口
- 跨平台 release/runtime truth 已在当前 release workflow 上闭环，不应继续把 macOS 写成“待验证”
- WinSSL 零依赖客户端 baseline 已有当前证据，但 `session resumption / tickets` 仍是 experimental public surface
- `observed_reuse=false` / `session_configured=true` 是当前 WinSSL session truth 的关键摘要，不应再被 `100% 完成 / 生产就绪 / 完全支持` 文案覆盖掉

## Steps

1. 新增 focused contract，对活跃文档里的 stale release/platform/WinSSL wording 和 placeholder 入口先做 RED
2. 最小修改 4 份活跃文档，使其重新指向当前 release / WinSSL truth source
3. 跑 focused contract 与 `git diff --check`

## Commands

```bash
bash -n tests/scripts/test_active_release_platform_truth_contract.sh
bash tests/scripts/test_active_release_platform_truth_contract.sh
git diff --check
```

## Expected Result

- `RELEASE_NOTES.md` 顶部明确当前稳定版本是 `v1.5.0`，`v1.0.0` 只作为历史快照保留
- `PLATFORM_SUPPORT.md` 不再保留 `97.5% / 99%+ / macOS 待验证 / WinSSL 100% 完成` 这类过时口径
- WinSSL 用户/部署文档显式回到当前 `session resumption / tickets` 的 experimental truth
- 活跃文档中的 placeholder repo/contact 入口被全部清掉
