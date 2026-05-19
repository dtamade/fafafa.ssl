# 2026-05-19 Migration Guide Active Truth

## Goal

继续沿着 interface/backend completeness 主线推进，收口高入口迁移指南里仍在教授旧版本叙事、旧单元名、旧 helper 名称的问题：

- `docs/guides/MIGRATION_GUIDE.md`
  - 顶部仍停在 `v0.8`
  - 仍把 `v0.7/v0.8` 当作当前 active 迁移主线
  - 仍使用已经不存在的 `fafafa.ssl.abstract.intf`
  - 仍把旧企业 helper 名称写成：
    - `IsFipsModeEnabled`
    - `GetEnterpriseTrustedRoots`
    - `GetGroupPolicies`
  - 仍把 OpenSSL 低层 error helper 混成 generic public migration surface

## Scope

- 只处理 `docs/guides/MIGRATION_GUIDE.md`
- 用 focused shell contract 锁住：
  - 当前迁移真相入口
  - 当前公开单元/门面/连接语义
  - WinSSL enterprise helper 当前命名
  - OpenSSL low-level helper 的边界说明
- 不修改 runtime 实现
- 不扩到其它 guides/reference/history 文档重写

## Files

- `docs/guides/MIGRATION_GUIDE.md`
- `tests/scripts/test_migration_guide_active_truth_contract.sh`
- `docs/plans/2026-05-19-migration-guide-active-truth.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- 当前迁移真相以：
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.pas`
  - `src/fafafa.ssl.tls.pas`
  - `docs/reference/API_REFERENCE.md`
  为准
- 当前推荐 public migration path：
  - `fafafa.ssl`
  - `fafafa.ssl.context.builder`
  - `TSSLFactory`
  - `TSSLConnector`
  - `TSSLStream`
- client SNI / hostname 当前推荐走：
  - `TSSLConnector.ConnectSocket(..., ServerName)`
  - 或 `ISSLClientConnection.SetServerName(...)`
- `TSSLConfig.ServerName` / `ISSLContext.SetServerName(...)` / `TSSLContextBuilder.WithSNI(...)`
  当前都是 compatibility-only，不是迁移主路径
- WinSSL enterprise helper 当前 shipped 命名是：
  - `IsFIPSEnabled`
  - `GetTrustedRoots`
  - `GetAllPolicies`
- `GetFriendlyErrorMessage(...)` / `GetOpenSSLErrorCategory(...)`
  当前来自 `fafafa.ssl.openssl.api.err`，属于 OpenSSL-specific low-level helper，不是 generic public migration contract

## Steps

1. 补 focused shell contract，让 migration guide 的旧版本/旧单元/旧 helper 先 RED。
2. 把 `MIGRATION_GUIDE` 改回当前 active migration truth。
3. 同步台账，避免后续再把这份旧迁移叙事当成当前主线。
4. 跑轻量验证并提交。

## Commands

```bash
bash -n tests/scripts/test_migration_guide_active_truth_contract.sh
bash tests/scripts/test_migration_guide_active_truth_contract.sh
git diff --check
```

## Expected Result

- `MIGRATION_GUIDE` 不再把 `v0.8` 和不存在的单元名当成当前迁移入口
- 迁移示例重新对齐当前公开门面、per-connection SNI 与 transport-first 语义
- WinSSL enterprise / OpenSSL low-level helper 的边界重新说清楚
