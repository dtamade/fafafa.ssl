# ServerName Migration Policy Plan

**Goal**
- 把 `ISSLContext.ServerName` 的角色从“仅有编译期 deprecated 提示”提升为仓内明确、可发现的迁移策略。
- 让 README、架构文档、接口注释都指向同一条推荐路径：per-connection SNI。

**Architecture**
- `ISSLClientConnection.SetServerName(...)` 是推荐入口，因为它把主机名绑定到单个连接，而不是共享 context。
- `ISSLContext.SetServerName(...)` / `GetServerName(...)` 仍保留，但只作为 backward-compatible fallback bridge。
- 这条 fallback 只影响后续 client connection 创建；server connection 不继承。
- precedence 保持既有合同：`connection override > context default > empty`。

**Files**
- Add: `docs/plans/2026-03-09-server-name-migration-policy.md`
- Add: `tests/scripts/test_server_name_migration_policy_contract.sh`
- Modify: `src/fafafa.ssl.base.pas`
- Modify: `README.md`
- Modify: `docs/reference/ARCHITECTURE.md`
- Update: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加迁移策略合同 RED。
2. 跑 RED，确认文档面还没有把迁移路径说清楚。
3. 在接口注释、README、架构文档中统一策略表述。
4. 跑合同验证，并回写 working memory。

**Expected Outputs**
- 新调用方能直接从 README / 架构文档看到推荐路径。
- `ISSLContext.ServerName` 的兼容边界被写清楚，而不是只靠零散 deprecation 字符串。
- 后续如果进一步收缩 deprecated surface，有稳定的策略文档可依赖。
