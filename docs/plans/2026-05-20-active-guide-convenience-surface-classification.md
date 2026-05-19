# Active Guide Convenience Surface Classification（2026-05-20）

## Goal
- 把 active guides 对 `ISSLConnection` 这组 still-shipped convenience surface 的使用说明收回到当前 shipped truth：
  - `ReadString` / `WriteString` 仍保留，但更适合作为 `v1.x` 文本 convenience helper
  - `SetTimeout` / `SetBlocking` 仍保留，但新代码优先在 `TSSLConnectionBuilder` / `TSSLConnector` / `TSSLAcceptor` 上配置
  - 活跃指南如果继续展示这些方法，必须明确它们是 direct-connection 场景下的 convenience / override，而不是推荐主路径

## Why now
- `src/fafafa.ssl.base.pas`、`docs/reference/API_REFERENCE.md`、`docs/reference/INTERFACE_DESIGN_V2.md`、`docs/ARCHITECTURE.md`、`docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
  已经对这组方法的 current shipped truth 完成 classification 收口。
- 但 `docs/INTEGRATION_GUIDE.md`、`docs/guides/MIGRATION_GUIDE.md`、`docs/guides/USER_GUIDE.md`
  仍在高可见示例里直接使用：
  - `Conn.SetTimeout(...)`
  - `Conn.SetBlocking(...)`
  - `ReadString(...)`
  - `WriteString(...)`
- 如果这些指南不补推荐路径说明，后续审查者仍会把“活跃示例正在用”误读成“这就是当前主路径”，继续重开同一条 convenience/core 线路。

## Scope
- `docs/INTEGRATION_GUIDE.md`
- `docs/guides/MIGRATION_GUIDE.md`
- `docs/guides/USER_GUIDE.md`
- `tests/scripts/test_active_guide_convenience_surface_classification_contract.sh`
- `docs/plans/2026-05-20-active-guide-convenience-surface-classification.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals
- 不删除 `ReadString` / `WriteString` / `SetTimeout` / `SetBlocking`。
- 不修改 Pascal public source 或 backend runtime 实现。
- 不重做已经收口的 `API_REFERENCE` / `INTERFACE_DESIGN_V2` / `ARCHITECTURE` / runtime CI 线路。

## Approach
1. 先新增 focused shell contract，冻结：
   - `INTEGRATION_GUIDE` 若继续展示 `Conn.SetTimeout` / `Conn.SetBlocking`，必须明确：
     - builder / connector / acceptor-first
     - 连接侧调用只是 direct-connection 场景下的 convenience override
   - `MIGRATION_GUIDE` 若继续展示 `WriteString`，必须明确：
     - direct `ISSLConnection` 控制方式仍 shipped
     - 框架/transport 集成优先 `TSSLStream` 或 `Read` / `Write`
   - `USER_GUIDE` 若继续展示 `ReadString` / `WriteString`，必须明确：
     - 这是为了快速演示文本交互
     - 更复杂集成优先 `Read` / `Write` 或 `TSSLStream`
2. 先跑合同拿到 RED，确认问题仍真实存在。
3. 做最小文档修复，不扩大到别的 guide / archive。
4. 重新跑 focused 合同与 diff hygiene。
5. 更新 planning files 后提交推送。

## Commands
```bash
bash -n tests/scripts/test_active_guide_convenience_surface_classification_contract.sh
bash tests/scripts/test_active_guide_convenience_surface_classification_contract.sh
git diff --check
git status --short
```

## Expected Outputs
- 活跃指南不再把 still-shipped helper surface 误教成推荐主路径
- direct `ISSLConnection` 示例仍保留，但会明确标成 convenience / override 场景
- 未来如果这些高入口 guide 再回漂，focused contract 会立即报警

