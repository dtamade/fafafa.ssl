# ISSLConnectionInfo Mirror Demotion Migration Map

## Goal

在不修改生产实现的前提下，把 `ISSLConnectionInfo` 这组 connection-info mirrors 的迁移地图冻结下来，避免 `ISSLConnection` 的下一条 slimming 路线继续被过时或互相冲突的设计文档带偏。

## Scope

本批只处理设计文档与 focused contract：

- `docs/reference/INTERFACE_DESIGN_V2.md`
- `tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不直接修改 `ISSLConnection` public signature
- 不改 `src/` 下任何 backend connection 实现
- 不重跑重型 compile-all / minimal-ci gates

## Source Of Truth

- active source truth:
  - `src/fafafa.ssl.base.pas:1524-1544`
  - `src/fafafa.ssl.connection.base.pas:52-63`
  - `src/fafafa.ssl.connection.base.pas:237-268`
- active compatibility-core note:
  - `docs/reference/API_REFERENCE.md:482-491`
- current contract proof:
  - `tests/contract/test_backend_contract.pas:1762-1850`
- current design/audit route:
  - `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md:20-37`
  - `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md:103-107`

## Why This Batch Comes Next

- `ISSLConnection` / `ISSLSession` 的 active-doc truth freeze 已完成。
- 连接层历史 execution receipt 也已补齐，不再需要继续停留在旧 plan closeout。
- 当前最明显的设计漂移已经转移到 `docs/reference/INTERFACE_DESIGN_V2.md`：
  - 层次图仍漏掉 `ISSLConnectionInfo`
  - 仍保留不存在的 `ISSLAdvanced`
  - 实现类示例没有列出 `ISSLConnectionInfo`
  - 迁移表把 `GetConnectionInfo` 错归给 `ISSLDiagnostics`
  - 还过早把 `GetStateString` / `GetContext` / `GetSelectedALPNProtocol` 直接写死成其它路线

如果这里不先收口，下一批真正动 `compatibility-core slimming` 时就会失去统一的设计锚点。

## Planned Changes

1. 在 `INTERFACE_DESIGN_V2.md` 中补出 `ISSLConnectionInfo` 扩展接口。
2. 把层次图改成当前真实的 optional extension family，不再使用 `ISSLAdvanced` 这个空壳名。
3. 明确 `GetConnectionInfo` / `GetContext` / `GetSelectedALPNProtocol` / `GetStateString` 的 Stage-A demotion target：
   - 先 demote 到 `ISSLConnectionInfo`
   - 后续是否进一步收窄，再单独决策
4. 把实现类示例与 migration snippet 改成和当前路线一致。
5. 新增 focused shell contract，防止错误 owner / 旧空壳名回流。

## Verification

```bash
bash -n tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh
bash tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh
git diff --check
```

## Expected Outcome

- `ISSLConnectionInfo` 这组 mirror 的 owner 和迁移顺序变成单一真相
- `INTERFACE_DESIGN_V2.md` 不再给后续 `ISSLConnection` slimming 提供错误锚点
- 下一批可以直接进入 source-facing slimming prep，而不是继续修设计文档冲突
