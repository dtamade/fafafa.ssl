# ISSLConnection Text Owner-Path Adoption

## Goal

继续收口 `ISSLConnection`
core-too-fat 这条主残口，
但这批只切当前最小、真实可落地的一刀：

- 为 `ReadString`
- 为 `WriteString`

补上正式的 connection-side owner interface。

让当前 `v1.5.0` shipped truth 从：

- `ReadString` / `WriteString`
  只是留在 `ISSLConnection` core 上的 convenience text helper

推进到：

- core 上这两个方法继续兼容保留
- 但连接创建后也有正式 owner path
- `ISSLConnection` slimming
  不再只剩“分类说明”，而是继续有真实 public surface 演进

## Scope

- Add:
  - `docs/plans/2026-05-21-isslconnection-text-owner-path-adoption.md`
  - `tests/contract/test_isslconnection_text_owner_entry.pas`
  - `tests/scripts/test_isslconnection_text_owner_path_contract.sh`
- Update:
  - `src/fafafa.ssl.base.pas`
  - `src/fafafa.ssl.connection.base.pas`
  - `src/fafafa.ssl.pas`
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/INTERFACE_DESIGN_V2.md`
  - `docs/ARCHITECTURE.md`
  - `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
  - `tests/scripts/test_facade_optional_owner_surface_export_contract.sh`
  - `tests/contract/test_facade_optional_owner_surface_entry.pas`
  - `tests/scripts/test_architecture_optional_owner_surface_truth_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

不做：

- 不删除 `ISSLConnection` 上的 `ReadString` / `WriteString`
- 不把它们直接改成 compiler-deprecated
- 不扩大到新的 transport / framing API redesign
- 不重开 timeout / blocking 那条已完成 lane

## Architecture Truth

- `ReadString` / `WriteString`
  当前仍是 shipped public surface，
  也仍被 active docs / examples / tests 使用
- timeout / blocking
  已经有了
  `ISSLConnectionControl`
  owner path；
  因而当前更像 live residual 的 convenience slice
  已收窄到 text helpers
- 这批目标不是把文本 helper 宣布“应该立刻删掉”，
  而是先像 timeout / blocking 一样，
  给它们补出正式 owner path，
  同时保留 core convenience mirror

## Steps

1. 先新增 focused shell contract + compile/run proof，锁住新的 owner-path 目标。
2. 运行合同拿到 RED。
3. 最小修改 source：
   - `src/fafafa.ssl.base.pas`
   - `src/fafafa.ssl.connection.base.pas`
   - `src/fafafa.ssl.pas`
4. 同步 canonical docs / audit / adjacent owner contracts。
5. 重新跑 focused verification。
6. 更新台账并准备 commit / push。

## Verification

```bash
bash -n tests/scripts/test_isslconnection_text_owner_path_contract.sh
bash tests/scripts/test_isslconnection_text_owner_path_contract.sh
bash tests/scripts/test_facade_optional_owner_surface_export_contract.sh
bash tests/scripts/test_architecture_optional_owner_surface_truth_contract.sh
git diff --check
```

## Expected Outcome

- `ReadString` / `WriteString`
  当前不再只是 owner-less convenience methods
- `TBaseSSLConnection`
  显式承接新的 text owner interface
- 主门面、canonical docs、设计文档与审计报告
  一起切回新的 owner truth
- `ISSLConnection`
  主线向“更小 core + 更清晰 owner path”再次前进一步

