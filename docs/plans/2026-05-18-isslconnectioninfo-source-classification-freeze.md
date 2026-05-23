# ISSLConnectionInfo Source Classification Freeze

## Goal

把 `ISSLConnectionInfo` 这组 mirrors 的 Stage-A 分类直接写进 source 注释层，并用 focused source contract 锁住当前真相，避免后续 source-facing slimming 再次依赖会漂移的口头约定。

## Scope

本批只处理 source comments、focused contract 与台账：

- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.connection.base.pas`
- `tests/scripts/test_isslconnectioninfo_source_classification_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改 public signature
- 不做 compiler-level deprecation
- 不修改 backend connection 实现

## Why This Batch

现在：

- 设计文档已经把 Stage-A demotion map 写清楚
- active docs 也已经开始优先走 `ISSLConnectionInfo`

但 source 本身还没有清楚表达：

- `GetConnectionInfo`
- `GetContext`
- `GetSelectedALPNProtocol`
- `GetStateString`

这 4 个方法在当前 `v1.x` 里是 `compatibility-core duplicates`，且下一阶段的默认 demotion target 是 `ISSLConnectionInfo`。

## Planned Changes

1. 在 `src/fafafa.ssl.base.pas` 的 4 个核心 getter 注释中补 `v1.x compatibility-core mirror / Stage-A demotion target` 说明。
2. 在 `ISSLConnectionInfo` 接口注释中明确它承接这 4 个 mirrors。
3. 在 `TBaseSSLConnection` 类注释中明确它当前通过 `ISSLConnectionInfo` 承接这组 duplicates。
4. 新增 focused source contract，防止这些 source-facing classification 说明回流丢失。

## Verification

```bash
bash -n tests/scripts/test_isslconnectioninfo_source_classification_contract.sh
bash tests/scripts/test_isslconnectioninfo_source_classification_contract.sh
git diff --check
```

## Expected Outcome

- source、设计文档、active docs 对 `ISSLConnectionInfo` mirror group 的描述三者对齐
- 下一批可以在更稳定的 source-facing truth 上决定第一条真正的实现切片

## Execution Result

- PASS.
- Revalidated `tests/scripts/test_isslconnectioninfo_source_classification_contract.sh` with `bash -n` and `bash`.
- Source-facing classification for the four `ISSLConnectionInfo` mirrors remains aligned with the Stage-A roadmap.
