# README Helper Surface Truth Resync

## Goal

收口
`README.md`
对
helper surface
的过宽表述，
避免首页把
“deprecated 顶层 helper aliases/functions 已移除”
误说成
“helper API 都已经移除”，
从而掩盖当前仍然 shipped 的：

- `TSSLHelper`
- `QuickServer(...)`
- `CreateOCSPClient(...)`
- `CreateCRLManager(...)`

这些 convenience helper
仍然存在、
但不属于
TLS bootstrap
主入口
的真实状态。

## Scope

本批只处理：

- README 首页 / 版本历史 wording
- focused helper-surface contract
- 台账同步

本批不做：

- 不改 `src/fafafa.ssl.pas` public surface
- 不改 helper runtime 行为
- 不重开 helper 删除/保留范围讨论

## Why This Batch

当前 canonical truth
已经很清楚：

- `RELEASE_NOTES_V1.5.0.md`
  明确写了：
  - 移除的是
    deprecated global helper aliases/functions
  - 显式
    `TSSLHelper`
    类
    仍然保留
- `docs/reference/API_REFERENCE.md`
  也明确把：
  - `TSSLHelper`
  - `QuickServer(...)`
  - `CreateOCSPClient(...)`
  - `CreateCRLManager(...)`
  归为
  convenience / certificate-tooling helper
  而不是
  TLS bootstrap
  主入口

但 `README.md`
当前仍写：

- `deprecated helper API 已移除`
- `移除 deprecated helper API，统一迁移到 TSSLFactory.*`

这会让首页读者误解为：

- helper surface 已整体消失
- `TSSLHelper` / `QuickServer` / `CreateOCSPClient`
  不再是 shipped API

## Files

- Add: `docs/plans/2026-05-21-readme-helper-surface-truth-resync.md`
- Update: `README.md`
- Update: `tests/scripts/test_helper_surface_classification_truth_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps

1. 先补 focused contract：
   - README 必须明确
     removed 的是
     deprecated 顶层 helper aliases/functions
   - README 必须明确
     `TSSLHelper` /
     `QuickServer(...)` /
     `CreateOCSPClient(...)` /
     `CreateCRLManager(...)`
     仍然保留
     但不代替主入口
   - README 不得再使用
     `deprecated helper API 已移除`
     这种过宽表述
2. 运行 focused contract，拿到 RED。
3. 用最小文字改动修正 README。
4. 重跑 focused contract 与 diff hygiene。

## Verification

1. `bash -n tests/scripts/test_helper_surface_classification_truth_contract.sh`
2. `bash tests/scripts/test_helper_surface_classification_truth_contract.sh`
3. `git diff --check`
4. `git status --short`

## Expected Outcome

- README 首页与版本历史
  不再把
  helper surface
  说成“整体移除”
- 首页会明确：
  - TLS bootstrap 主入口
  - 仍保留的 convenience helpers
  之间的边界
