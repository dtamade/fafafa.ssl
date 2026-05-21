# 2026-05-21 Builder Merge Empty VerifyMode Clear Semantics

## Goal

修复 `ISSLContextBuilder.Merge(...)` 在 source snapshot 显式携带 `verify_modes = []` 时仍吞掉“清空 verify mode”语义的问题，避免 merged builder 继续保留旧的 `[sslVerifyPeer]`，与 source builder 已经表达出的 no-verify 状态分叉。

## Scope

- 修改：
  - `src/fafafa.ssl.context.builder.pas`
  - `tests/contract/test_builder_merge_empty_verifymode_entry.pas`
  - `tests/scripts/test_builder_merge_empty_verifymode_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- 不修改：
  - 不新增 `TVerificationMode` typed builder seam
  - 不重构 `WithVerifyNone` 当前 public surface
  - 不扩到 active docs 的 `[]` / `[sslVerifyNone]` 文案统一

## Architecture Truth

- `ExportToJSON(...)` 当前总会导出 `verify_modes` 字段。
- `ImportFromJSON(...)` / `ImportFromINI(...)` 当前都可以把 builder 状态导成：
  - `FVerifyMode = []`
- 但 `Merge(...)` 当前仍写成：
  - `if LVerify.Count > 0 then`
    才覆盖 `FVerifyMode`
- 这意味着 source snapshot 即便明确携带空数组 `[]`，merge 后也无法清空 target 原有的 `[sslVerifyPeer]`。

## Steps

1. 新增 focused contract，先以 RED 固定“source builder 的空 verify_modes 在 merge 后被吞掉”的行为。
2. 最小修改 `src/fafafa.ssl.context.builder.pas`，让 `Merge(...)` 在字段存在时就按 source snapshot 覆盖 `FVerifyMode`，包括空集合。
3. 更新 `task_plan.md` / `findings.md` / `progress.md`。
4. 跑 focused contract 与 `git diff --check` 收口。

## Commands

```bash
bash -n tests/scripts/test_builder_merge_empty_verifymode_contract.sh
bash tests/scripts/test_builder_merge_empty_verifymode_contract.sh
git diff --check
```

## Expected Outcome

- source builder 如果显式表达：
  - `verify_modes = []`
  merge 后 target builder 也会真实落成：
  - `GetVerifyMode = []`
- merged builder validation 会与 runtime no-verify 真相一致，给出禁用验证 warning。
- builder verify 线上的 import / merge / validation 语义进一步拉平。
