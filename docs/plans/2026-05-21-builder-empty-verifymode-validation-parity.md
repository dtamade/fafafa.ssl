# 2026-05-21 Builder Empty VerifyMode Validation Parity

## Goal

修复 `ISSLContextBuilder.Validate*` 在 `FVerifyMode = []` 时漏报“禁用证书验证”警告的语义裂缝，避免导入后的 builder 已经在 runtime 上 no-verify，但 validation 仍假装它是安全默认。

## Scope

- 修改：
  - `src/fafafa.ssl.context.builder.pas`
  - `tests/contract/test_builder_empty_verifymode_validation_entry.pas`
  - `tests/scripts/test_builder_empty_verifymode_validation_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- 不修改：
  - 不新增 `TVerificationMode` typed builder seam
  - 不统一所有活跃文档里的 `[]` / `[sslVerifyNone]` 写法
  - 不重构 `WithVerifyNone` 当前 public surface

## Architecture Truth

- 当前 runtime 上，`FVerifyMode = []` 与 `FVerifyMode = [sslVerifyNone]` 都会导致“不做 peer verification”。
- 但 `ISSLContextBuilder.Validate*` 之前只在：
  - `sslVerifyNone in ABuilder.FVerifyMode`
  时才发出：
  - `Certificate verification is disabled - insecure for production`
- 同时 `ImportFromJSON(...)` / `ImportFromINI(...)` 都可以把 `verify_modes` 导入成空集合 `[]`。
- 因而导入后的 builder 可能已经 runtime no-verify，却绕过当前 validation warning。

## Steps

1. 新增 focused runtime contract，先以 RED 固定“empty verify_modes import 后 validation 漏警告”的问题。
2. 最小修改 `src/fafafa.ssl.context.builder.pas`，让 validation 以“未启用 `sslVerifyPeer`”作为 no-verify 判据。
3. 更新 `task_plan.md` / `findings.md` / `progress.md`。
4. 跑 focused contract 与 `git diff --check` 收口。

## Commands

```bash
bash -n tests/scripts/test_builder_empty_verifymode_validation_contract.sh
bash tests/scripts/test_builder_empty_verifymode_validation_contract.sh
git diff --check
```

## Expected Outcome

- `ImportFromJSON(...)` / `ImportFromINI(...)` 导入 `verify_modes = []` 后，`ValidateClient` 会给出 no-verify 警告。
- 导入后的 builder validation 不再与当前 runtime no-verify 真相分叉。
- verify 线后续就能在更一致的 builder/runtime 语义上继续收口。
