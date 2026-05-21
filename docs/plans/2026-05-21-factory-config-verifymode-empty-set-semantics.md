# 2026-05-21 Factory Config VerifyMode Empty-Set Semantics

## Goal

修复 `VerifyMode = []` 在 factory / library-default context creation path 上被错误当成“未设置”而跳过 `SetVerifyMode(...)` 的语义 bug，避免调用方明确禁用验证时，结果仍意外保留 backend 默认的 `sslVerifyPeer`。

## Scope

- 修改：
  - `src/fafafa.ssl.factory.pas`
  - `src/fafafa.ssl.openssl.backed.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `tests/contract/test_factory_config_verifymode_empty_set_entry.pas`
  - `tests/scripts/test_factory_config_verifymode_empty_set_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- 不修改：
  - 不新增新的 `TVerificationMode` builder/context typed seam
  - 不重构 direct-context `SetVerifyMode([])` / `SetVerifyMode([sslVerifyNone])` 全库语义
  - 不改变 `TSSLContextBuilder.WithVerifyNone` 当前 public surface

## Architecture Truth

- 当前各 backend/context 默认 verify mode 基线基本都是：
  - `[sslVerifyPeer]`
- 但当前 repo 多条 context-creation path 之前都只有在
  - `LConfig.VerifyMode <> []`
  时才会调用 `Result.SetVerifyMode(LConfig.VerifyMode)`
- 这会让调用方无法通过：
  - one-shot factory config 的 `VerifyMode := []`
  - library default config 的 `VerifyMode := []`
  显式禁用验证
- 当前 shipped public usage 面已经把 `[]` 广泛当成 direct-context “禁用验证”语义，因此 one-shot factory path 不能继续把它误读成“未设置”

## Steps

1. 新增 focused runtime contract，先以 RED 固定 one-shot factory path 的空集合 verify bug。
2. 最小修改 factory / library-default context creation path，让它们都能真实应用调用方提供的 `VerifyMode = []`。
3. 更新 `task_plan.md` / `findings.md` / `progress.md`。
4. 跑 focused contract 与 `git diff --check` 收口。

## Commands

```bash
bash -n tests/scripts/test_factory_config_verifymode_empty_set_contract.sh
bash tests/scripts/test_factory_config_verifymode_empty_set_contract.sh
git diff --check
```

## Expected Outcome

- `TSSLFactory.CreateContext(const AConfig)` 在 `VerifyMode := []` 时会真实落成 `GetVerifyMode = []`。
- `ISSLLibrary.SetDefaultConfig(... VerifyMode := [])` 后的 `CreateContext(...)` 也会真实落成 `GetVerifyMode = []`。
- factory / direct-library default-config path 与当前 direct-context/public docs 对 “空集合表示禁用验证” 的理解不再冲突。
- 后续 `TVerificationMode` 这条线可以在正确 runtime truth 之上继续判断是否需要 typed adoption。
