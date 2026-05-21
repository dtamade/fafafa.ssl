# 2026-05-21 Server Helper VerifyMode Default Alignment

## Goal

把 `CreateServerContext(...)` / `QuickServer(...)` 这条高入口 server helper 的默认 verify 语义收回到和 `CreateDefaultConfig(sslCtxServer)`、`CreateContext(sslCtxServer, ...)`、以及 builder `BuildServer` 一致，避免 helper 再静默切到 no-verify。

## Scope

- 修改：
  - `src/fafafa.ssl.factory.pas`
  - `docs/reference/API_REFERENCE.md`
  - `tests/contract/test_server_helper_verifymode_default_entry.pas`
  - `tests/scripts/test_server_helper_verifymode_default_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- 不修改：
  - 不重构 server verify 的更大语义模型
  - 不改变 `.WithVerifyNone` / `SetVerifyMode([])` 当前 explicit no-verify surface
  - 不扩到 archive/history 文档

## Architecture Truth

- 当前 fresh default-config surface：
  - `CreateDefaultConfig(sslCtxServer)`
  仍返回：
  - `VerifyMode = [sslVerifyPeer]`
- `TSSLFactory.CreateContext(sslCtxServer, ...)`
  会套用 library default config，
  因而当前 server raw context baseline 也走：
  - `sslVerifyPeer`
- builder `BuildServer`
  当前也会把默认
  - `FVerifyMode = [sslVerifyPeer]`
  真实写到 runtime context
- 但 `TSSLFactory.CreateServerContext(...)`
  之前还额外硬编码：
  - `Result.SetVerifyMode([sslVerifyNone])`
- 这会让 `QuickServer(...)`
  与其它 server 高入口默认语义分叉成：
  - raw server path / builder path = verify peer baseline
  - convenience helper path = silent no-verify

## Steps

1. 新增 focused contract，先以 RED 固定 helper 仍在 silent no-verify 的行为。
2. 最小修改 `src/fafafa.ssl.factory.pas`，移除 helper 的隐式 no-verify override。
3. 在 `API_REFERENCE` helper 分类处补一条 active truth 说明：
   - `CreateServerContext(...)` / `QuickServer(...)` 不再隐式切换到 no-verify
   - 如需 non-mTLS/no-verify，调用方必须显式配置
4. 更新 `task_plan.md` / `findings.md` / `progress.md`。
5. 跑 focused contract 与 `git diff --check` 收口。

## Commands

```bash
bash -n tests/scripts/test_server_helper_verifymode_default_contract.sh
bash tests/scripts/test_server_helper_verifymode_default_contract.sh
git diff --check
```

## Expected Outcome

- `CreateServerContext(...)` / `QuickServer(...)` 的 verify baseline 不再和其它 server 高入口分叉。
- 调用方若确实要做 non-mTLS/no-verify server，必须显式写：
  - direct-context/config: `SetVerifyMode([])` / `VerifyMode := []`
  - builder: `.WithVerifyNone`
- server helper 的接口设计语义更可预期，不再藏着 implicit policy override。
