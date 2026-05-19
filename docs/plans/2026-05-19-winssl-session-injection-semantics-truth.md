# WinSSL Session Injection Semantics Truth

## Goal

把 WinSSL session-resumption 的一个关键实现真相同步到高入口文档：

- `ISSLSessionResumption.SetSession(...)` 在 WinSSL 上当前更接近 compatibility metadata surface
- shared client reconnect 仍主要依赖 Schannel 的自动 cache key
  - `target name`
  - `credential handle`

避免调用方把“接口存在”误读成“显式 native session 注入语义已经完整”。

## Architecture

这批以 source+docs truth 对齐为主：

- 新增 focused shell contract，冻结 WinSSL session-injection semantics truth
- 在 source 旁边补一条简短注释，避免后续误读
- 收紧最容易被拿来做决策的高入口文档
- 不改 WinSSL 握手实现行为

## Files

- Add: `docs/plans/2026-05-19-winssl-session-injection-semantics-truth.md`
- Add: `tests/scripts/test_winssl_session_injection_semantics_truth_contract.sh`
- Modify: `src/fafafa.ssl.winssl.connection.pas`
- Modify: `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- Modify: `docs/guides/WINSSL_USER_GUIDE.md`
- Modify: `docs/BACKEND_SELECTION_GUIDE.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

当前实现和文档之间还存在一条容易让人高估 WinSSL session 语义完整度的 gap：

- source 事实：
  - `DoSetSession(...)` 当前只保存 `FCurrentSession`
  - `ClientHandshake` 没有把 caller-supplied session 作为 native handle
    注入 `InitializeSecurityContextW`
  - shared reconnect 仍主要依赖 Schannel 的自动 cache key
- 但高入口示例如果只写：
  - `Resumption2.SetSession(Session);`
  就很容易让调用方以为 WinSSL 已经具备和 OpenSSL 类似的显式 session restore
  语义

## Verification

```bash
bash -n tests/scripts/test_winssl_session_injection_semantics_truth_contract.sh
bash tests/scripts/test_winssl_session_injection_semantics_truth_contract.sh
bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh
npx prettier --write docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md docs/guides/WINSSL_USER_GUIDE.md docs/BACKEND_SELECTION_GUIDE.md
git diff --check
```

## Expected Outcome

- source 旁边有一句明确注释，说明 WinSSL `SetSession(...)` 的当前语义边界
- WinSSL capability matrix 与 user guide 会显式讲清：
  - `SetSession(...)` 当前更接近 compatibility metadata surface
  - reconnect 仍主要跟 `target name + credential handle` 绑定
- backend selection guide 在 Windows 场景下不再让人忽略这条 caveat
