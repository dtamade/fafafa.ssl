# 2026-05-18 WinSSL Client Reconnect Truth Alignment

## Goal

把 WinSSL client-side session resumption 的实现与文档真相重新对齐到 Schannel 官方规则，修正把 `SCH_CRED_DISABLE_RECONNECTS` 直接挂到 client credential path 的错误，并明确 client reconnect 依赖 `target name + credential handle`，而不是 `SetSession(...)` 的 native handle 注入。

## Scope

- 不在本批承诺 WinSSL 已经稳定命中 native resumed handshake。
- 不重开 shared crash、session object serialization、capability/docs truth 等已收口 lane。
- 只收以下 truth drift：
  1. `SCH_CRED_DISABLE_RECONNECTS` 被错误地映射进 client credential path
  2. 活跃文档没有明确 Schannel client reconnect 的 canonical truth

## Files

- `src/fafafa.ssl.winssl.context.pas`
- `tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
- `docs/reference/API_REFERENCE.md`
- `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
- `docs/plans/2026-05-18-winssl-session-cache-runtime-flag-alignment.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- Schannel client reconnect / session cache lookup 的官方 truth 是：
  - same target name
  - same credential handle
  - same process
  - same logon session
- `SCH_CRED_DISABLE_RECONNECTS` 在 `SCHANNEL_CRED` 上是 server-only flag，不应直接挂到 client credential acquisition 路径。
- 因而当前 WinSSL `SetSession(...)` 更准确的 public truth 是：
  - compatibility metadata surface 仍存在
  - native reconnect 若发生，根因仍应是 Schannel auto-cache 命中，而不是手工注入 session object

## Steps

1. 先修 focused contract，避免继续把错误 truth 锁成绿色。
2. 最小修复 `src/fafafa.ssl.winssl.context.pas`：
   - `SCH_CRED_DISABLE_RECONNECTS` 仅保留在 server-side disable truth
   - client-side comment 明确回到 target-name/credential-handle truth
3. 更新活跃文档与记录文件。
4. 跑 focused contract、Win64 compile、`git diff --check`。

## Commands

```bash
bash -n tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh
bash tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh
mkdir -p tmp/winssl_client_reconnect_truth_win64
fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/winssl_client_reconnect_truth_win64 \
  -FEtmp/winssl_client_reconnect_truth_win64 \
  -otmp/winssl_client_reconnect_truth_win64/test_winssl_session_resumption.exe \
  tests/winssl/test_winssl_session_resumption.pas
git diff --check
```

## Execution Result

- PASS
- `SCH_CRED_DISABLE_RECONNECTS` 已从错误的 client credential path 收紧回 server-side truth
- WinSSL client reconnect 的 canonical truth 已重新固定为：
  - same `target name`
  - same context-level `credential handle`
  - same process / logon session
- `ISSLSessionResumption.SetSession(...)` 在 WinSSL 上当前明确记为 compatibility metadata surface，而不是 native session-handle injection 点
- focused verification：
  - `bash -n tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
  - `bash tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
  - `mkdir -p tmp/winssl_client_reconnect_truth_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_client_reconnect_truth_win64 -FEtmp/winssl_client_reconnect_truth_win64 -otmp/winssl_client_reconnect_truth_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
  - `git diff --check`
