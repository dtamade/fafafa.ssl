# 2026-05-18 WinSSL Session Cache Runtime Flag Alignment

## Goal

把 `TWinSSLContext` 的 `SetSessionCacheMode(...)` / `SetOptions(...)` 从“只改 Pascal 字段”收紧为真正影响 Schannel credential acquisition 的实现，避免 WinSSL `session cache / session tickets` 接口继续停留在半实现状态。

## Scope

- 不在本批强行承诺 WinSSL 已经能稳定命中 native resumed handshake。
- 不重开已关闭的 shared crash / docs truth / capability truth lane。
- 只收以下实现缺口：
  1. `SetSessionCacheMode(...)` 改变后没有触发 credential rebuild
  2. `SetOptions(...)` 改变后没有触发 credential rebuild
  3. `EnsureCredentialsAcquired` 没有把 session-cache / session-ticket 选项映射到 Schannel `dwFlags`

## Files

- `src/fafafa.ssl.winssl.context.pas`
- `tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- 当前 `TWinSSLContext` 复用同一个 `CredHandle` 给多条 WinSSL 连接，这条 context-level handle 是 Schannel session caching / reconnect 的关键 runtime surface。
- 但源码当前仍存在两条实现缺口：
  - `SetSessionCacheMode(...)` / `SetOptions(...)` 只更新字段，没有要求 `FCredentialsNeedRebuild := True`
  - `EnsureCredentialsAcquired` 只设置了 `SCH_CRED_NO_DEFAULT_CREDS` 和 `SCH_CRED_MANUAL_CRED_VALIDATION`，没有把 `session cache / tickets` 语义映射到 `SCH_CRED_DISABLE_RECONNECTS`
- 因而当前 public interface 虽然暴露了这些配置，但 runtime credential path 还没有真正响应它们。

## Steps

1. 新增 focused source contract，先以 RED 固定当前实现缺口。
2. 最小修改 `src/fafafa.ssl.winssl.context.pas`：
   - 让 session cache / options 改动触发 credential rebuild
   - 让 `EnsureCredentialsAcquired` 在 cache disabled 或 tickets disabled 时带上 `SCH_CRED_DISABLE_RECONNECTS`
3. 跑 focused contract、Win64 compile、`git diff --check`。

## Commands

```bash
bash -n tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh
bash tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh
mkdir -p tmp/winssl_session_cache_runtime_flag_win64
fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/winssl_session_cache_runtime_flag_win64 \
  -FEtmp/winssl_session_cache_runtime_flag_win64 \
  -otmp/winssl_session_cache_runtime_flag_win64/test_winssl_session_resumption.exe \
  tests/winssl/test_winssl_session_resumption.pas
git diff --check
```

## Execution Result

- PASS
- `SetSessionCacheMode(...)` 与 `SetOptions(...)` 的 session/ticket 相关变化现在都会触发 credential rebuild
- `EnsureCredentialsAcquired` 现在会把 cache disabled 或 tickets disabled 的 truth 映射到 `SCH_CRED_DISABLE_RECONNECTS`
- focused verification：
  - `bash -n tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
  - `bash tests/scripts/test_winssl_session_cache_runtime_flag_contract.sh`
  - `mkdir -p tmp/winssl_session_cache_runtime_flag_win64 && fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/winssl_session_cache_runtime_flag_win64 -FEtmp/winssl_session_cache_runtime_flag_win64 -otmp/winssl_session_cache_runtime_flag_win64/test_winssl_session_resumption.exe tests/winssl/test_winssl_session_resumption.pas`
  - `git diff --check`
