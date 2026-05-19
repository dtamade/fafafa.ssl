# WinSSL Native Probe Safe Query Path

## Goal

把 WinSSL session native probe 从“直接三参调用 `QueryContextAttributesW`”收紧为更安全的优先路径：

- 优先使用 `QueryContextAttributesExW(..., cbBuffer)` 查询 `SECPKG_ATTR_SESSION_INFO`
- 仅在拿不到 `QueryContextAttributesExW` 时回退到现有三参调用

## Scope

- 只作用于 `tests/winssl/test_winssl_session_resumption.pas` 的 isolated native probe lane
- 不改 canonical shared/public handshake path
- 不重开 `UpdateSessionReuseTruthFromContext(...)` 的 conservative shared-truth 设计

## Files

- Add: `docs/plans/2026-05-19-winssl-native-probe-safe-query-path.md`
- Add: `tests/scripts/test_winssl_native_probe_safe_query_contract.sh`
- Modify: `tests/winssl/test_winssl_session_resumption.pas`
- Modify: `tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
- Modify: `tests/scripts/test_winssl_session_info_probe_allowlist_contract.sh`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

最新 GitHub Windows run `26104446972` 已经把当前 native-probe 崩点缩到：

- `native_probe label=initial_handshake stage=before_query_context_attributes`
- `native_probe_worker exit_code=-1073741819`

这说明 runtime 真问题已经不在 workflow / artifact / public marker，而在 probe 调用链自身。

当前 probe 还是直接调用：

- `QueryContextAttributesW(LCtxtHandle, SECPKG_ATTR_SESSION_INFO, @LSessionInfo)`

官方同时提供了更适合这类结构查询的：

- `QueryContextAttributesExW(..., cbBuffer)`

因此这批先做一个最小、安全、只作用于 probe lane 的 source-side tightening。

## Steps

1. 新增 focused contract，先锁住“ExW 优先 + W 回退”的应有路径。
2. 给 proof program 加一个可缓存的 `QueryContextAttributesExW` resolver。
3. 在 native probe 中改为：
   - 优先 `QueryContextAttributesExW(..., SizeOf(SecPkgContext_SessionInfo))`
   - 否则回退 `QueryContextAttributesW(...)`
4. 跑 focused contract 与现有 runtime-truth contract。

## Commands

```bash
bash -n tests/scripts/test_winssl_native_probe_safe_query_contract.sh
bash tests/scripts/test_winssl_native_probe_safe_query_contract.sh
bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh
bash tests/scripts/test_winssl_session_info_probe_allowlist_contract.sh
git diff --check
```

## Expected Outcome

- repo-side probe source truth 从“只会三参 QueryContextAttributesW”收紧到“ExW 优先、W 回退”
- native-probe log 会额外记录本次实际使用的 query API
- 下一轮 Windows native-probe run 可以直接验证这条最小 source-side tightening 是否减少 `-1073741819`
