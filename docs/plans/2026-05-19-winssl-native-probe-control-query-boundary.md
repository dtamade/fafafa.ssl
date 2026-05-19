# WinSSL Native Probe Control Query Boundary

## Goal

在 isolated native probe 里加入一条已知更稳定的 control query：

- `QueryContextAttributesW(..., SECPKG_ATTR_CONNECTION_INFO, ...)`

用它来区分两类问题：

1. extracted native handle 路径本身就不稳定
2. 只有 `SECPKG_ATTR_SESSION_INFO` / `QueryContextAttributesExW` 这条 attribute-specific 路径会崩

## Fresh Runtime Evidence

最新 Windows run `26107307586` 已经给出新的关键事实：

- `stage=query_resolver module=sspicli.dll symbol=QueryContextAttributesExW resolved=true`
- `stage=query_api api=query_context_attributes_exw`
- `native_probe_worker exit_code=-1073741819`

这说明问题已经不在 resolver，而是在真正进入 `QueryContextAttributesExW(..., SECPKG_ATTR_SESSION_INFO, ...)` 之后。

## Scope

- 仅修改 `tests/winssl/test_winssl_session_resumption.pas` 的 isolated native probe
- 新增一个 focused shell contract
- 不改 canonical shared/public path
- 不改 broader suite 其它测试语义

## Files

- Add: `docs/plans/2026-05-19-winssl-native-probe-control-query-boundary.md`
- Add: `tests/scripts/test_winssl_native_probe_control_query_contract.sh`
- Modify: `tests/winssl/test_winssl_session_resumption.pas`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Steps

1. 新增 focused contract，锁住 control-query helper 和 marker。
2. 在 native probe 里先跑：
   - `SECPKG_ATTR_CONNECTION_INFO`
3. 为 control query 增加 marker：
   - `stage=before_control_query`
   - `stage=after_control_query`
   - `stage=control_query_failed`
4. 跑 focused contract、相关回归 contract、Win64 compile、`git diff --check`。
5. 推送后复跑 Windows native-probe manual lane。

## Expected Outcome

- 如果 control query 也崩：
  - 问题更偏向 extracted handle path / context lifetime
- 如果 control query 通过，而 session-info probe 继续崩：
  - 问题更偏向 `SECPKG_ATTR_SESSION_INFO` attribute-specific provider/runtime boundary
