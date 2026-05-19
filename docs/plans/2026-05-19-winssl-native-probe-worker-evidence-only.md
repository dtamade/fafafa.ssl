# WinSSL Native Probe Worker Evidence-Only

## Goal

把已知会在 `SECPKG_ATTR_SESSION_INFO` 上触发 Windows runner/provider 崩溃的 isolated native probe worker，
从默认 hard-fail 调整为 evidence-only：

- 默认：
  - 记录 worker exit / marker / probe_succeeded
  - 不让 broader runtime suite 因 worker 非零退出而失败
- 仅在显式要求 native reuse truth 时：
  - 继续把 worker 非零退出视为失败

## Why Now

最新两条 Windows run 已经把问题边界钉死：

- run `26107307586`
  - `QueryContextAttributesExW` 已从 `sspicli.dll` 解析成功
  - 但在 `query_context_attributes_exw` 调用后崩溃
- run `26108237632`
  - 同一 extracted native handle 上的 control query
    - `SECPKG_ATTR_CONNECTION_INFO`
    成功返回
  - 说明 handle path 本身可用
  - 失败点已收窄为：
    - `SECPKG_ATTR_SESSION_INFO` 的 attribute-specific provider/runtime boundary

因此，继续让这条 investigatory probe 默认拖垮 broader suite 的收益已经很低。

## Scope

- 仅修改 `tests/winssl/test_winssl_session_resumption.pas`
- 新增一个 focused shell contract
- 不改变 public/canonical session truth
- 不改变显式 `FAFAFA_WINSSL_REQUIRE_NATIVE_REUSE=1` 时的严格语义

## Files

- Add: `docs/plans/2026-05-19-winssl-native-probe-worker-evidence-only.md`
- Add: `tests/scripts/test_winssl_native_probe_worker_evidence_only_contract.sh`
- Modify: `tests/winssl/test_winssl_session_resumption.pas`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Steps

1. 新增 focused contract，锁住：
   - `LRequireNativeReuse` 严格门
   - 默认 evidence-only check
2. 调整 parent-side worker check：
   - `LRequireNativeReuse=true` 时仍要求 exit code = 0
   - 否则仅记录 evidence
3. 跑 focused contract、相关回归 contract、Win64 compile、`git diff --check`。
4. 推送后复跑 Windows native-probe manual lane，验证 suite 是否恢复为 PASS 并保留 native marker。

## Expected Outcome

- 默认 native-probe 调查 lane 不再因为已知 attribute-specific crash 而把 Windows overall 打红
- `FAFAFA_WINSSL_REQUIRE_NATIVE_REUSE=1` 仍能保留严格失败语义
