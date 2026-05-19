# 2026-05-19 WinSSL Session Shim Safe Fallback

## Goal
把 `src/fafafa.ssl.winssl.session.pas` 真正收回“compatibility shim”边界：不再在 shim 内直接调用 `QueryContextAttributesW(..., SECPKG_ATTR_SESSION_INFO, ...)`，而是回到与 canonical safe path 一致的保守 fallback session metadata。

## Scope
- `src/fafafa.ssl.winssl.session.pas`
- `tests/scripts/test_winssl_session_shim_safe_fallback_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：
- 不修改 `src/fafafa.ssl.winssl.connection.pas` shared handshake logic
- 不更改 public `TWinSSLSession` API surface
- 不把这条 shim 改造成新的 probe lane

## Why This Batch
当前 repo 已经把 WinSSL `SECPKG_ATTR_SESSION_INFO` probe 降格成：

- opt-in
- isolated worker
- experimental evidence

但 `src/fafafa.ssl.winssl.session.pas` 这个兼容 shim 里仍然保留着一条未经 quarantine 的直接 `QueryContextAttributesW(...)` 路径。  
这和当前 canonical truth 明显冲突，而且一旦外部代码调用这个 shim，理论上会把同一类 runtime risk 又带回来。

## Planned Changes
1. 先写 focused RED contract，锁住 shim 不得再直接触碰：
   - `QueryContextAttributesW`
   - `SECPKG_ATTR_SESSION_INFO`
2. 最小修改 `src/fafafa.ssl.winssl.session.pas`：
   - 去掉 risky query 和相关 helper残留
   - 改成保守 fallback session id
   - 继续保持 `SetSessionMetadata(..., False)` conservative truth
3. 复跑 focused contract 与既有 truth-source contract。

## Verification
```bash
bash -n tests/scripts/test_winssl_session_shim_safe_fallback_contract.sh
bash tests/scripts/test_winssl_session_shim_safe_fallback_contract.sh
bash tests/scripts/test_winssl_session_truth_source_contract.sh
git diff --check
```

## Expected Outcome
- `winssl.session.pas` 不再私自维护一条 risky Schannel session-info probe
- compatibility shim 与当前 canonical safe path 重新一致
