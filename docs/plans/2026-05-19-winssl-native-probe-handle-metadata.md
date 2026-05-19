# 2026-05-19 WinSSL Native Probe Handle Metadata

## Goal
在已经确认 crash 落到 `QueryContextAttributesW(...)` 调用边界之后，再补一层纯静态安全的 handle metadata evidence，避免当前 `handle_nil=false` 这种过于粗糙的 marker 继续误导后续判断。

## Scope
- `tests/winssl/test_winssl_session_resumption.pas`
- `tests/scripts/test_winssl_native_probe_handle_metadata_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：
- 不修改 `src/fafafa.ssl.winssl.connection.pas`
- 不触碰 shared reconnect / session-info canonical path
- 不让 probe 直接转成生产逻辑

## Why This Batch
run `26071361489` 现在已经把崩溃点收窄到：

- `stage=after_get_native_handle handle_nil=false`
- `stage=before_query_context_attributes`

这说明：

- `Supports(...)` 没先炸
- `GetNativeHandle` 也没先炸

但还不能回答：

- `GetNativeHandle` 返回的到底是不是一个“内容有效”的 `CtxtHandle`
- `IsNativeHandleValid` 会不会在 probe 时已经是 `false`
- `dwLower` / `dwUpper` 在这一步到底长什么样

## Planned Changes
1. 先写 focused RED 合同，锁住 `handle_metadata` marker。
2. 在 probe helper 里补充：
   - `GetBackendType`
   - `IsNativeHandleValid`
   - `dwLower` / `dwUpper`
3. 复跑 source contract、stage-marker contract、worker quarantine contract 与 Win64 cross compile。
4. 若本地继续绿，再考虑派发下一轮 Windows run。

## Verification
```bash
bash -n tests/scripts/test_winssl_native_probe_handle_metadata_contract.sh
bash tests/scripts/test_winssl_native_probe_handle_metadata_contract.sh
bash tests/scripts/test_winssl_native_probe_stage_markers_contract.sh
bash tests/scripts/test_winssl_native_probe_worker_quarantine_contract.sh
mkdir -p tmp/winssl_native_probe_handle_metadata_win64
fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/winssl_native_probe_handle_metadata_win64 \
  -FEtmp/winssl_native_probe_handle_metadata_win64 \
  -otmp/winssl_native_probe_handle_metadata_win64/test_winssl_session_resumption.exe \
  tests/winssl/test_winssl_session_resumption.pas
git diff --check
```

## Expected Outcome
- native probe 在真正调用 `QueryContextAttributesW(...)` 前，会额外留下：
  - backend 类型
  - native handle validity
  - `dwLower` / `dwUpper`
- 下一轮 Windows artifact 将能区分“无效句柄导致的 query 崩溃”与“有效句柄上调用 `SECPKG_ATTR_SESSION_INFO` 也会崩”
