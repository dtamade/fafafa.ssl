# 2026-05-18 WinSSL Native Probe Evidence Lane

## Goal

把 WinSSL dedicated session-resumption proof lane 再往前推进一层：在不重开 shared crash / production reconnect 实现的前提下，把 public conservative truth 和 native Schannel observation 分开记录，让 GitHub Windows artifact 能直接回答“public `observed_reuse=false` 时，native probe 到底有没有看到 `SSL_SESSION_RECONNECT`”。

## Scope

- 不修改 `src/fafafa.ssl.winssl.connection.pas` 的共享 reconnect 逻辑。
- 不把 `SetSession(...)` 重新解释成 WinSSL native reconnect 注入点。
- 不重开：
  - shared `SECPKG_ATTR_SESSION_INFO` crash
  - docs/capability truth drift
  - `SCH_CRED_DISABLE_RECONNECTS` client-side 误映射
- 只扩 dedicated proof/program/contracts/records。

## Files

- `tests/winssl/test_winssl_session_resumption.pas`
- `tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- WinSSL 当前 shared/public truth 仍然是保守模型：
  - `ISSLConnection.IsSessionReused` 不会在 canonical shared path 上直接依赖 live `SECPKG_ATTR_SESSION_INFO`
  - `observed_reuse=false` 只能说明 public surface 没有宣称已复用，不能单独证明 native Schannel reconnect 一定未命中
- dedicated proof lane 可以单独拥有实验性 native observation：
  - `ISSLNativeHandleAccess.GetNativeHandle`
  - `PCtxtHandle`
  - `QueryContextAttributesW(..., SECPKG_ATTR_SESSION_INFO, ...)`
  - `dwFlags and SSL_SESSION_RECONNECT`
- 因而同一条 Windows runtime evidence 里应该同时记录两类真相：
  - public truth：`observed_reuse`
  - native observation：`native_observed_reuse` / `native_probe_succeeded`

## Steps

1. 在 `test_winssl_session_resumption.pas` 中增加 dedicated `TryQueryNativeSessionReuse(...)` helper。
2. 在初始握手和每次 same-context attempt 后都输出 `native_probe ...` markers。
3. 在 summary marker 中显式区分：
   - `observed_reuse`
   - `native_observed_reuse`
   - `native_probe_succeeded`
   - `require_native_reuse`
4. 扩 `test_winssl_session_resumption_runtime_truth_contract.sh`，锁住新的 native probe evidence surface。
5. 做 focused Win64 cross-target compile 与 `git diff --check`。
6. push 后触发 `wave-b-b2-manual.yml`，直接读取 Windows artifact 里的 `native_probe` / `summary` markers。

## Commands

```bash
bash -n tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh
bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh
mkdir -p tmp/winssl_native_probe_truth_win64
fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/winssl_native_probe_truth_win64 \
  -FEtmp/winssl_native_probe_truth_win64 \
  -otmp/winssl_native_probe_truth_win64/test_winssl_session_resumption.exe \
  tests/winssl/test_winssl_session_resumption.pas
git diff --check
gh workflow run wave-b-b2-manual.yml --ref master -f run_id=<custom-id>
```

## Execution Result

- GREEN:
  - dedicated proof program 已增加 native probe helper 与 markers
  - focused source contract 已扩到 native probe evidence surface
  - Win64 cross-target compile 通过
  - `git diff --check` 通过
- PENDING:
  - GitHub Windows runner 还需要给出 live artifact，回答：
    - `native_probe_succeeded=true|false`
    - `native_observed_reuse=true|false`
  - 在这之前，不再把 public `observed_reuse=false` 当成 native reconnect 未命中的铁证
