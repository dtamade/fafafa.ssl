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
- 但 GitHub Windows live run `26042437486` 证明：
  - 这条 public-handle probe 方式在 broader suite 默认开启时并不安全
  - dedicated test 在首个 public signal 之后、第一条 `native_probe` marker 之前就会以 `-1073741819` 退出
  - 因而 broader suite 默认 lane 只能把 native probe 维持为 opt-in evidence，不得默认开启
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
- RED:
  - GitHub Windows live run `26042437486` 证明当前 probe 调用方式仍然不安全：
    - `WinSSL Session Resumption Truth` 在 `initial handshake must not report reuse: PASS` 后立刻以 `exit_code=-1073741819` 退出
    - artifact 中连第一条 `native_probe` marker 都没有来得及写出
- FOLLOW-UP:
  - broader suite 默认 lane 先把 native probe 改成 opt-in
  - 默认记录 `reason=disabled_by_default`
  - 未来若要继续追 native observation，必须先设计更安全的 WinSSL-specific probe seam，而不是再次默认开启 public-handle probe
- GREEN:
  - 本地 follow-up 已落地：
    - `FAFAFA_WINSSL_ENABLE_NATIVE_PROBE` 变成显式 opt-in 开关
    - 未开启时 dedicated proof 会继续输出 `native_probe ... reason=disabled_by_default`
    - summary 新增 `native_probe_enabled=...`
  - focused source contract / Win64 cross-target compile / `git diff --check` 再次通过
- PENDING:
  - 还需要重新触发 GitHub Windows runner，确认 broader suite 默认 lane 已恢复绿色，并把 `disabled_by_default` summary 真实写进 artifact
