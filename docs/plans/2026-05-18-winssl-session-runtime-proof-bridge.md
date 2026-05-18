# 2026-05-18 WinSSL Session Runtime Proof Bridge

## Goal

把 WinSSL 的 session-resumption lane 从“只有设计意图和零散测试文件”推进到“源码已读取 Schannel session truth，broader suite 已有 dedicated proof lane，Windows CI 只差 live run”。

## Scope

- 不重开已经闭环的 Windows runtime evidence capture。
- 不在本批强行承诺 `SetSession(...)` 已经完整驱动 WinSSL native resume。
- 先修三件更基础也更确定的真问题：
  1. client `DoConnect` 成功后没有保存 session metadata
  2. canonical `winssl.connection` 没有读取 `SECPKG_ATTR_SESSION_INFO`
  3. broader `tests/run_winssl_tests.ps1` 没有真正跑 dedicated session-resumption proof lane

## Files

- `src/fafafa.ssl.winssl.base.pas`
- `src/fafafa.ssl.winssl.connection.pas`
- `tests/winssl/test_winssl_session_resumption.lpi`
- `tests/winssl/test_winssl_session_resumption.pas`
- `tests/run_winssl_tests.ps1`
- `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md`
- `tests/windows/VALIDATION_BUNDLE.md`
- `tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh`
- `tests/scripts/test_winssl_windows_runtime_project_target_contract.sh`
- `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `IsSessionReused` 的 WinSSL truth source 不应是内存标志臆测，而应来自 Schannel `QueryContextAttributesW(..., SECPKG_ATTR_SESSION_INFO, ...)`。
- `SaveSessionAfterHandshake` 必须在 client handshake 成功后也执行，否则 `GetSession()` / resumed metadata 只在 server path 有保存机会。
- broader suite 若声称覆盖 `session resumption / tickets`，就必须真的跑 dedicated `test_winssl_session_resumption.lpi`，并把结果写进 runtime artifact。

## Steps

1. 在 canonical `winssl.connection` 中补 `SECPKG_ATTR_SESSION_INFO` 真值读取。
2. 让 client `DoConnect` / generic `PerformHandshake` 与 server path 一样在成功后保存 session metadata。
3. 重写 `test_winssl_session_resumption.pas`，聚焦：
   - owner/core/info/perf 四路 reuse truth 一致性
   - same-context repeated handshake evidence
   - stable `[WINSSL-SESSION-RESUME]` markers
4. 把该测试接入 `tests/run_winssl_tests.ps1` broader suite，并提升成 `[WINSSL-RUNTIME] session_resumption ...` evidence markers。
5. 先做本地 source contract + Win64 focused cross-target compile。
6. 提交推送后，再用 GitHub Windows runner 取真实 live proof。

## Commands

```bash
bash -n tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh
bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh
bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh
bash tests/scripts/test_winssl_runtime_suite_markers_contract.sh
bash tests/scripts/test_workflow_winssl_tests_truth_contract.sh
bash tests/scripts/test_session_reused_semantic_truth_contract.sh
mkdir -p tmp/winssl_session_resumption_win64
fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/winssl_session_resumption_win64 \
  -FEtmp/winssl_session_resumption_win64 \
  -otmp/winssl_session_resumption_win64/test_winssl_session_resumption.exe \
  tests/winssl/test_winssl_session_resumption.pas
git diff --check
```

## Execution Result

- GREEN:
  - 新增 dedicated source contract 通过
  - Windows validation docs contracts 继续通过
  - `session_reused_semantic_truth_contract` 继续通过
  - focused Win64 cross-target compile 通过
- RED:
  - GitHub Actions live run `26033545656` 证明 broader suite 并未卡在 runtime proof 本身，而是 `test_winssl_session_resumption.lpi` 仍硬编码 `TargetOS=linux`
  - Windows runner 因此把 dedicated session-resumption lane 当成 Linux 工程去编，直接在 `Run broader WinSSL runtime suite` 的 compile phase 失败
- FOLLOW-UP:
  - 去掉 `test_winssl_session_resumption.lpi` 的硬编码 Linux target
  - 把现有 `test_winssl_windows_runtime_project_target_contract.sh` 扩到该新 `.lpi`
  - push 后重新触发 `wave-b-b2-manual.yml`，再看 live runtime artifact 给出的 `observed_reuse=true|false`
- GREEN:
  - GitHub Actions live rerun `26034303732` 已证明 `.lpi` target 漂移修复有效：
    - `test_winssl_session_resumption.lpi` 在 Windows broader suite compile phase 成功通过
    - `windows-gate` 已重新推进到真正的 runtime phase
  - 同一 rerun 中 `macos-gate` 也已转绿，当前 workflow 只剩 Windows broader suite blocker
- RED:
  - 新的 Windows first hard blocker 已压缩到 shared runtime helper：
    - `UpdateSessionReuseTruthFromContext(...)` 在 `Run broader WinSSL runtime suite` 中触发 `EAccessViolation`
    - 它不仅打倒 `WinSSL Session Resumption Truth`，也连带打倒 `Integration Multi` / `Performance Benchmark` / `HTTPS Client`
  - 这说明问题不在某个专项测试，而在“握手后读取 `SECPKG_ATTR_SESSION_INFO` 的共享实现”本身
- FOLLOW-UP:
  - 把 `TryGetCurrentSessionInfo(...)` / `UpdateSessionReuseTruthFromContext(...)` 降成 best-effort observation
  - 保留 `SECPKG_ATTR_SESSION_INFO` 作为 truth source，但任何异常都只能回落成 `session_id='' / reused=false`
  - 重新触发 `wave-b-b2-manual.yml`，确认 broader suite 不再因 session-info observation 崩溃
- PENDING:
  - GitHub Windows runner live run 尚未刷新
  - 是否稳定观测到 `observed_reuse=true` 仍待 Windows artifact 给出结论
