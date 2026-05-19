# WinSSL Native Probe Worker Quarantine

## Goal

把 WinSSL opt-in native probe 从“主测试进程里直接调用、崩了就整条 proof 中断”的状态，收口成一个更安全的专用调查层：

- broader suite 主进程继续产出 public reuse truth
- 显式 opt-in 的 native probe 改由隔离子进程执行
- 即使子进程仍以 `-1073741819` 崩掉，主进程也要把：
  - worker exit code
  - 最后一个已观测 marker
  - native probe summary truth
  完整写进 artifact，而不是在第一条 `native_probe` marker 之前直接失踪

## Scope

- `tests/winssl/test_winssl_session_resumption.pas`
- `tests/scripts/test_winssl_native_probe_worker_quarantine_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改 `src/fafafa.ssl.winssl.connection.pas` 的 shared reconnect / session-info 实现
- 不把 native probe 升格回 canonical shared path
- 不把 opt-in native probe 伪装成已经稳定可用

## Why This Batch

当前 live run `26068984446` 已明确证明：

- opt-in native probe 输入已经真正生效
- dedicated proof 先成功写出了 `initial_handshake` 的 public marker
- 但随后在第一条 `native_probe` evidence 之前，以 `exit_code=-1073741819` 退出

也就是说，现在缺的不是“有没有失败证据”，而是：

- 如何让 risky native probe 失败时仍留下完整、可复盘的 worker-level truth

## Planned Changes

1. 新增 focused source contract：
   - 锁住 dedicated proof 通过 `TProcess` / `ParamStr(0)` 隔离 native probe worker
   - 锁住 child-mode guard
   - 锁住 `pending=true` pre-probe markers
   - 锁住 controlled `native_probe_worker exit_code=...` reporting
2. 修改 `test_winssl_session_resumption.pas`：
   - 主进程保留 public truth
   - opt-in native probe 改由子进程执行
   - 子进程继续直接调用 risky native probe
   - 父进程负责转发 native probe markers、汇总 worker exit truth，并做 controlled failure
3. 保持 summary surface：
   - `native_probe_enabled`
   - `native_observed_reuse`
   - `native_probe_succeeded`
   继续和 public truth 分离

## Verification

```bash
bash -n tests/scripts/test_winssl_native_probe_worker_quarantine_contract.sh
bash tests/scripts/test_winssl_native_probe_worker_quarantine_contract.sh
bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh
mkdir -p tmp/winssl_native_probe_worker_quarantine_win64
fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/winssl_native_probe_worker_quarantine_win64 \
  -FEtmp/winssl_native_probe_worker_quarantine_win64 \
  -otmp/winssl_native_probe_worker_quarantine_win64/test_winssl_session_resumption.exe \
  tests/winssl/test_winssl_session_resumption.pas
git diff --check
git push origin master
gh workflow run wave-b-b2-manual.yml \
  --ref master \
  -f run_id=<custom-id> \
  -f strict_closure=false \
  -f winssl_session_host=www.google.com \
  -f winssl_enable_native_probe=true
```

## Expected Outcome

- opt-in native probe 即使继续失败，也会留下 worker-level controlled evidence
- broader runtime transcript 不再在第一条 risky probe 之前直接中断成半截
- 后续再追 WinSSL-specific safe probe seam 时，会有更完整的 Windows artifact 作为真相基线
