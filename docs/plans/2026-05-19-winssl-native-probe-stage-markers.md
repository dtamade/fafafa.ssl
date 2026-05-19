# 2026-05-19 WinSSL Native Probe Stage Markers

## Goal
把 `tests/winssl/test_winssl_session_resumption.pas` 的 opt-in native probe 再收窄一层：在 `TryQueryNativeSessionReuse(...)` 内部补齐分阶段 `native_probe` markers，让下一轮 GitHub Windows artifact 能直接告诉我们崩在：

- `Supports(...)` 之前或之后
- `GetNativeHandle` 之前或之后
- `QueryContextAttributesW(...)` 调用之前、失败返回之后，还是异常路径

## Scope
- `tests/winssl/test_winssl_session_resumption.pas`
- `tests/scripts/test_winssl_native_probe_stage_markers_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：
- 不修改 `src/fafafa.ssl.winssl.connection.pas`
- 不修改 shared reconnect/session-info 实现
- 不把 native probe 从 opt-in experimental lane 提升成 canonical truth

## Why This Batch
当前 worker quarantine 已经把 WinSSL native probe 的失败收口成可控事实：

- parent 不再直接被打崩
- `native_probe_worker exit_code=... last_marker=...` 已经稳定写进 artifact

但截至 run `26070488337`，`last_marker` 仍只停在：

- `native_probe label=initial_handshake pending=true mode=isolated_worker`

也就是说，现有 evidence 只能说明“崩在 probe body 里”，还不足以回答到底是：

- owner-surface cast
- native handle 读取
- Schannel `QueryContextAttributesW(...)`

哪一步先出的问题。

## Planned Changes
1. 先写 focused RED source contract：
   - 锁住 `TryQueryNativeSessionReuse(...)` 接收 `label`
   - 锁住阶段性 `native_probe ... stage=...` markers
2. 最小修改测试程序：
   - probe body 开头写 `stage=before_supports`
   - 通过 owner-surface 后写 `stage=after_supports`
   - 调 `GetNativeHandle` 前后写 marker
   - 调 `QueryContextAttributesW(...)` 前写 marker
   - 成功/失败/异常路径继续写 marker
3. 复跑 source contract、现有 worker quarantine contract、runtime-truth contract 与 Win64 cross compile。
4. 若这批通过，再基于新 commit 触发下一轮 GitHub Windows manual run。

## Verification
```bash
bash -n tests/scripts/test_winssl_native_probe_stage_markers_contract.sh
bash tests/scripts/test_winssl_native_probe_stage_markers_contract.sh
bash tests/scripts/test_winssl_native_probe_worker_quarantine_contract.sh
bash tests/scripts/test_winssl_session_resumption_runtime_truth_contract.sh
mkdir -p tmp/winssl_native_probe_stage_markers_win64
fpc -Twin64 -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/winssl_native_probe_stage_markers_win64 \
  -FEtmp/winssl_native_probe_stage_markers_win64 \
  -otmp/winssl_native_probe_stage_markers_win64/test_winssl_session_resumption.exe \
  tests/winssl/test_winssl_session_resumption.pas
git diff --check
```

## Expected Outcome
- 下一轮 worker crash 时，`last_marker` 不会再只停在 `pending=true`
- Windows artifact 将能直接区分：
  - crash 发生在 owner-surface/cast 之前
  - 还是确实发生在 `QueryContextAttributesW(...)` 调用边界
