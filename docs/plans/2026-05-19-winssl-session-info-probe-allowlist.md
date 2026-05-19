# 2026-05-19 WinSSL Session-Info Probe Allowlist

## Goal
把刚刚暴露出来的 WinSSL session-info probe 漂移，收口成一个 repo 级防回归合同：`SECPKG_ATTR_SESSION_INFO` 的直接 query 只能留在当前明确受控的位置，避免未来再在其他 WinSSL 文件里悄悄长出未隔离 probe。

## Scope
- `tests/scripts/test_winssl_session_info_probe_allowlist_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：
- 不修改生产实现
- 不扩张 probe 行为
- 不改变现有 dedicated proof / canonical helper 边界

## Why This Batch
这次 `src/fafafa.ssl.winssl.session.pas` 的静态漂移之所以能混进来，核心原因不是修法复杂，而是 repo 里还没有一个“session-info probe 只能出现在哪些文件”的 focused guard。

## Planned Changes
1. 新增 allowlist contract。
2. 锁住当前允许的受控位置：
   - `src/fafafa.ssl.winssl.connection.pas`
   - `tests/winssl/test_winssl_session_resumption.pas`
3. 显式禁止已知不该再碰这条 query 的 residual 文件：
   - `src/fafafa.ssl.winssl.session.pas`

## Verification
```bash
bash -n tests/scripts/test_winssl_session_info_probe_allowlist_contract.sh
bash tests/scripts/test_winssl_session_info_probe_allowlist_contract.sh
git diff --check
```

## Expected Outcome
- 未来若又有新的未隔离 `SECPKG_ATTR_SESSION_INFO` probe 混进 repo，会立刻被 source contract 打红
