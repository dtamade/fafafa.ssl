# WinSSL Session Truth-Source Collapse Plan

**Goal:** 收敛 WinSSL session 的重复 truth source：`src/fafafa.ssl.winssl.connection.pas` 保留唯一真实实现，`src/fafafa.ssl.winssl.session.pas` 退化为兼容 shim，并去掉 session 级假 native-handle surface。

**Architecture:** 这批是 source-contract + compatibility-shim 收口，不预设 Linux 上可以做 WinSSL runtime proof。先用脚本契约锁住结构边界，再做最小生产改动，最后用仓库 compile/minimal gate 证明没有误伤 Linux 核心面。

**Files:**
- Modify: `src/fafafa.ssl.winssl.connection.pas`
- Modify: `src/fafafa.ssl.winssl.session.pas`
- Modify: `tests/winssl/test_winssl_session_management.pas`
- Add: `tests/scripts/test_winssl_session_truth_source_contract.sh`
- Update: `docs/reference/WINSSL_DESIGN.md`
- Update: `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED/Audit - prove WinSSL session truth-source drift

Run:

```bash
bash -n tests/scripts/test_winssl_session_truth_source_contract.sh
bash tests/scripts/test_winssl_session_truth_source_contract.sh
```

Contract goals:
- `winssl.connection.pas` 的 `TWinSSLSession` 不再实现 `ISSLNativeHandleAccess`
- `winssl.connection.pas` 不再保留 `GetNativeHandle` / `GetBackendType` / `IsNativeHandleValid` 这组三件 session 级方法
- `winssl.session.pas` 不再保留独立 `TInterfacedObject` session 实现，而是兼容 shim
- WinSSL session 测试不再把 `ISSLSession` 当成有 `GetNativeHandle` 的旧接口
- 关键文档不再把 `winssl.session.pas` 写成唯一会话实现 truth source

## Task 2: GREEN - collapse to one real implementation

Change:
- `src/fafafa.ssl.winssl.connection.pas`
  - `TWinSSLSession = class(TInterfacedObject, ISSLSession)`
  - 删除 session 级 `ISSLNativeHandleAccess` 假 surface
- `src/fafafa.ssl.winssl.session.pas`
  - 改成基于 `winssl.connection.TWinSSLSession` 的 compatibility shim
- `tests/winssl/test_winssl_session_management.pas`
  - 改为断言 WinSSL session 不暴露 `ISSLNativeHandleAccess`
- 文档同步到当前真相

Constraints:
- 不重开 WinSSL 握手/证书/ALPN 逻辑
- 不把 Linux 结果写成 Windows runtime 已证实
- 不删除 `winssl.session.pas` 文件本身，优先保留兼容入口

## Task 3: Verification

Run:

```bash
bash -n tests/scripts/test_winssl_session_truth_source_contract.sh
bash tests/scripts/test_winssl_session_truth_source_contract.sh
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- WinSSL session 只剩一个真实实现 truth source
- `winssl.session.pas` 不再携带平行实现
- WinSSL session 的假 native-handle surface 已收紧
- focused source contract、compile gate、minimal CI gate 全绿
- 台账和文档同步到当前真相
