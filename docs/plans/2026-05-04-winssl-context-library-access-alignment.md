# WinSSL Context And Library Access Alignment Plan

**Goal:** 收口 WinSSL connection 对 context/library 的内部访问路径，不再把 `FContext: ISSLContext` 和 `ISSLLibrary` 直接硬转成 `TWinSSLContext` / `TWinSSLLibrary`，改成显式内部 access interface，消掉 Win64 交叉编译中的不安全类型告警。

**Architecture:** 这批不改 WinSSL 握手、证书验证、SNI/ALPN/session 行为，只修内部协作边界。先用一个 focused source contract 锁住 `src/fafafa.ssl.winssl.connection.pas` 不再做 `TWinSSLContext(FContext)` / `TWinSSLLibrary(...)` 类硬转。然后在 `src/fafafa.ssl.winssl.context.pas` 和 `src/fafafa.ssl.winssl.lib.pas` 各补一个内部 access interface，让 connection 通过 `Supports(...)` 拿到 verify callback、info callback、CA store、library statistics updater。最后跑 source contract、Win64 交叉编译和仓库门禁，确认行为面没有被误伤。

**Files:**
- Add: `tests/scripts/test_winssl_connection_context_access_contract.sh`
- Modify: `src/fafafa.ssl.winssl.context.pas`
- Modify: `src/fafafa.ssl.winssl.lib.pas`
- Modify: `src/fafafa.ssl.winssl.connection.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - prove unsafe class-cast access is still present

Run:

```bash
bash -n tests/scripts/test_winssl_connection_context_access_contract.sh
bash tests/scripts/test_winssl_connection_context_access_contract.sh
```

Expected RED:
- `winssl.connection.pas` 仍出现 `TWinSSLContext(FContext)`
- 或仍出现 `TWinSSLLibrary(TWinSSLContext(FContext).GetLibrary)`

## Task 2: GREEN - replace hard casts with internal access interfaces

Change:
- `src/fafafa.ssl.winssl.context.pas`
  - 新增 `IWinSSLContextAccess`
  - `TWinSSLContext` 显式实现该内部接口
- `src/fafafa.ssl.winssl.lib.pas`
  - 新增 `IWinSSLLibraryStatsAccess`
  - `TWinSSLLibrary` 显式实现该内部接口
- `src/fafafa.ssl.winssl.connection.pas`
  - 用 `Supports(FContext, IWinSSLContextAccess, ...)`
  - 用 `Supports(LContextAccess.GetLibrary, IWinSSLLibraryStatsAccess, ...)`
  - 删除对 `TWinSSLContext` / `TWinSSLLibrary` 的直接类硬转

Constraints:
- 不新增 public backend feature
- 不改 `ISSLContext` / `ISSLLibrary` 的对外接口
- 不把这批扩大到 WinSSL runtime handshake 审计

## Task 3: Verification

Run:

```bash
bash -n tests/scripts/test_winssl_connection_context_access_contract.sh
bash tests/scripts/test_winssl_connection_context_access_contract.sh
mkdir -p tmp/winssl_session_mgmt_win64
fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_session_mgmt_win64 -FEtmp/winssl_session_mgmt_win64 -otmp/winssl_session_mgmt_win64/test_winssl_session_management.exe tests/winssl/test_winssl_session_management.pas
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- `winssl.connection.pas` 不再做 `ISSLContext` / `ISSLLibrary` 到具体类的硬转
- connection 通过内部 access interface 拿到 verify/info/CA-store/stats 能力
- Win64 交叉编译不再出现 `ISSLContext`/`ISSLLibrary` 与 `TWinSSLContext`/`TWinSSLLibrary` 不相关的告警
- focused source contract、compile gate、minimal CI gate 全绿
- 台账同步到新的 WinSSL 内部协作真相
