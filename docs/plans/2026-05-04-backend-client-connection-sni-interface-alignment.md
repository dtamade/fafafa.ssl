# Backend Client-Connection SNI Interface Alignment Plan

**Goal:** 收口 `MbedTLS` / `WolfSSL` 在 client-side SNI public interface 上的 contract 漂移，让 capability、connection interface 和现有实现保持同一套真相源。

**Architecture:** 这批不新增 SNI 行为、不重开 hostname verify 或 handshake 流程，也不触碰 `OpenSSL` / `FreePascal` 已经稳定的 client-connection 路径。只做两件事：
- `tests/contract/test_backend_contract.pas`：新增跨后端 RED/GREEN 合约，锁住 `SupportsSNI=True => connection 必须暴露 ISSLClientConnection`。
- `src/fafafa.ssl.mbedtls.connection.pas` / `src/fafafa.ssl.wolfssl.connection.pas`：把现有 `SetServerName/GetServerName` 公开挂进 `ISSLClientConnection`。

**Files:**
- Modify: `tests/contract/test_backend_contract.pas`
- Modify: `src/fafafa.ssl.mbedtls.connection.pas`
- Modify: `src/fafafa.ssl.wolfssl.connection.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Status

- 状态：已完成
- 真实 RED：
  - `WolfSSL` / `MbedTLS` capability 都宣称 `SupportsSNI=True`
  - 但 `Supports(Connection, ISSLClientConnection, ...)` 为 `False`
- 真实 GREEN：
  - 两个后端现在都公开实现 `ISSLClientConnection`
  - focused contract、compile gate、minimal CI gate 全部通过

## Task 1: RED - prove the interface drift

Run:

```bash
fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract
./tmp/test_backend_contract
```

Add checks:
- `SupportsSNI=True` 的 backend 必须让 client connection 支持 `ISSLClientConnection`
- `SetServerName(...)` / `GetServerName` 必须能 round-trip

Observed RED:
- `WolfSSL`: `SupportsSNI=True but connection does not expose ISSLClientConnection`
- `MbedTLS`: `SupportsSNI=True but connection does not expose ISSLClientConnection`

## Task 2: GREEN - minimal public interface alignment

Change:
- `TMbedTLSConnection = class(TBaseSSLConnection, ISSLClientConnection)`
- `TWolfSSLConnection = class(TBaseSSLConnection, ISSLClientConnection, ISSLEarlyDataConnection, ISSLNativeHandleAccess)`

Constraint:
- do not widen into new SNI logic
- reuse the already existing `SetServerName/GetServerName` implementations

## Task 3: Verification

Run:

```bash
fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract
./tmp/test_backend_contract
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- `MbedTLS` / `WolfSSL` no longer claim `SupportsSNI=True` while hiding `ISSLClientConnection`
- cross-backend contract keeps this truth locked
- repo compile gate passes
- minimal CI gate passes

## Focused Revalidation Result (2026-05-18)

- focused 重新编译并运行 `tests/contract/test_backend_contract.pas`
- 当前结果：`Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
- 与本 plan 直接相关的 `Contract 8: Client connection SNI interface alignment` 当前 live truth：
  - `OpenSSL`：PASS，SNI-capable backend 暴露 `ISSLClientConnection`
  - `WolfSSL`：PASS，SNI-capable backend 暴露 `ISSLClientConnection`
  - `MbedTLS`：PASS，SNI-capable backend 暴露 `ISSLClientConnection`
  - `FreePascal`：PASS，SNI-capable backend 暴露 `ISSLClientConnection`
  - `WinSSL`：SKIP，当前 Linux 主机无该 backend live runtime
- 结论：
  - 这条 connection-level SNI alignment 当前继续成立
  - 本批不重跑 `compile_all_modules.py` / `run_minimal_ci_gate.sh --fast-local`
    因为没有生产代码改动，本次目标只是补当前 focused execution receipt
