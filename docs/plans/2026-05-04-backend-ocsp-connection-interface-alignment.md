# Backend OCSP Connection Interface Alignment Plan

**Goal:** 收紧连接级 `ISSLOCSPStapling` optional interface 暴露，让 `FreePascal` / `OpenSSL` / `WolfSSL` / `MbedTLS` / `WinSSL` 的 capability、connection interface 和 getter 语义重新回到同一套真相源。

**Architecture:** 这批不重开新的 OCSP 功能面，不扩到 online fetch / responder 调度，也不重写 capability 框架。只做三件事：
- `tests/contract/test_backend_contract.pas`：新增跨后端 contract，锁住 `SupportsOCSPStapling` / `OCSPStaplingSupport` 与 connection `ISSLOCSPStapling` optional interface 暴露的一致性，并防止 capable backend 继续返回基类存根 `Not Supported`。
- `src/fafafa.ssl.connection.base.pas`：把 `ISSLOCSPStapling` 从 `TBaseSSLConnection` 的类声明中移出，保留共享 getter/stub 作为子类复用实现。
- `src/fafafa.ssl.freepascal.connection.pas` / `src/fafafa.ssl.openssl.connection.pas` / `src/fafafa.ssl.wolfssl.connection.pas`：让真实有 connection OCSP surface 的 backend 显式实现 `ISSLOCSPStapling`。

**Files:**
- Modify: `tests/contract/test_backend_contract.pas`
- Modify: `src/fafafa.ssl.connection.base.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`
- Modify: `src/fafafa.ssl.openssl.connection.pas`
- Modify: `src/fafafa.ssl.wolfssl.connection.pas`
- Modify: `docs/BACKEND_CAPABILITY_MATRIX.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - prove OCSP connection interface drift

Run:

```bash
fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract
./tmp/test_backend_contract
```

Add checks:
- `SupportsOCSPStapling=True` 的 backend，connection 必须暴露 `ISSLOCSPStapling`
- `OCSPStaplingSupport <> sslSupportNone` 的 backend，getter status 不能继续落到基类存根 `Not Supported`
- capability 为 `False/None` 的 backend，不应继续暴露 `ISSLOCSPStapling` 假阳性 interface

Expected RED:
- `MbedTLS` / `WinSSL` 仍通过 `TBaseSSLConnection` 暴露 `ISSLOCSPStapling`

## Task 2: GREEN - align public surface to real runtime

Change:
- `TBaseSSLConnection`: 移出 `ISSLOCSPStapling` 接口声明
- `TFreePascalConnection`: 显式实现 `ISSLOCSPStapling`
- `TOpenSSLConnection`: 显式实现 `ISSLOCSPStapling`
- `TWolfSSLConnection`: 显式实现 `ISSLOCSPStapling`

Constraints:
- 不新增新的 OCSP builder 开关
- 不扩大到 `WinSSL` / `MbedTLS` 的新 OCSP 实现
- 不重开 OCSP cryptographic verification / online fetch / server issuance 其它主线

## Task 3: Verification

Run:

```bash
fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract
./tmp/test_backend_contract
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- unsupported backend 不再通过 `Supports(...)` 暴露 `ISSLOCSPStapling` 假阳性 interface
- `FreePascal` / `OpenSSL` / `WolfSSL` 的 connection OCSP surface 继续可见且不走基类 `Not Supported` 存根
- focused contract、compile gate、minimal CI gate 全绿
- 台账和 capability 文档同步到当前真相
