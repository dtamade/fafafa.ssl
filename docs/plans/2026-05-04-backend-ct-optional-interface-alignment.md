# Backend CT Optional Interface Alignment Plan

**Goal:** 收紧连接级 CT / CT validation optional interface 暴露，让 `FreePascal` / `OpenSSL` / `MbedTLS` / `WolfSSL` / `WinSSL` 的 capability、connection interface 和 getter 语义重新回到同一套真相源。

**Architecture:** 这批不重开新的 CT 功能面，不扩到新的 TLS parser，也不重写 capability 框架。最终落地只做三件事：
- `tests/contract/test_backend_contract.pas`：新增跨后端 contract，锁住 `SupportsCertificateTransparency` / `CertTransparencySupport` 与 connection CT optional interface 暴露的一致性，并防止 capable backend 继续返回基类存根 `Not Supported`。
- `src/fafafa.ssl.connection.base.pas`：把 CT / CT validation optional interface 从 `TBaseSSLConnection` 的类声明中移出，保留共享 getter/stub 作为子类复用实现。
- `src/fafafa.ssl.freepascal.connection.pas`：让当前唯一真正发布 CT connection surface 的 backend 继续显式实现这些接口。

**Files:**
- Modify: `tests/contract/test_backend_contract.pas`
- Modify: `src/fafafa.ssl.connection.base.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`
- Modify: `docs/BACKEND_CAPABILITY_MATRIX.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - prove CT interface drift

Run:

```bash
fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract
./tmp/test_backend_contract
```

Add checks:
- `SupportsCertificateTransparency=True` 的 backend，connection 必须暴露 `ISSLCertificateTransparency`
- `CertTransparencySupport <> sslSupportNone` 的 backend，connection 必须暴露 `ISSLCertificateTransparencyValidation`
- capable backend 的 CT getter status 不能继续落到基类存根 `Not Supported`
- capability 为 `False/None` 的 backend，不应继续暴露 CT / validation 假阳性 interface

Observed RED:
- `OpenSSL`: `SupportsCertificateTransparency=False but connection still exposes ISSLCertificateTransparency`
- `OpenSSL`: `CertTransparencySupport=None but connection still exposes ISSLCertificateTransparencyValidation`
- `WolfSSL` / `MbedTLS`: 同样两项失败

Interpretation:
- 当前默认 runtime truth 下，`OpenSSL` 也没有把 CT 发布成 user-facing capability
- 真正的 drift 点是 `TBaseSSLConnection` 无条件暴露了 CT interface

## Task 2: GREEN - align public surface to real runtime

Change:
- `TBaseSSLConnection`: 移出 `ISSLCertificateTransparency` / `ISSLCertificateTransparencyValidation` 接口声明
- `TFreePascalConnection`: 显式实现 CT / validation interface

Constraints:
- 不新增新的 CT builder 开关
- 不扩大到 `OpenSSL` / `WolfSSL` / `MbedTLS` / `WinSSL` 的新 CT 实现
- 不把仓库里已有的底层 OpenSSL CT binding 误写成“当前 backend 已发布的连接能力”

## Task 3: Verification

Run:

```bash
fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract
./tmp/test_backend_contract
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- unsupported backend 不再通过 `Supports(...)` 暴露 CT / validation 假阳性 interface
- `FreePascal` 的 connection CT surface 继续可见且不走基类 `Not Supported` 存根
- focused contract、compile gate、minimal CI gate 全绿
- 台账和 capability 文档同步到当前真相

## Execution Result

- 最小 GREEN 比最初设想更小：
  - 不需要新增 `OpenSSL` 生产代码
  - 只需要让基类停止无条件暴露 CT interface，并让 `FreePascal` 显式保留它
- focused contract：
  - `Passed: 55 / Failed: 0 / Skipped: 12`
- `python3 scripts/compile_all_modules.py`：
  - `185/185`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`：
  - PASS
