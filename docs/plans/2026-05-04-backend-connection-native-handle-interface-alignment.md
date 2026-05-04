# Backend Connection Native Handle Interface Alignment Plan

**Goal:** 把连接级 `ISSLNativeHandleAccess` public contract 对齐到当前迁移文档和 helper 语义：基于 C 库的 backend connection 应显式暴露该可选接口，纯 Pascal backend 继续保持 absent。

**Architecture:** 这批不新增新的 native-handle helper，也不改 context/certificate/store/session 已有行为。只做三件事：
- `tests/contract/test_backend_contract.pas`：新增跨后端 contract，锁住 connection-level `ISSLNativeHandleAccess` 暴露、backend type、以及最小 native handle 可取性。
- `src/fafafa.ssl.mbedtls.connection.pas` / `src/fafafa.ssl.winssl.connection.pas`：让当前已有 `DoGetNativeHandle` 的 C-library connection 显式实现 `ISSLNativeHandleAccess`，补齐缺失的 `GetBackendType` / `IsNativeHandleValid`。
- 维持 `FreePascal` connection 不实现 `ISSLNativeHandleAccess`，避免把纯 Pascal backend 重新变成假阳性。

**Files:**
- Modify: `tests/contract/test_backend_contract.pas`
- Modify: `src/fafafa.ssl.mbedtls.connection.pas`
- Modify: `src/fafafa.ssl.winssl.connection.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - prove documented connection native-handle drift

Run:

```bash
fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract
./tmp/test_backend_contract
```

Add checks:
- `OpenSSL` / `WolfSSL` / `MbedTLS` / `WinSSL` 这类 C-library backend 的 connection 应支持 `ISSLNativeHandleAccess`
- `GetBackendType` 必须和 backend 枚举一致
- `GetNativeHandle` 不应返回 `nil`
- `FreePascal` backend connection 不应暴露 `ISSLNativeHandleAccess`

Expected RED:
- `MbedTLS`: connection 目前未暴露 `ISSLNativeHandleAccess`
- `WinSSL`: 在 Windows 主机上应同型 RED；本机如 backend 不可用则静态对称修复

## Task 2: GREEN - expose the missing optional interface

Change:
- `TMbedTLSConnection = class(TBaseSSLConnection, ISSLClientConnection, ISSLNativeHandleAccess)`
- `TWinSSLConnection = class(TBaseSSLConnection, ISSLClientConnection, ISSLNativeHandleAccess)`
- 复用基类现有 `GetNativeHandle`，只补 `GetBackendType` / `IsNativeHandleValid`

Constraints:
- 不重开新的 backend capability 字段
- 不修改 context/certificate/store/session 的 native-handle surface
- 不让 `FreePascal` backend 暴露伪 native handle

## Task 3: Verification

Run:

```bash
fpc -Fu./src tests/contract/test_backend_contract.pas -otmp/test_backend_contract && ./tmp/test_backend_contract
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- `MbedTLS` / `WinSSL` connection 与现有 OpenSSL/WolfSSL 一样显式暴露 `ISSLNativeHandleAccess`
- `FreePascal` connection 继续保持 absent
- focused contract、compile gate、minimal CI gate 全绿
- 台账同步到当前真相
