# Task Plan - Backend OCSP Connection Interface Alignment

## Goal
把连接级 `ISSLOCSPStapling` public contract 收紧到真实实现边界：unsupported backend 不再暴露假阳性 interface，`FreePascal` / `OpenSSL` / `WolfSSL` 的 capability 与 connection getter 语义保持一致。

## Current Batch
1. 在 `tests/contract/test_backend_contract.pas` 增加跨后端 OCSP contract，锁住 capability、optional interface 暴露、以及 getter status 不再落回 `Not Supported` 存根。
2. 对 `src/fafafa.ssl.connection.base.pas` 做最小收紧：移出基类上的 `ISSLOCSPStapling` 接口声明，但保留共享 getter/stub 供显式支持的 backend 复用。
3. 只对 `src/fafafa.ssl.freepascal.connection.pas` / `src/fafafa.ssl.openssl.connection.pas` / `src/fafafa.ssl.wolfssl.connection.pas` 做最小 GREEN，把现有 OCSP getter surface 公开挂进 `ISSLOCSPStapling`。
4. 跑 focused contract、`python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local`，然后按批次提交。

## Status
- [completed] RED contract
- [completed] GREEN implementation
- [completed] Verification
- [completed] Review and commit

## Outcome
- `tests/contract/test_backend_contract.pas` 新增 `Contract 10`，锁住 capability、connection optional interface 暴露和 getter status 三者一致。
- `TBaseSSLConnection` 不再无条件暴露 `ISSLOCSPStapling`；`FreePascal` / `OpenSSL` / `WolfSSL` 改为显式实现。
- `MbedTLS` 在 focused RED 中真实暴露出 connection-level OCSP 假阳性；收口后 unsupported backend 已不再通过 `Supports(...)` 误判支持。
- `docs/BACKEND_CAPABILITY_MATRIX.md` 已把 WinSSL 的 OCSP stapling truth 收紧到当前仓库真实 public surface。

## Risks
- `WinSSL` 本机仍无法做 runtime 验证；这批的 WinSSL 结论来自 capability truth 与类层级对称性，不是 Windows 主机实测。
- 这批只收紧 public contract，没有扩展 `MbedTLS` / `WinSSL` 的新 OCSP 实现，也没有重开在线验证或 responder 调度。

## Follow-up Queue
1. 如果这批收口后仍有 drift，再继续审计其它 connection-level optional/public surface。
2. 后续再决定是否把 OpenSSL CT 从底层 binding 提升回默认 user-facing capability，并补 focused runtime proof。
