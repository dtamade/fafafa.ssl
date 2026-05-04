# Task Plan - Backend Connection Native Handle Interface Alignment

## Goal
把连接级 `ISSLNativeHandleAccess` public contract 对齐到当前迁移文档和 helper 语义：基于 C 库的 backend connection 应显式暴露该可选接口，纯 Pascal backend 继续保持 absent。

## Current Batch
1. 在 `tests/contract/test_backend_contract.pas` 增加跨后端 native-handle contract，锁住 connection-level `ISSLNativeHandleAccess` 暴露、backend type、以及最小 native handle 可取性。
2. 对 `src/fafafa.ssl.mbedtls.connection.pas` / `src/fafafa.ssl.winssl.connection.pas` 做最小 GREEN：让已有 `DoGetNativeHandle` 的 C-library connection 显式实现 `ISSLNativeHandleAccess`，并补齐 `GetBackendType` / `IsNativeHandleValid`。
3. 保持 `FreePascal` connection 不实现该接口，避免纯 Pascal backend 出现伪 native handle surface。
4. 跑 focused contract、`python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local`，然后按批次提交。

## Status
- [completed] RED contract
- [completed] GREEN implementation
- [completed] Verification
- [in_progress] Review and commit

## Outcome
- `tests/contract/test_backend_contract.pas` 新增 `Contract 11`，锁住 connection-level `ISSLNativeHandleAccess` 暴露、backend type 和最小 native handle 可取性。
- `TMbedTLSConnection` / `TWinSSLConnection` 现在显式实现 `ISSLNativeHandleAccess`，复用基类 `GetNativeHandle`，并补齐 `GetBackendType` / `IsNativeHandleValid`。
- `FreePascal` connection 继续保持该接口 absent，避免纯 Pascal backend 暴露伪 native handle。

## Risks
- 这批没有 capability 字段可直接对照，主要依据是 `docs/MIGRATION_GUIDE_V1.1.md` 里“旧 `ISSLConnection.GetNativeHandle` 已迁到可选接口”的 public contract。
- `WinSSL` 的对称修复在 Linux 主机上无法单元级编译到 `Windows` SDK 依赖；当前证据来自结构对称性和 focused contract 设计，不是 Windows 主机实测。

## Follow-up Queue
1. 如果这批收口后仍有 drift，再继续审计其它未被 cross-backend contract 锁住的 optional/public surface。
2. 如果 native-handle contract 被证明确实是文档过宽而非实现缺口，则要单开 docs truth 批次，而不是继续扩实现。
