# Task Plan - Backend Client-Connection SNI Interface Alignment

## Goal
把 `MbedTLS` / `WolfSSL` 这两个已经具备 SNI 实现细节的后端，从“connection 类里有 `SetServerName/GetServerName`，但 public interface 没挂 `ISSLClientConnection`”推进到和 `OpenSSL` / `FreePascal` 一致的 public contract truth。

## Current Batch
1. 在 `tests/contract/test_backend_contract.pas` 增加跨后端 contract，锁住 `SupportsSNI=True => connection 必须暴露 ISSLClientConnection`。
2. 只对 `src/fafafa.ssl.mbedtls.connection.pas` / `src/fafafa.ssl.wolfssl.connection.pas` 做最小 GREEN：把现有 `SetServerName/GetServerName` 绑定进 `ISSLClientConnection`。
3. 跑 focused contract、`python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local`，然后按批次提交。

## Status
- [completed] RED contract
- [completed] GREEN implementation
- [completed] Verification
- [completed] Review and commit

## Outcome
- `MbedTLS` / `WolfSSL` 现在不再依赖“类里有方法但 interface 不可见”的隐式实现。
- cross-backend contract 已经直接锁住：只要 backend capability 仍宣称 `SupportsSNI=True`，connection 就必须暴露 `ISSLClientConnection` 并支持 per-connection server-name round-trip。

## Risks
- 这批只修 public interface 暴露，不重开 handshake / hostname verify / ALPN 语义。
- `MbedTLS` / `WolfSSL` 构造函数里仍保留从 deprecated context-level `GetServerName` 回退填充默认值的兼容路径；这不是本批阻塞点，但仍是后续可继续收紧的信号。

## Follow-up Queue
1. 审计 connection-level 其他 optional/public surface 是否还存在 capability 与 interface 暴露分叉。
2. 优先检查 CT / validation 这类仍依赖 `Supports(...)` 的 connection optional interface truth。
