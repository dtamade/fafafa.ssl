# Task Plan - Backend CT Optional Interface Alignment

## Goal
把连接级 `ISSLCertificateTransparency` / `ISSLCertificateTransparencyValidation` public contract 收紧到真实实现边界：unsupported backend 不再暴露假阳性 interface，`FreePascal` / `OpenSSL` 的 capability 与 connection getter 语义保持一致。

## Current Batch
1. 在 `tests/contract/test_backend_contract.pas` 增加跨后端 CT contract，锁住 capability、optional interface 暴露、以及 getter status 不再落回 `Not Supported` 存根。
2. 对 `src/fafafa.ssl.connection.base.pas` 做最小收紧：移出基类上的 CT / validation interface 声明，但保留共享 getter/stub 供显式支持的 backend 复用。
3. 只对 `src/fafafa.ssl.freepascal.connection.pas` / `src/fafafa.ssl.openssl.connection.pas` 做最小 GREEN，其中 `OpenSSL` 基于已有 CT binding 补 connection runtime surface。
4. 跑 focused contract、`python3 scripts/compile_all_modules.py`、`bash scripts/run_minimal_ci_gate.sh --fast-local`，然后按批次提交。

## Status
- [completed] RED contract
- [completed] GREEN implementation
- [completed] Verification
- [pending] Review and commit

## Outcome
- `TBaseSSLConnection` 不再无条件暴露 `ISSLCertificateTransparency` / `ISSLCertificateTransparencyValidation`。
- `FreePascal` 继续显式暴露并实现 CT / validation connection surface。
- 当前默认 runtime truth 下，`OpenSSL` / `WolfSSL` / `MbedTLS` connection 都不再通过 `Supports(...)` 假阳性暴露 CT interface。
- 新增的 cross-backend contract 已经锁住：没有 capability 的 backend 不能再靠基类存根冒充 CT surface。

## Risks
- `OpenSSL` 仓库里仍有底层 CT binding，但默认 capability 目前没有把它发布成 connection surface；如果后续要重新打开这条线，必须一并补 capability、interface 和 runtime 证据，而不能只恢复接口声明。

## Follow-up Queue
1. 如果这批收口后仍有 drift，再继续审计其它 connection-level optional/public surface。
2. 如果要把 OpenSSL CT 重新升级为 user-facing capability，应单开一批去接通 `osmCT` 默认加载、connection getter 和 focused runtime proof。
