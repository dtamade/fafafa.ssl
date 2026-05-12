# Task Plan - WinSSL sslCtxBoth Verification Role Clarification

## Goal
修复 `sslCtxBoth` 在 `WinSSL` 显式 `Connect` / `Accept` 之后仍按 `ContextType` 猜证书校验角色的 public drift。

## Current Batch
1. 写 focused RED source contract，锁住 WinSSL dual-context verification path 仍按 `ContextType` 推导 role。
2. 在 `src/fafafa.ssl.winssl.connection.pas` 做最小修法，把 verify role 改成显式来源，并让 verify-result getter 复用同一真相源。
3. 跑 focused GREEN、Win64 compile proof、Linux compile gate。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the WinSSL verification-role batch
- [completed] focused RED contract added and observed
- [completed] minimal WinSSL verification-role fix implemented
- [completed] focused verification and compile proof

## Current Evidence
- focused RED:
  - `bash tests/scripts/test_winssl_sslctxboth_verification_role_contract.sh`
  - result before fix: FAIL on missing explicit peer-validation role state, explicit role recorder/resolver, and role-parameterized `ValidatePeerCertificate(...)`
- minimal implementation:
  - `src/fafafa.ssl.winssl.connection.pas`
    - added connection-local peer-validation role state and resolver
    - `ValidatePeerCertificate(...)` now accepts explicit `AIsClient`
    - `DoConnect` / `DoAccept` / verify-result getter now use explicit verification role truth instead of `ContextType`
    - fixed a Pascal `if ... then ... else` semicolon slip caught by Win64 cross-compile while landing the change
- focused GREEN:
  - `bash tests/scripts/test_winssl_sslctxboth_verification_role_contract.sh`: PASS
  - `fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_role_client_win64 -FEtmp/winssl_role_client_win64 -otmp/winssl_role_client_win64/test_winssl_hostname_mismatch_online.exe tests/winssl/test_winssl_hostname_mismatch_online.pas`: PASS
  - `fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_role_server_win64 -FEtmp/winssl_role_server_win64 -otmp/winssl_role_server_win64/test_winssl_mtls_e2e_local.exe tests/winssl/test_winssl_mtls_e2e_local.pas`: PASS
  - `python3 scripts/compile_all_modules.py`: PASS, `185/185`
  - `git diff --check`: PASS

# Task Plan - sslCtxBoth Roleless Handshake Clarification

## Goal
修复 `sslCtxBoth` 在 role-less 握手入口上的公共合同漂移：`DoHandshake` 和 OpenSSL 未连接 stream `Read/Write` 当前会静默猜 client/server，缺少明确边界。

## Current Batch
1. 写 focused RED，证明 `sslCtxBoth` 走 `DoHandshake` 和 OpenSSL 隐式 stream handshake 时仍会偷偷猜角色。
2. 在连接基类与 OpenSSL stream implicit handshake 入口做最小 fail-fast 修法，不扩到更大的 dual-role state 设计。
3. 跑 focused GREEN 与相邻回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the sslCtxBoth roleless-handshake batch
- [completed] focused RED regression added and observed
- [completed] minimal handshake-boundary fix implemented
- [completed] focused verification and neighbor regression review

## Notes
- 这批不改变显式 `Connect` / `Accept` 的语义。
- 这批也不引入新的 “connection role” 持久状态；先把当前没有角色来源的公共入口收口成清晰 precondition。

## Current Evidence
- focused RED:
  - `fpc -Fu./src -Fu./tests tests/test_sslctxboth_roleless_handshake_clarification.pas -otmp/test_sslctxboth_roleless_handshake_clarification && ./tmp/test_sslctxboth_roleless_handshake_clarification`
  - result before fix: `17 passed / 7 failed`
  - failure shape:
    - FreePascal / OpenSSL / MbedTLS `sslCtxBoth` `DoHandshake` 没有给出 configuration boundary
    - WolfSSL `sslCtxBoth` `DoHandshake` 甚至直接回到 `sslHsInProgress`
    - OpenSSL dual-context stream `Read/Write` 的隐式握手没有记录明确 configuration error
- minimal implementation:
  - `src/fafafa.ssl.connection.base.pas`
    - added shared dual-role handshake ambiguity helpers
    - `DoHandshake` now fail-fast on `sslCtxBoth` with `sslErrConfiguration`
  - `src/fafafa.ssl.openssl.connection.pas`
    - stream `Read/Write` now reject disconnected `sslCtxBoth` implicit handshake paths
    - `DoHandshake` log label now reports `Dual` instead of misleading `Server`
- focused GREEN:
  - `tests/test_sslctxboth_roleless_handshake_clarification.pas`: PASS, `24 passed / 0 failed`
  - `tests/test_openssl_connection_stream_handshake_contract.pas`: PASS
  - `tests/test_sslctxboth_client_capability_clarification.pas`: PASS, `28 passed / 0 failed / 1 skipped`
  - `python3 scripts/compile_all_modules.py`: PASS, `185/185`
  - `git diff --check`: PASS

# Task Plan - sslCtxBoth Client Capability Clarification

## Goal
修复 `sslCtxBoth` 在连接级 client capability 上的公共合同漂移：当前多个后端的 connection 构造和 early-data role gate 仍把它当成“不是 client”，与 `sslCtxBoth` 的公开语义不一致。

## Current Batch
1. 写 focused RED，锁定 `sslCtxBoth` 作为客户端使用时的两类失真：
   - context fallback `ServerName` 没有继承到连接
   - supporting backends 的 `SetEarlyData(...)` 先被错误 role gate 拒绝
2. 在相关 connection units 做最小 capability-based 修法，不扩到更大的 dual-role handshake 设计。
3. 跑 focused GREEN 与相邻 server-name / early-data 回归。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the sslCtxBoth client-capability batch
- [completed] focused RED regression added and observed
- [completed] minimal connection capability fix implemented
- [completed] focused verification and neighbor regression review

## Notes
- 这批只收“client-capable runtime truth”，不重新定义 `sslCtxBoth` 的完整 dual-role 握手状态机。
- 如果 fresh RED 证明 `DoHandshakeInternal` / implicit handshake 路径也存在真实 public drift，再单开下一批。

## Current Evidence
- focused RED:
  - `fpc -Fu./src -Fu./tests tests/test_sslctxboth_client_capability_clarification.pas -otmp/test_sslctxboth_client_capability_clarification && ./tmp/test_sslctxboth_client_capability_clarification`
  - result before fix: `21 passed / 7 failed / 1 skipped`
  - failure shape:
    - `sslCtxBoth` stream connections on FreePascal / OpenSSL / WolfSSL / MbedTLS lost context fallback `ServerName`
    - `sslCtxBoth` socket connections on FreePascal lost the same fallback
    - FreePascal / OpenSSL `SetEarlyData(...)` rejected `sslCtxBoth` with `Early data is only available on client connections`
- minimal implementation:
  - `src/fafafa.ssl.connection.base.pas`
    - added shared client/server capability helpers for connection units
  - `src/fafafa.ssl.freepascal.connection.pas`
    - dual-context connections now inherit client fallback `ServerName`
    - client early-data gate now accepts `sslCtxBoth`
  - `src/fafafa.ssl.openssl.connection.pas`
    - dual-context constructors now inherit client fallback `ServerName`
    - client early-data gate now accepts `sslCtxBoth`
  - `src/fafafa.ssl.wolfssl.connection.pas`
    - dual-context constructors now inherit client fallback `ServerName`
    - client/server pre-handshake OCSP capability gates now accept `sslCtxBoth`
    - client early-data gate now accepts `sslCtxBoth`
  - `src/fafafa.ssl.winssl.connection.pas`
    - dual-context constructors now inherit client fallback `ServerName`
  - `src/fafafa.ssl.mbedtls.connection.pas`
    - dual-context constructors now inherit client fallback `ServerName`
- focused GREEN:
  - `tests/test_sslctxboth_client_capability_clarification.pas`: PASS, `28 passed / 0 failed / 1 skipped`
  - `tests/test_freepascal_context_server_name_inheritance.pas`: PASS
  - `tests/test_early_data_public_api_contract.pas`: PASS
  - `tests/test_factory_config_server_name_isolation.pas`: PASS
  - `tests/test_openssl_wolfssl_early_data_connection_contract.pas`: PASS
  - `python3 scripts/compile_all_modules.py`: PASS, `185/185`
  - `git diff --check`: PASS

# Task Plan - Early-Data Context Scope Clarification

## Goal
修复 early-data mixed-scope 下发漂移：`TSSLContextBuilder` / `TSSLConfig` 可以同时携带 client/server early-data 默认值，但 builder/factory/helper 当前会把 opposite-side 值也写进错误的 context。

## Current Batch
1. 写 focused RED，证明 builder / factory / public helper 会把 `ClientEarlyDataEnabled`、`ServerEarlyDataPolicy`、`ServerMaxEarlyDataSize` 泄漏到错误的 context type。
2. 在 `src/fafafa.ssl.context.builder.pas`、`src/fafafa.ssl.factory.pas` 做最小 scope-aware application 修法，保持组合配置模型不变。
3. 跑 focused GREEN、相邻 early-data 回归，并修掉回归里暴露出来的默认持久化 replay-ledger 测试脆弱点。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the early-data context-scope batch
- [completed] RED regression added and observed
- [completed] minimal builder/factory/helper scope fix implemented
- [completed] adjacent replay-store test flake hardened
- [completed] focused verification and neighbor regression review

## Notes
- 这批不是把组合 `TSSLConfig` / builder 拆掉，而是把“组合配置”与“具体 context 下发”分开：
  - 组合对象仍可同时携带 client/server 默认值
  - 具体 `sslCtxClient` / `sslCtxServer` / `sslCtxBoth` 创建时只应用对应子集
- 这批不把 mixed-scope 改成 fail-fast，因为现有 builder / config round-trip / shared-default 用法本身就隐含“一个组合配置可供两侧复用”的设计。
- `TSSLHelper.ConfigureClientEarlyData(...)` / `ConfigureServerEarlyData(...)` 现在也跟随 context type 收口，wrong-scope context 返回 `False`。
- 相邻发现的测试脆弱点不是生产回归：
  - `tests/test_factory_config_early_data_isolation.pas` 对默认持久化 replay-ledger 复用了固定 session label
  - 多次重跑会被历史 residue 污染
  - 现已改成每次运行唯一 label

## Current Evidence
- focused RED:
  - `fpc -Fu./src -Fu./tests tests/test_early_data_context_scope_clarification.pas -otmp/test_early_data_context_scope_clarification && ./tmp/test_early_data_context_scope_clarification`
  - result before fix: `30 passed / 14 failed`
  - failure shape:
    - `BuildClient` / factory client path 会观察到 server policy/max
    - `BuildServer` / factory server path 会观察到 client early-data flag
    - `TSSLHelper.ConfigureServerEarlyData(...)` 会错误接受 client context
    - `TSSLHelper.ConfigureClientEarlyData(...)` 会错误接受 server context
- minimal implementation:
  - `src/fafafa.ssl.context.builder.pas`
    - new scope-aware early-data application helper
    - `BuildClient` 仅下发 client early-data flag
    - `BuildServer` 仅下发 server policy/max
  - `src/fafafa.ssl.factory.pas`
    - `ApplyEarlyDataContextConfig(...)` now applies only the context-relevant subset
    - `TSSLHelper.ConfigureClientEarlyData(...)` / `ConfigureServerEarlyData(...)` now refuse wrong-scope contexts
  - `src/fafafa.ssl.debug.utils.pas`
    - config dump now labels client/server early-data scalar fields with their actual application scope
  - `tests/test_factory_config_early_data_isolation.pas`
    - one-shot server-context assertion updated to the new scope truth
    - default persistent replay-ledger probe labels now use per-run unique session ids
- focused GREEN:
  - `tests/test_early_data_context_scope_clarification.pas`: PASS, `44 passed / 0 failed`
  - `tests/test_factory_config_early_data_isolation.pas`: PASS, `60 passed / 0 failed`
  - `tests/config/test_context_builder_early_data_contract.pas`: PASS
  - `tests/test_early_data_public_api_contract.pas`: PASS
  - `tests/config/test_context_builder_try.pas`: PASS, `66 passed / 0 failed`

## Verification Plan
1. `fpc -Fu./src -Fu./tests tests/test_early_data_context_scope_clarification.pas -otmp/test_early_data_context_scope_clarification && ./tmp/test_early_data_context_scope_clarification`
2. `fpc -Fu./src -Fu./tests tests/test_factory_config_early_data_isolation.pas -otmp/test_factory_config_early_data_isolation && ./tmp/test_factory_config_early_data_isolation`
3. `fpc -Fu./src -Fu./tests tests/config/test_context_builder_early_data_contract.pas -otmp/test_context_builder_early_data_contract && ./tmp/test_context_builder_early_data_contract`
4. `fpc -Fu./src -Fu./tests tests/test_early_data_public_api_contract.pas -otmp/test_early_data_public_api_contract && ./tmp/test_early_data_public_api_contract`
5. `fpc -Fu./src -Fu./tests tests/config/test_context_builder_try.pas -otmp/test_context_builder_try && ./tmp/test_context_builder_try`
6. `git diff --check`
7. `git status --short`

# Task Plan - Client Replay-Store Scope Clarification

## Goal
修复 `server_early_data_replay_store_file` / `server_early_data_replay_store_directory` 在 client builder/factory 路径上的静默 no-op，让 server-only replay-store opt-in 变成明确的 scope contract。

## Current Batch
1. 写 focused RED，证明 `ValidateClient` / `TryBuildClient` / factory default-config client path / factory one-shot client path 都会静默接受 server replay-store 字段。
2. 在 `src/fafafa.ssl.context.builder.pas` 和 `src/fafafa.ssl.factory.pas` 做最小 fail-fast scope 修法，不改 server replay-store runtime 安装链。
3. 跑 focused GREEN、相邻 replay-store 回归、diff hygiene，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the client replay-store scope batch
- [completed] RED regression added and observed
- [completed] minimal builder/factory scope fix implemented
- [completed] focused verification and adjacent regression review

## Notes
- 这批只收 `ServerEarlyDataReplayStoreFile` / `ServerEarlyDataReplayStoreDirectory` 这两个最明确的 server-only opt-in。
- 这批不扩到 `ClientEarlyDataEnabled` / `ServerEarlyDataPolicy` / `ServerMaxEarlyDataSize` 的 broader scope truth；先收最小高价值误导点。
- shared default config 一旦携带 server replay-store opt-in，default-path client context 现在会 fail-fast；这比继续静默丢掉 replay-store 配置更符合 public truth。
- factory 的 replay-store 安装边界现在按 server-capable context 处理，`sslCtxServer` / `sslCtxBoth` 继续允许。

## Current Evidence
- focused RED:
  - `fpc -Fu./src -Fu./tests tests/test_early_data_replay_store_client_scope_clarification.pas -otmp/test_early_data_replay_store_client_scope_clarification && ./tmp/test_early_data_replay_store_client_scope_clarification`
  - result before fix: `0 passed / 14 failed`
  - failure shape: `ValidateClient` / `TryBuildClient` / factory default-path client / factory one-shot client 全都静默接受 server replay-store config
- minimal implementation:
  - `src/fafafa.ssl.context.builder.pas`
    - `ValidateClient` now reports server replay-store fields as invalid on client builders
    - `BuildClient` / `TryBuildClient` now fail fast on those fields
  - `src/fafafa.ssl.factory.pas`
    - client factory paths now reject server replay-store fields with `ESSLConfigurationException`
    - replay-store installer application now treats `sslCtxBoth` as server-capable
  - `src/fafafa.ssl.debug.utils.pas`
    - config dump now labels replay-store fields as server-scoped and notes client builder/factory contexts do not accept them
  - `tests/test_factory_config_early_data_isolation.pas`
    - old default-path client no-op expectation updated to the new fail-fast truth
- focused GREEN:
  - `tests/test_early_data_replay_store_client_scope_clarification.pas`: PASS, `14 passed / 0 failed`
  - `tests/test_factory_config_early_data_isolation.pas`: PASS, `60 passed / 0 failed`
  - `tests/config/test_context_builder_try.pas`: PASS, `66 passed / 0 failed`
  - `tests/config/test_config_validation.pas`: PASS, `53 passed / 0 failed`

## Verification Plan
1. `fpc -Fu./src -Fu./tests tests/test_early_data_replay_store_client_scope_clarification.pas -otmp/test_early_data_replay_store_client_scope_clarification && ./tmp/test_early_data_replay_store_client_scope_clarification`
2. `fpc -Fu./src -Fu./tests tests/test_factory_config_early_data_isolation.pas -otmp/test_factory_config_early_data_isolation && ./tmp/test_factory_config_early_data_isolation`
3. `fpc -Fu./src -Fu./tests tests/config/test_context_builder_try.pas -otmp/test_context_builder_try && ./tmp/test_context_builder_try`
4. `fpc -Fu./src -Fu./tests tests/config/test_config_validation.pas -otmp/test_config_validation && ./tmp/test_config_validation`
5. `git diff --check`
6. `git status --short`

# Task Plan - Factory ServerName Scope Clarification

## Goal
修复 `TSSLFactory.CreateContext(...)` 对 `TSSLConfig.ServerName` 的 server-side scope 漂移：当前 factory/config 路径会把 client-only `ServerName` 静默接受并写入 server context，但 server-side connections 明确忽略它。

## Current Batch
1. 写 focused RED 合同，证明 factory 的 one-shot config path 和 library-default path 都会接受 server-context `ServerName`。
2. 在 `src/fafafa.ssl.factory.pas` 做最小 scope 校验，让 factory 在没有 warning surface 的情况下 fail-fast。
3. 跑 focused verification、相邻回归、diff hygiene，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the factory ServerName scope batch
- [completed] RED regression added and observed
- [completed] minimal factory scope fix implemented
- [completed] focused verification and review

## Notes
- 这批不改 builder：`WithSNI` 在 server path 上继续保留兼容 + warning 语义。
- 这批只收口 factory/config，因为它没有 builder 那样的 validation warning surface，继续静默接受会更误导。
- 优先修 `sslCtxServer` 这条已被现有验证文案明确定义为“连接会忽略”的路径。

## Current Evidence
- focused RED:
  - `fpc -Fu./src -Fu./tests tests/test_factory_server_name_scope_clarification.pas -otmp/test_factory_server_name_scope_clarification && ./tmp/test_factory_server_name_scope_clarification`
  - result before fix: `4 passed / 2 failed`
  - failure shape: client controls passed, while server default-config and one-shot config paths both silently accepted `ServerName` instead of rejecting it
- minimal implementation:
  - `src/fafafa.ssl.factory.pas`
    - `ValidateConnectionCreationScope(...)` now receives the effective context type
    - server-context `ServerName` now raises `ESSLConfigurationException` on factory/config creation paths
  - `src/fafafa.ssl.debug.utils.pas`
    - `DumpSSLConfig(...)` now labels `ServerName` as client-scoped and notes server factory contexts do not accept it
- focused GREEN:
  - `tests/test_factory_server_name_scope_clarification.pas`: PASS, `6 passed / 0 failed`
  - `tests/test_factory_config_server_name_isolation.pas`: PASS
  - `tests/config/test_config_validation.pas`: PASS, including the existing builder server-side warning contract

## Verification Plan
1. `fpc -Fu./src -Fu./tests tests/test_factory_server_name_scope_clarification.pas -otmp/test_factory_server_name_scope_clarification && ./tmp/test_factory_server_name_scope_clarification`
2. `fpc -Fu./src -Fu./tests tests/test_factory_config_server_name_isolation.pas -otmp/test_factory_config_server_name_isolation && ./tmp/test_factory_config_server_name_isolation`
3. `fpc -Fu./src -Fu./tests tests/config/test_config_validation.pas -otmp/test_config_validation && ./tmp/test_config_validation`
4. `git diff --check`
5. `git status --short`

# Task Plan - Builder Server Smoke Truth

## Goal
修复 `tests/test_builder_integration.pas` 的 server-context smoke 误导：当前脚本把一个缺少证书的 `BuildServer` 调用当成集成 smoke，导致输出看起来像 runtime 失败；应改成带临时自签名证书的真实成功路径。

## Current Batch
1. 复核 docs / builder validation / existing config tests，确认 `BuildServer` 缺证书失败是既有正确语义，不是 runtime regression。
2. 最小修正 builder integration smoke，让 server-context case 带测试证书与私钥。
3. 跑 focused verification、diff hygiene，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the builder server smoke batch
- [completed] server-context semantics revalidated against docs and existing builder contracts
- [completed] misleading integration smoke updated to a real success path
- [completed] focused verification and review

## Notes
- 这批只修测试真值，不改 `BuildServer` 运行时语义。
- `Server context requires a certificate` 仍然是正确的 runtime/validation 结论。
- 真实要修的是：集成 smoke 不该把一个注定失败的 server build 当成“成功路径”示例。

## Current Evidence
- semantics revalidated before the fix:
  - `src/fafafa.ssl.context.builder.pas` 明确在 `BuildServer` 和 `ValidateServer` 上要求证书
  - `docs/BACKEND_SELECTION_GUIDE.md` 的服务器示例也始终带 `WithCertificate(...)` / `WithPrivateKey(...)`
  - `tests/config/test_context_builder_try.pas` 已覆盖“无证书失败、有证书成功”的 builder contract
- minimal test-truth fix:
  - `tests/test_builder_integration.pas` server-context smoke now generates a temporary self-signed certificate and private key via `TCertificateUtils.TryGenerateSelfSignedSimple(...)`
  - the smoke then uses `WithCertificatePEM(...)` + `WithPrivateKeyPEM(...)` before `BuildServer`
- focused verification:
  - `fpc -Fu./src -Fu./tests tests/test_builder_integration.pas -otmp/test_builder_integration && ./tmp/test_builder_integration`
  - result: all eight smoke cases now report success

# Task Plan - Security-First Selector Viability

## Goal
修复 `CreateSecurityFirstRequirements` 与当前能力矩阵/安全评分真值的脱节，避免在本机已有 OpenSSL 可用且硬性协议/算法条件满足时，security-first 需求模板仍因过高门槛而选不出任何后端。

## Current Batch
1. 写 focused RED 合同，证明当存在满足 security-first 硬性协议/算法要求的可用后端时，`CreateSecurityFirstRequirements` 的最低安全分门槛不能高于这些后端可达到的真实安全分。
2. 在 `src/fafafa.ssl.backend.selector.pas` 做最小修法，让 security-first 默认模板重新可用。
3. 跑 focused verification、相邻 smoke、diff hygiene，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the security-first viability batch
- [completed] RED regression added and observed
- [completed] minimal requirement-template fix implemented
- [completed] focused verification and review

## Notes
- 这批优先修需求模板真值，不重算全局 `GetSecurityScore(...)` 权重。
- 修法必须和当前 capability truth 对齐，而不是靠放宽协议/算法硬要求掩盖问题。
- 如果 fresh evidence 证明现有最高可达安全分是 `80`，那 security-first 默认门槛就不能继续写成 `85`。

## Current Evidence
- focused RED:
  - `fpc -Fu./src -Fu./tests tests/test_backend_selector_security_first_viability.pas -otmp/test_backend_selector_security_first_viability && ./tmp/test_backend_selector_security_first_viability`
  - result before fix: `1 passed / 2 failed`
  - failure shape: at least one available backend satisfied the hard security-first protocol/algorithm requirements, but `CreateSecurityFirstRequirements.MinSecurityScore` still exceeded the best eligible backend security score, so selection returned none
- minimal implementation:
  - `src/fafafa.ssl.backend.selector.pas`: `CreateSecurityFirstRequirements.MinSecurityScore` tightened from `85` to `80`
  - `docs/BACKEND_SELECTION_GUIDE.md`: threshold examples updated to `80`
- focused GREEN:
  - `tests/test_backend_selector_security_first_viability.pas`: PASS, `3 passed / 0 failed`
  - `tests/test_backend_selector_basic.pas`: security-first smoke now succeeds and reports `最低安全评分要求: 80`
  - `tests/test_builder_integration.pas`: `WithSecurityFirst` path now creates a client context successfully
- residual neighbor signal:
  - `tests/test_builder_integration.pas` still prints `Server context requires a certificate` for the performance-first server-context smoke; this batch did not widen into server-certificate provisioning semantics

## Verification Plan
1. `fpc -Fu./src -Fu./tests tests/test_backend_selector_security_first_viability.pas -otmp/test_backend_selector_security_first_viability && ./tmp/test_backend_selector_security_first_viability`
2. `fpc -Fu./src -Fu./tests tests/test_backend_selector_basic.pas -otmp/test_backend_selector_basic && ./tmp/test_backend_selector_basic`
3. `fpc -Fu./src -Fu./tests tests/test_builder_integration.pas -otmp/test_builder_integration && ./tmp/test_builder_integration`
4. `git diff --check`
5. `git status --short`

# Task Plan - Backend Selector Required-Feature Truth

## Goal
修复 backend selector 对 `RequiredFeatures` 的漏判/错判，让 `sslFeatSessionCache`、`sslFeatSessionTickets`、`sslFeatRenegotiation`、`sslFeatOCSPStapling`、`sslFeatCertificateTransparency` 等必需功能真正参与筛选，并以 capability support-level 作为真相源。

## Current Batch
1. 写 focused RED 合同，证明 `RequiredFeatures = [sslFeatRenegotiation]` 时 selector 仍会接受不满足的候选，且 `RequiredFeaturesTotal` 统计错误。
2. 在 `src/fafafa.ssl.backend.selector.pas` 增加最小 feature helper，统一从 support-level 判断功能是否存在。
3. 跑 focused verification、diff hygiene，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the selector batch
- [completed] RED regression added, corrected to minimal requirement truth, and observed
- [completed] minimal selector fix implemented
- [completed] focused verification and review

## Notes
- 这批不改 capability producer，不改 serializer/diff 文档，只修 public selector 行为。
- `RequiredFeatures` 的 requirement 语义按“功能存在即可”处理：`stable / experimental / deprecated` 都算满足，只有 `none` 不满足。
- 旧布尔字段保留兼容，但 selector 这条链不再把它们当主真相。
- `TSSLBackendMatchDetails.RequiredFeaturesTotal/Matched` 当前实际统计的是所有 required 维度总数，不只是 `RequiredFeatures` 子集；focused 合同必须用最小 requirement 基线隔离 feature 参与度。

## Current Evidence
- first RED attempt exposed a test-model drift, not a production regression:
  - `CreateDefaultRequirements(optBalanced)` 自带 `TLS12/TLS13` 和最低评分门槛，掩盖了单 feature requirement 的真值
  - `RequiredFeaturesTotal/Matched` 命名偏窄，但实现会把协议/算法/平台 requirement 一起计入
- corrected focused GREEN:
  - `fpc -Fu./src -Fu./tests tests/test_backend_selector_required_feature_truth.pas -otmp/test_backend_selector_required_feature_truth && ./tmp/test_backend_selector_required_feature_truth`
  - result: PASS, `8 passed / 0 failed`
- neighbor regressions:
  - `tests/test_backend_selector_minimum_score_filtering.pas`: PASS
  - `tests/test_backend_selector_basic.pas`: compile+smoke PASS; historical informational output still says security-first selection failed on this host, but the harness has no failing assertion and exits `0`

## Verification Plan
1. `fpc -Fu./src -Fu./tests tests/test_backend_selector_required_feature_truth.pas -otmp/test_backend_selector_required_feature_truth && ./tmp/test_backend_selector_required_feature_truth`
2. `git diff --check`
3. `git status --short`

# Task Plan - Factory Connection-Scope Clarification

## Goal
修复 `TSSLConfig.BufferSize` / `TSSLConfig.HandshakeTimeout` 在 factory/context 创建路径上的静默吞配置问题，让 `TSSLFactory.CreateContext(...)` 不再接受看起来可配、实际不生效的 connection-scoped 字段。

## Current Batch
1. 写 focused RED 合同，证明 one-shot request path 和 library-default path 都会静默接受自定义 `BufferSize` / `HandshakeTimeout`。
2. 在 `src/fafafa.ssl.factory.pas` 做最小 fail-fast 作用域校验，不扩 runtime surface。
3. 跑 focused test + diff hygiene。
4. 更新 working-memory，并在 review 后提交。

## Status
- [completed] working-memory refreshed for the config-scope batch
- [completed] RED regression added and observed
- [completed] minimal factory scope validation implemented
- [completed] focused verification and review

## Notes
- 这批不去“补实现” `BufferSize` / `HandshakeTimeout`，因为当前 context/runtime surface 没有一致的消费路径。
- 修法以 fail-fast 为主，保持现有连接/后端行为不变。
- `HandshakeTimeout` 的真实替代路径是 `TSSLConnector.WithTimeout(...)` / `TSSLAcceptor.WithTimeout(...)` / `ISSLConnection.SetTimeout(...)`。
- `BufferSize` 不是 context factory 选项，应由 transport/IO 层自行管理。

## Current Evidence
- fresh RED:
  - `fpc -Fu./src -Fu./tests tests/test_factory_connection_scope_clarification.pas -otmp/test_factory_connection_scope_clarification && ./tmp/test_factory_connection_scope_clarification`
  - result before fix: 4 FAIL, all on "should raise ESSLConfigurationException"
- minimal implementation:
  - `src/fafafa.ssl.factory.pas` now rejects custom `HandshakeTimeout` / `BufferSize` on both one-shot request path and library-default path
  - `src/fafafa.ssl.debug.utils.pas` now labels both fields as non-context runtime settings in config dumps
- fresh GREEN:
  - `tests/test_factory_connection_scope_clarification.pas`: PASS, `12 passed / 0 failed`
  - `tests/test_factory_logging_scope_clarification.pas`: PASS
  - `tests/config/test_default_config.pas`: PASS
  - `git diff --check`: PASS

## Verification Plan
1. `fpc -Fu./src -Fu./tests tests/test_factory_connection_scope_clarification.pas -otmp/test_factory_connection_scope_clarification && ./tmp/test_factory_connection_scope_clarification`
2. `git diff --check`
3. `git status --short`

# Task Plan - Interface Design Audit

## Goal
对 `fafafa.ssl` 的公开 Pascal 接口做一次完整静态审查，找出设计上有问题、边界不清或与文档不一致的地方，并形成可追溯的审查结论。

## Current Batch
1. 静态核对 `src/fafafa.ssl.base.pas`、连接基类、各后端连接类、factory、context builder 和 facade。
2. 对比 `docs/ARCHITECTURE.md`、`docs/reference/INTERFACE_DESIGN_V2.md` 与源码真实 public surface。
3. 生成 `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`。
4. 保留既有工作记忆历史，只追加本次审查摘要。

## Status
- [completed] interface discovery and evidence collection
- [completed] audit report written
- [completed] diff hygiene checked with `git diff --check`
- [pending] final delivery to user

## Notes
- 本批次是静态设计审查，不改实现、不跑编译门禁。
- 主要结论：core `ISSLConnection` 过胖，context-level SNI 已弃用但仍被高层入口写入，`TSSLConfig` 跨层混用且含 inert 字段，能力矩阵存在双真相，文档承诺了源码里不存在的 `ISSLServerConnection`。

## Verification Plan
1. `git diff --check`
2. `git status --short`
3. 手工复核报告引用的关键源码/文档行号

---

# Task Plan - v1.5.0 Linux Static Audit Closeout

## Goal
把 `v1.5.0` 的正式发布收口到 Linux-only 可合并状态：本地 gate 全绿、Pascal 公共接口与实现完整、release 文档与 readiness 对齐，并在 review 后完整合回 `main`。

## Current Batch
1. 复跑 Linux release gates，确认当前仓库仍然全绿。
2. 做 Pascal 静态审查，锁住 public facade、factory API、placeholder scan 和 WinSSL 骨架测试的边界。
3. 更新 release notes、readiness report 和静态审查报告，使文档真实反映 Linux-only closeout。
4. 复核 diff hygiene，提交后将已验证分支合回 `main`。

## Status
- [completed] Linux gates green on the current branch
- [completed] static Pascal audit and docs alignment
- [completed] review, commit, and merge back to `main`

## Notes
- 这批不再把 GitHub Actions 额度不足当成阻塞项；Windows runtime proof 明确转为后续独立批次。
- `TSSLHelper` 仍然是公开辅助类；移除的是旧全局 helper 别名/函数，不是 helper 类本身。
- `src/fafafa.ssl*.pas` 里不应再有 `TODO` / `FIXME` / `skeleton` / `placeholder` 这类未完成信号。

## Verification Plan
1. `python3 scripts/compile_all_modules.py`
2. `bash scripts/run_minimal_ci_gate.sh --fast-local`
3. `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id release_1_5_0_20260512`
4. `python3 scripts/check_code_style.py src`
5. `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local`
6. `bash tests/scripts/test_release_workflow_v1_5_0_contract.sh`
7. `bash tests/scripts/test_v1_5_0_static_pascal_audit_contract.sh`
8. `git diff --check`
9. merge the verified branch back into `main`

## Definition Of Done
- Linux gates are green
- static Pascal audit is green
- release notes and readiness report match the Linux-only closeout policy
- branch is merged back into `main`

# Task Plan - v1.5.0 Release Formalization

## Goal
把 `v1.5.0` 的正式发布准备收口到可审查、可复跑、可签字的状态：先确认版本真相和本地发布门禁，再把 release workflow / release notes / README / changelog 对齐，补一份 release workflow 契约和最终 readiness 报告，最后只在用户明确批准后再打 tag。

## Current Batch
1. 复核 baseline 真相与本地 release 门禁。
2. 落地 release workflow、release notes、版本文档对齐。
3. 触发并收集 Wave B/B2 Windows runtime 证据。
4. 生成 release readiness 报告，review 后提交。

## Status
- [completed] baseline truth / local release gates
- [completed] release workflow + notes + docs alignment
- [completed] readiness report drafted and updated with remote blocker evidence
- [completed] local release-prep batch committed as `8491b91`
- [completed] pushed `glm51` and dispatched Wave B/B2
- [blocked] Windows runtime proof refresh is blocked by GitHub Actions billing/spending-limit settings
- [pending] rerun Wave B/B2 after billing access is restored, then collect artifacts

## Notes
- 版本真相已经在 `src/fafafa.ssl.base.pas`：`FAFAFA_SSL_VERSION_STRING = '1.5.0'`，`FAFAFA_SSL_INTERFACE_VERSION = 10500`
- `CHANGELOG.md`、`README.md`、`fafafa_ssl.lpk`、`RELEASE_NOTES_V1.5.0.md` 已对齐到 `v1.5.0`
- `.github/workflows/release.yml` 已启用，`.github/workflows/release.yml.disabled` 已同步成同一份当前模板
- `python3 scripts/check_code_style.py src` 首轮打出 369 个缩进错误；已按 checker 实际报错做 44 个文件 / 369 行机械缩进修复，复跑通过
- `docs/test_reports/RELEASE_READINESS_V1.5.0.md` 已生成，并已记录 GitHub run `25698425400` 的外部 billing blocker
- `glm51` 已推送；刷新 Windows 证据链需要先恢复 GitHub Actions billing/spending-limit access，或使用等价可信 Windows 主机执行同一验证链

## Verification Plan
1. `git status --short`
2. `git clean -nd`
3. `git clean -ndX`
4. `python3 scripts/compile_all_modules.py`
5. `bash scripts/run_minimal_ci_gate.sh --fast-local`
6. `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id release_1_5_0_20260512`
7. `python3 scripts/check_code_style.py src`
8. `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local`
9. `bash tests/scripts/test_release_workflow_v1_5_0_contract.sh`
10. `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
11. `bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh`
12. `bash tests/scripts/test_active_roadmap_references_contract.sh`
13. `gh workflow run .github/workflows/wave-b-b2-manual.yml --ref glm51 ...` and collect artifacts if Windows proof is still missing

## Definition Of Done
- release workflow contract passes
- docs and package version align to `v1.5.0`
- local gates are green
- Wave B/B2 Windows evidence is captured or a concrete external blocker is recorded
- `docs/test_reports/RELEASE_READINESS_V1.5.0.md` exists and says whether the batch is ready for tag approval
- batch is committed after review

---

# Task Plan - Repo Hygiene And Ignore Consolidation

## Goal
收口仓库里的 build/output 噪音，补齐 nested `tests/**/test_*` 可执行文件的 ignore 规则，并安全清理已知输出目录，让工作树保持可复现、可审查。

## Current Batch
1. 收紧根目录专属 ignore 规则，避免把归档文档里的同名文件误判成工作记忆。
2. 移除示例目录里仓库不该自带的生成型 PEM 成品。
3. 修复数字签名示例契约对 `tmp/` 已存在的隐式假设。
4. 复核状态和 diff hygiene。
5. 提交仓库整理批次。

## Status
- [completed] Inventory ignored/untracked noise and size the safe cleanup scope
- [completed] Expand ignore coverage and clean safe generated outputs
- [completed] Update working-memory records for the new hygiene batch
- [completed] Root-anchor repo-local ignore entries and drop sample key artifacts
- [completed] Make digital-signature contract create its ignored tmp parent
- [completed] Verify diff hygiene and commit

## Current Evidence
- `git clean -ndX` showed the repository had a lot of ignored build output, including:
  - `bin/` around `728M`
  - `tests/bin/` around `131M`
  - `tests/lib/` around `5.2M`
  - `examples/bin/` around `107M`
  - `artifacts/` around `1.7M`
  - `tmp/` around `6.0G`
  - `tools/test_audit/bin/` around `2.0M`
- `tests/**/test_*` was not covered by the existing top-level test-binary ignore rule, so nested generated executables could still surface as untracked files.
- The cleanup sweep removed generated output directories. The first broad pass also swept local ignored agent/config folders and `archive/`, so this batch now makes local agent/cache ignores explicit.
- After the `.gitignore` update, `git check-ignore -v` confirms nested `tests/**/test_*` executables are ignored, test sources remain visible, and benchmark report markdown stays ignored.
- `git clean -nd` only reports this new plan doc; `git clean -ndX` only reports `.agents/` and `.codex/` as ignored local caches.
- The follow-up sweep found two more repo-hygiene nits:
  - rootless `task_plan.md` / `findings.md` / `progress.md` / `WARP.md` ignore patterns can accidentally match archive docs such as `docs/archive/old_reports/PROGRESS.md`
  - `examples/digital_signature/private.pem` and `public.pem` are generated outputs that the README already instructs users to create locally
- `git check-ignore -v --no-index` now confirms the root-local working-memory files are matched only at the repo root, while `docs/archive/old_reports/PROGRESS.md` is no longer caught by those patterns.
- `tests/scripts/test_example_digital_signature_password_protected_private_key_contract.sh` now creates `tmp/` itself, so it still works after a clean artifact sweep removes the ignored directory.
- After the tmp parent fix and follow-up cleanup, `git clean -nd` and `git clean -ndX` are back to empty.

## Risks
- Do not delete local agent/config folders or archived notes.
- Do not broaden the cleanup into tracked source trees.
- Preserve the test source files and docs under `tests/**`.
- Keep the ignore rules root-scoped for repo-local files only.

## Follow-up Queue
1. Organization batch committed.

# Task Plan - Working-Memory, Artifact Hygiene, And WinSSL Workflow Closeout

## Goal
把当前工作树从“历史批次 + 本地产物残留”收口回 `HEAD` `e80100a` 的真实状态，清掉 3 个测试二进制残留，并把 `wave-b-b2-manual.yml` 的 Windows lane 对齐到 WinSSL runtime checklist。

## Current Batch
1. 清理 `tests/contract/` 与 `tests/wolfssl/` 下的无扩展名 ELF 测试产物。
2. 新增本批 plan 文档，作为可恢复的工作记忆。
3. 把 `task_plan.md` / `findings.md` / `progress.md` 顶部对齐到当前真相。
4. 修复 `test_wave_b_b2_windows_runtime_workflow_contract.sh` 打出的 workflow RED。
5. 复跑 focused contracts、diff hygiene，并提交。

## Status
- [completed] Freeze current state and remove generated test binaries
- [completed] Resync working-memory files to current HEAD and next queue
- [completed] Align Wave B/B2 Windows workflow to the runtime checklist
- [completed] Verify diff hygiene and record results
- [pending] Commit the batch

## Current Evidence
- `git log --oneline -1` shows current `HEAD` as `e80100a fix: batch 6 - compiler warning reduction and capabilities contract test`
- `git status --short` initially listed only three untracked ELF test binaries:
  - `tests/contract/test_capabilities_contract`
  - `tests/wolfssl/test_wolfssl_connection_contract`
  - `tests/wolfssl/test_wolfssl_context_contract`
- `file` confirmed those artifacts are Linux ELF executables, not source files
- `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh` produced a real RED: the workflow did not install or verify Lazarus / `lazbuild`
- current product-side blocker remains real Windows runtime evidence, but the GitHub Actions lane can now be made capable of collecting it

## Risks
- Do not reopen `src/fafafa.ssl.winssl.*` in this batch.
- Do not treat workflow readiness as a substitute for Windows runtime proof.
- Keep the batch narrow so the commit stays reviewable.

## Follow-up Queue
1. Commit the closeout batch.
2. Trigger the updated `wave-b-b2-manual.yml` on GitHub Actions when a real Windows runtime proof run is needed.

# Task Plan - Wave B/B2 WinSSL Runtime Workflow Alignment

## Goal
把当前手动 Windows CI workflow 对齐到最新 `WinSSL` runtime checklist，让仓库在没有本地 Windows 主机时，仍能通过 `wave-b-b2-manual.yml` 去推进 quick smoke、Wave B gate、broader suite 这条真实运行时证据链。

## Current Batch
1. 用 focused contract 证明当前 `wave-b-b2-manual.yml` 的 Windows job 仍低于最新 runtime checklist。
2. 最小修改 workflow / docs，把 quick smoke、Wave B gate、broader suite transcript 接进现有 Windows lane。
3. 复跑 focused contracts，并更新计划/台账。

## Status
- [pending] RED workflow contract for Windows runtime checklist alignment
- [pending] Align wave-b-b2-manual Windows lane
- [pending] Re-run workflow + bundle contracts
- [pending] Record evidence and commit the batch

## Current Evidence
- fresh broad completion audit 已证明：
  - `tests/contract/test_backend_contract.pas`：`135 total / 111 passed / 0 failed / 24 skipped`
  - `tests/test_capability_cache.pas`：`FreePascal` / `WolfSSL` / `MbedTLS` wording truth 全绿
  - `python3 scripts/compile_all_modules.py`：`185/185`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id broad_completion_audit_20260505`：`17 passed / 0 failed`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`：PASS
  - `WinSSL` source contract / bundle contract：全部 PASS
- fresh Win64 cross-target compile 已补齐：
  - `tests/winssl/test_winssl_session_management.pas` 可成功交叉编译到 Win64
  - `tests/integration/test_backend_comparison.pas` 可成功交叉编译到 Win64
- 当前 Linux 主机环境边界已确认：
  - `command -v pwsh`：空 / exit `1`
  - `wine --version`：exit `159`
- 结论：
  - Linux 侧 public surface、capability truth、repo gates、source contract、Win64 compile proof 都已闭合
  - 唯一未闭合 requirement 是真实 Windows 主机上的 `WinSSL` runtime proof
- 当前仓库已有的 Windows CI 入口是 `.github/workflows/wave-b-b2-manual.yml`，但从源码可见：
  - 只跑 `scripts/run_wave_b_windows_gate.ps1`
  - 还没显式安装/验证 `lazbuild`
  - 还没把 quick smoke 和 broader suite transcript 纳入 artifact
- 因此“有 Windows workflow”还不等于“这条 workflow 已覆盖当前 runtime checklist”。

## Risks
- 这批只能把 CI lane 对齐到 checklist，不能替代真实 Windows runtime 结果本身。
- 如果 workflow 只补命令不补 artifact，后续仍然没法做可审查闭环。
- 如果只改 `.yml` 不同步 `.disabled` 模板和文档，后续容易再次漂移。

## Follow-up Queue
1. 触发对齐后的 `wave-b-b2-manual.yml` Windows lane。
2. 审查 quick smoke / Wave B / broader suite artifacts。
3. 只有当这些 Windows runtime 结果真实返回后，才继续判断 broad objective 是否可标记完成。
