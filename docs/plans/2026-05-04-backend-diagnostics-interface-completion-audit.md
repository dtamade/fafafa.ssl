# Backend Diagnostics Interface Completion Audit Plan

**Goal:** 把连接级 `ISSLDiagnostics` public surface 纳入 cross-backend completion audit，确认五个 backend 在公开连接对象上都暴露同一组诊断接口，并保持最基本的字段自洽。

**Architecture:** 这批优先做 completion audit，不预设生产代码改动。`ISSLDiagnostics` 当前由 `TBaseSSLConnection` 统一实现，因此先在 `tests/contract/test_backend_contract.pas` 新增一个跨后端 contract，锁住三类真相：
- `Supports(LConn, ISSLDiagnostics, ...)` 必须成功
- `GetHealthStatus` / `GetPerformanceMetrics` / `GetDiagnosticInfo` 不应抛异常
- 诊断结果必须和同一连接对象的基础状态保持最小自洽

只有 focused RED 证明某个 backend 连接没有真正走到基类统一语义时，才改对应 connection 实现。

**Files:**
- Modify: `tests/contract/test_backend_contract.pas`
- Modify: `src/fafafa.ssl.connection.base.pas` (only if RED proves shared diagnostics drift)
- Modify: `src/fafafa.ssl.openssl.connection.pas` (only if RED proves backend-specific drift)
- Modify: `src/fafafa.ssl.wolfssl.connection.pas` (only if RED proves backend-specific drift)
- Modify: `src/fafafa.ssl.mbedtls.connection.pas` (only if RED proves backend-specific drift)
- Modify: `src/fafafa.ssl.freepascal.connection.pas` (only if RED proves backend-specific drift)
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED/Audit - prove diagnostics surface truth

Run:

```bash
mkdir -p tmp/backend_contract_units
fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas
./tmp/backend_contract_units/test_backend_contract
```

Add checks:
- 所有可用 backend 的 connection 都应支持 `ISSLDiagnostics`
- `HealthStatus.IsConnected` 应与 `LConn.IsConnected` 一致
- `IsHealthy` 应与 `HealthStatus` 的布尔条件一致
- `DiagnosticInfo.HealthStatus` / `DiagnosticInfo.PerformanceMetrics` 应与直接 getter 保持一致

## Task 2: GREEN - only if the new contract exposes real drift

Possible change directions:
- 若某个 backend connection 没有真正暴露 `ISSLDiagnostics`，收口其类声明或继承边界
- 若共享基类 getter 不自洽，优先修 `TBaseSSLConnection`

Constraints:
- 不扩大到 `ISSLSessionResumption`
- 不扩大到 `ISSLCertificateVerification`
- 不扩大到 `ISSLConnectionInfo`

## Task 3: Verification

Run:

```bash
mkdir -p tmp/backend_contract_units
fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas
./tmp/backend_contract_units/test_backend_contract
python3 scripts/compile_all_modules.py
bash scripts/run_minimal_ci_gate.sh --fast-local
```

## Definition Of Done

- `ISSLDiagnostics` 已被 cross-backend contract 锁住
- 若无 RED，这批作为 completion audit 证据提交
- 若有 RED，最小修复完成且 focused / repo gates 全绿

## Focused Revalidation Result (2026-05-18)

- `tests/contract/test_backend_contract.pas` 当前已包含 `Contract 18: Diagnostics interface alignment`
- focused revalidation command：
  - `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
- 结果：
  - `Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
  - OpenSSL / WolfSSL / MbedTLS / FreePascal 的 diagnostics self-consistency contract PASS
  - WinSSL 在当前 Linux 主机继续按平台边界 SKIP
- 本批没有打出 implementation drift，因此不改 `src/` 下 diagnostics 相关实现
- 说明：
  - 本次只补 focused revalidation 证据，不重复重跑 `compile_all_modules.py` / `run_minimal_ci_gate.sh --fast-local`
