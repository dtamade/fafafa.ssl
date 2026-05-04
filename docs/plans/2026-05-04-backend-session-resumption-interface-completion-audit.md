# Backend Session-Resumption Interface Completion Audit Plan

**Goal:** 把连接级 `ISSLSessionResumption` public surface 纳入 cross-backend completion audit，确认各 backend 的公开 connection 都暴露该接口，并且会话 getter / reused 状态与 core `ISSLConnection` 保持最小自洽。

**Architecture:** 这批优先做 completion audit，不预设生产代码改动。`ISSLSessionResumption` 当前由 `TBaseSSLConnection` 统一实现，而核心 `ISSLConnection` 也已经暴露 `GetSession` / `SetSession` / `IsSessionReused`。因此 contract 的重点不是证明“真的跨连接恢复成功”，而是锁住 optional interface truth：
- `Supports(LConn, ISSLSessionResumption, ...)` 必须成功
- `ISSLSessionResumption.IsSessionReused` 必须与 `ISSLConnection.IsSessionReused` 一致
- `ISSLConnection.GetConnectionInfo.IsResumed` 必须与 `IsSessionReused` 一致
- `ISSLSessionResumption.GetSession` 与 core `GetSession` 在同一 connection 对象上必须保持最小自洽

只有 focused RED 证明某个 backend 的 `DoGetSession` / `DoIsSessionReused` / `DoSetSession` 偏离共享语义，才改对应 connection 实现。

**Files:**
- Modify: `tests/contract/test_backend_contract.pas`
- Modify: `src/fafafa.ssl.connection.base.pas` (only if RED proves shared drift)
- Modify: `src/fafafa.ssl.openssl.connection.pas` (only if RED proves backend drift)
- Modify: `src/fafafa.ssl.mbedtls.connection.pas` (only if RED proves backend drift)
- Modify: `src/fafafa.ssl.wolfssl.connection.pas` (only if RED proves backend drift)
- Modify: `src/fafafa.ssl.freepascal.connection.pas` (only if RED proves backend drift)
- Modify: `src/fafafa.ssl.winssl.connection.pas` (only if RED proves backend drift)
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED/Audit - prove session-resumption surface truth

Run:

```bash
mkdir -p tmp/backend_contract_units
fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas
./tmp/backend_contract_units/test_backend_contract
```

Add checks:
- 所有可用 backend 的 connection 都应支持 `ISSLSessionResumption`
- `IsSessionReused` 与 core getter 一致
- `GetConnectionInfo.IsResumed` 与 `IsSessionReused` 一致
- `GetSession` 的 nil/non-nil 结果与 core getter 一致
- 若 `GetSession` 非 `nil`，则 `IsValid` / `IsResumable` / `ProtocolVersion` / `CipherName` / `Timeout` / `PeerCertificate` nilness 与 core getter 保持自洽

## Task 2: GREEN - only if the new contract exposes real drift

Possible change directions:
- 若某个 backend connection 未真正暴露 `ISSLSessionResumption`，收口其类声明或继承边界
- 若 `DoGetSession` / `DoIsSessionReused` 在 optional/core surface 上不自洽，收口对应 backend connection

Constraints:
- 不扩大到 `ISSLCertificateVerification`
- 不扩大到真实跨连接 session reuse runtime 证明
- 不扩大到 session cache/ticket 策略重构

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

- `ISSLSessionResumption` 已被 cross-backend contract 锁住
- 若无 RED，这批作为 completion audit 证据提交
- 若有 RED，最小修复完成且 focused / repo gates 全绿

## Execution Result

- `tests/contract/test_backend_contract.pas` 已新增 `Contract 20: Session-resumption interface alignment`
- focused contract 结果：`Total Tests: 130 / Passed: 107 / Failed: 0 / Skipped: 23`
- `python3 scripts/compile_all_modules.py`：`185/185`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`：`[PASS]`
- 本批没有打出 backend implementation drift，因此不改 `src/` 下任何 connection 实现，按纯 completion audit 收口
- 后续队列继续推进 `ISSLCertificateVerification`
