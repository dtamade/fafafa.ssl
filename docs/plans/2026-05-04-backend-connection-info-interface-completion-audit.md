# Backend Connection-Info Interface Completion Audit Plan

**Goal:** 把连接级 `ISSLConnectionInfo` public surface 纳入 cross-backend completion audit，确认各 backend 的公开 connection 都暴露该接口，并且返回的连接信息与同一对象上的 direct getter 保持最小自洽。

**Architecture:** 这批优先做 completion audit，不预设生产代码改动。`ISSLConnectionInfo` 当前由 `TBaseSSLConnection` 统一实现，少数 backend 会覆盖 `GetConnectionInfo` 或 `GetStateString`。因此先在 `tests/contract/test_backend_contract.pas` 新增跨后端 contract，锁住四组真相：
- `Supports(LConn, ISSLConnectionInfo, ...)` 必须成功
- `GetConnectionInfo.ProtocolVersion` 必须与 `LConn.GetProtocolVersion` 一致
- `GetConnectionInfo.CipherSuite` 必须与 `LConn.GetCipherName` 一致
- `GetConnectionInfo.ALPNProtocol` / `GetContext` / `GetStateString` 必须与 direct getter 或创建时上下文保持最小自洽

只有 focused RED 证明某个 backend 的 override 偏离了共享语义，才改对应 connection 实现。

**Files:**
- Modify: `tests/contract/test_backend_contract.pas`
- Modify: `src/fafafa.ssl.connection.base.pas` (only if RED proves shared drift)
- Modify: `src/fafafa.ssl.openssl.connection.pas` (only if RED proves override drift)
- Modify: `src/fafafa.ssl.winssl.connection.pas` (only if RED proves override drift)
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED/Audit - prove connection-info surface truth

Run:

```bash
mkdir -p tmp/backend_contract_units
fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas
./tmp/backend_contract_units/test_backend_contract
```

Add checks:
- 所有可用 backend 的 connection 都应支持 `ISSLConnectionInfo`
- `ConnectionInfo.ProtocolVersion` 与 `LConn.GetProtocolVersion` 一致
- `ConnectionInfo.CipherSuite` 与 `LConn.GetCipherName` 一致
- `ConnectionInfo.ALPNProtocol` 与 `GetSelectedALPNProtocol` 一致
- `GetContext` 不能为 `nil`，且 `GetContextType` 与创建时 context 一致
- `GetStateString` 不能是空字符串

## Task 2: GREEN - only if the new contract exposes real drift

Possible change directions:
- 若某个 backend connection 未真正暴露 `ISSLConnectionInfo`，收口其类声明或继承边界
- 若 override 的 `GetConnectionInfo` / `GetStateString` 破坏自洽，收口对应 override

Constraints:
- 不扩大到 `ISSLSessionResumption`
- 不扩大到 `ISSLCertificateVerification`
- 不扩大到 runtime handshake proof

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

- `ISSLConnectionInfo` 已被 cross-backend contract 锁住
- 若无 RED，这批作为 completion audit 证据提交
- 若有 RED，最小修复完成且 focused / repo gates 全绿

## Execution Result

- `tests/contract/test_backend_contract.pas` 已新增 `Contract 19: Connection-info interface alignment`
- focused contract 结果：`Total Tests: 125 / Passed: 103 / Failed: 0 / Skipped: 22`
- `python3 scripts/compile_all_modules.py`：`185/185`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`：`[PASS]`
- 本批没有打出 backend implementation drift，因此不改 `src/` 下任何 connection 实现，按纯 completion audit 收口
- 后续队列继续推进 `ISSLSessionResumption`，`ISSLCertificateVerification` 保持下一层独立批次
