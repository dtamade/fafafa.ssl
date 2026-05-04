# Backend Certificate-Verification Interface Completion Audit Plan

**Goal:** 把连接级 `ISSLCertificateVerification` public surface 纳入 cross-backend completion audit，确认各 backend 的公开 connection 都暴露该接口，并且证书链 / verify getter 与 core `ISSLConnection` 保持最小自洽。

**Architecture:** 这批优先做 completion audit，不预设生产代码改动。`ISSLCertificateVerification` 当前由 `TBaseSSLConnection` 统一实现，而核心 `ISSLConnection` 也已经暴露 `GetPeerCertificateChain` / `GetVerifyResult` / `GetVerifyResultString`。因此 contract 的重点不是证明“证书验证逻辑完全正确”，而是锁住 optional interface truth：
- `Supports(LConn, ISSLCertificateVerification, ...)` 必须成功
- `GetVerifyResult` 必须与 core getter 一致
- `GetVerifyResultString` 必须与 core getter 一致
- `GetPeerCertificateChain` 与 core getter 在同一 connection 对象上必须保持最小自洽

只有 focused RED 证明某个 backend 的 `DoGetPeerCertificateChain` / `DoGetVerifyResult` / `DoGetVerifyResultString` 偏离共享语义，才改对应 connection 实现。

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

## Task 1: RED/Audit - prove certificate-verification surface truth

Run:

```bash
mkdir -p tmp/backend_contract_units
fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas
./tmp/backend_contract_units/test_backend_contract
```

Add checks:
- 所有可用 backend 的 connection 都应支持 `ISSLCertificateVerification`
- `GetVerifyResult` 与 core getter 一致
- `GetVerifyResultString` 与 core getter 一致
- `GetPeerCertificateChain` 的长度与 core getter 一致
- 若链非空，则每个元素的 nilness、`Subject`、`Issuer`、`SerialNumber` 与 core getter 对应项保持自洽

## Task 2: GREEN - only if the new contract exposes real drift

Possible change directions:
- 若某个 backend connection 未真正暴露 `ISSLCertificateVerification`，收口其类声明或继承边界
- 若 `DoGetPeerCertificateChain` / `DoGetVerifyResult` / `DoGetVerifyResultString` 在 optional/core surface 上不自洽，收口对应 backend connection

Constraints:
- 不扩大到 runtime certificate validation parity
- 不扩大到 OCSP/CRL/CT/hostname 语义
- 不扩大到 trust-store 或 chain-building 重构

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

- `ISSLCertificateVerification` 已被 cross-backend contract 锁住
- 若无 RED，这批作为 completion audit 证据提交
- 若有 RED，最小修复完成且 focused / repo gates 全绿

## Execution Result

- `tests/contract/test_backend_contract.pas` 已新增 `Contract 21: Certificate-verification interface alignment`
- focused contract 结果：`Total Tests: 135 / Passed: 111 / Failed: 0 / Skipped: 24`
- `python3 scripts/compile_all_modules.py`：`185/185`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`：`[PASS]`
- 本批没有打出 backend implementation drift，因此不改 `src/` 下任何 connection 实现，按纯 completion audit 收口
- 当前这轮明确排队的 connection optional public surface 已收尽，后续更适合回到总盘点
