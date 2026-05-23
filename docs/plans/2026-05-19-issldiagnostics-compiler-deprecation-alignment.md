# `ISSLDiagnostics` Compiler Deprecation Alignment

## Goal

把 `ISSLConnection.GetHealthStatus` / `IsHealthy` / `GetDiagnosticInfo` /
`GetPerformanceMetrics` 从“owner path 已明确、ordinary docs/tests 已转向
`ISSLDiagnostics`”继续收成真正的 compiler-level compatibility-only surface：
源码声明进入 `deprecated`，参考文档同步记录，cross-backend contract 保留一条
mirror proof，WinSSL 专项测试继续走 owner path。

## Scope

本批只处理 compiler-surface / source-doc-test alignment，不改 runtime 语义：

- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.connection.base.pas`
- `docs/reference/API_REFERENCE.md`
- `docs/reference/INTERFACE_DESIGN_V2.md`
- `tests/contract/test_backend_contract.pas`
- `tests/winssl/test_winssl_connection_edge_cases.pas`
- `tests/winssl/test_winssl_monitoring.pas`
- `tests/scripts/test_issldiagnostics_compiler_deprecated_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不改 `ISSLDiagnostics` runtime 行为
- 不重开 diagnostics completion audit
- 不做更大的 `ISSLConnection` interface surgery

## Why This Batch

当前 repo 真相已经走到这一步：

- active docs/tests 已经优先走 `ISSLDiagnostics`
- `ISSLDiagnostics` cross-backend completion audit 已经存在
- `GetConnectionInfo` / `GetContext` / `GetStateString` /
  `GetSelectedALPNProtocol` / `GetVerifyResult*` 都已经进入
  compiler-deprecated compatibility mirror

所以 diagnostics 这组方法当前真正缺的，不是再做 runtime proof，而是补齐最后一层
compiler-surface truth。

## Planned Changes

1. 在 `src/fafafa.ssl.base.pas` 中把四个 diagnostics core getter 标成
   compiler `deprecated`，统一导向 `ISSLDiagnostics` owner path。
2. 在 `src/fafafa.ssl.connection.base.pas` 中补 diagnostics residual note，
   明确 direct core 只剩 contract mirror proof。
3. 在 `API_REFERENCE.md` / `INTERFACE_DESIGN_V2.md` 中把 diagnostics core
   getter 明确记录为编译期 deprecated compatibility mirror。
4. 在 `tests/contract/test_backend_contract.pas` 中新增 direct-core diagnostics
   mirror proof，并做局部 warning quarantine。
5. 把 `tests/winssl/test_winssl_connection_edge_cases.pas` /
   `tests/winssl/test_winssl_monitoring.pas` 切到 `ISSLDiagnostics` owner path，
   让 direct core diagnostics 只保留 backend contract mirror proof。
6. 新增 focused shell contract，锁住 source/doc/mirror-proof/residual-allowlist
   truth。

## Verification

```bash
bash -n tests/scripts/test_issldiagnostics_compiler_deprecated_contract.sh
bash tests/scripts/test_issldiagnostics_compiler_deprecated_contract.sh
bash tests/scripts/test_issldiagnostics_active_guidance_contract.sh
mkdir -p tmp/test_backend_contract_diagnostics_deprecation && \
  fpc -B -Fu./src -Fu./tests -FUtmp/test_backend_contract_diagnostics_deprecation \
  -FEtmp/test_backend_contract_diagnostics_deprecation \
  -otmp/test_backend_contract_diagnostics_deprecation/test_backend_contract \
  tests/contract/test_backend_contract.pas && \
  ./tmp/test_backend_contract_diagnostics_deprecation/test_backend_contract
git diff --check
```

## Expected Outcome

- diagnostics core getter 在 source/doc/compiler 三层都被明确为
  compatibility-only mirror
- active docs/tests 继续优先走 `ISSLDiagnostics`
- direct core diagnostics 只保留 cross-backend contract mirror proof
- 这条 diagnostics route 后续不再反复停留在“owner path 已有但 core 还像主入口”
  的中间态

## Execution Result

- PASS.
- Revalidated `tests/scripts/test_issldiagnostics_compiler_deprecated_contract.sh`
  and `tests/scripts/test_issldiagnostics_active_guidance_contract.sh`.
- Moved WinSSL monitoring / edge-case diagnostics checks to `ISSLDiagnostics`
  owner path.
- Rebuilt and ran `tests/contract/test_backend_contract.pas` with the absolute FPC path.
- Linux-host compile attempts for the WinSSL test units stop at `fafafa.ssl.winssl.certificate.pas`
  because the `Windows` unit is unavailable here.
