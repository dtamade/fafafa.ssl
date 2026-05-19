# `ISSLSessionResumption` Compiler Deprecation Alignment

## Goal

把 `ISSLConnection.GetSession` / `SetSession` / `IsSessionReused` 从
“owner path 已明确、active docs/tests 已转向 `ISSLSessionResumption`”继续收成真正的
compiler-level compatibility-only surface：源码声明进入 `deprecated`，参考文档同步记录，
cross-backend contract 保留 direct-core mirror proof。

## Scope

本批只处理 compiler-surface / source-doc-test alignment，不改 runtime 语义：

- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.connection.base.pas`
- `docs/reference/API_REFERENCE.md`
- `docs/reference/INTERFACE_DESIGN_V2.md`
- `tests/contract/test_backend_contract.pas`
- `tests/scripts/test_isslsessionresumption_compiler_deprecated_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不改 `ISSLSessionResumption` runtime 行为
- 不重开 session-resumption runtime completeness 审查
- 不做更大的 `ISSLConnection` interface surgery

## Why This Batch

当前 repo 真相已经走到这一步：

- active docs/tests 已经优先走 `ISSLSessionResumption`
- `ISSLSessionResumption` cross-backend completion contract 已经存在
- `GetConnectionInfo` / `GetContext` / `GetSelectedALPNProtocol` /
  `GetStateString` / `GetVerifyResult*` / diagnostics mirrors 都已经进入
  compiler-deprecated compatibility mirror

所以 session-resumption 这组方法当前真正缺的，不是再做 runtime proof，而是补齐最后一层
compiler-surface truth。

## Planned Changes

1. 在 `src/fafafa.ssl.base.pas` 中把 3 个 session-resumption core mirror 标成
   compiler `deprecated`，统一导向 `ISSLSessionResumption` owner path。
2. 在 `src/fafafa.ssl.connection.base.pas` 中补 session-resumption residual note，
   明确 ordinary docs/tests 已转向 owner path。
3. 在 `API_REFERENCE.md` / `INTERFACE_DESIGN_V2.md` 中把 session-resumption core
   getter/setter 明确记录为编译期 deprecated compatibility mirror。
4. 在 `tests/contract/test_backend_contract.pas` 中为 direct-core session mirror proof
   补局部 warning quarantine。
5. 新增 focused shell contract，锁住 source/doc/contract mirror truth。

## Verification

```bash
bash -n tests/scripts/test_isslsessionresumption_compiler_deprecated_contract.sh
bash tests/scripts/test_isslsessionresumption_compiler_deprecated_contract.sh
bash tests/scripts/test_isslsessionresumption_active_guidance_contract.sh
mkdir -p tmp/test_backend_contract_session_resumption_deprecation && \
  fpc -B -Fu./src -Fu./tests -FUtmp/test_backend_contract_session_resumption_deprecation \
  -FEtmp/test_backend_contract_session_resumption_deprecation \
  -otmp/test_backend_contract_session_resumption_deprecation/test_backend_contract \
  tests/contract/test_backend_contract.pas && \
  ./tmp/test_backend_contract_session_resumption_deprecation/test_backend_contract
git diff --check
```

## Expected Outcome

- session-resumption core mirrors 在 source/doc/compiler 三层都被明确为
  compatibility-only mirror
- active docs/tests 继续优先走 `ISSLSessionResumption`
- cross-backend contract 继续保留一条 direct-core mirror proof
- 这条 session-resumption route 后续不再反复停留在
  “owner path 已有但 core 还像主入口” 的中间态
