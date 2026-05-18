# `GetConnectionInfo` Compiler Deprecation Alignment

## Goal

把 `ISSLConnection.GetConnectionInfo` 从“source/doc 已经明确是 compatibility mirror”继续收成真正的源码级 compatibility-only surface：

- `ISSLConnection.GetConnectionInfo` declaration 进入编译期 `deprecated`
- intentional direct-core residual tests 做局部 warning quarantine
- focused contracts 守住 declaration / doc wording / residual allowlist 这三层真相

## Scope

- `src/fafafa.ssl.base.pas`
- `docs/reference/API_REFERENCE.md`
- `docs/reference/INTERFACE_DESIGN_V2.md`
- `tests/contract/test_backend_contract.pas`
- `tests/winssl/test_winssl_connection_info.pas`
- `tests/winssl/test_winssl_connection_edge_cases.pas`
- `tests/scripts/test_getconnectioninfo_compiler_deprecated_contract.sh`
- `tests/scripts/test_getconnectioninfo_public_wording_deemphasis_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不改 runtime 行为
- 不改 `ISSLConnectionInfo` owner path
- 不重跑重型 repo gate

## Planned Changes

1. 给 `ISSLConnection.GetConnectionInfo` 加编译期 `deprecated` 声明，迁移消息统一导向 `ISSLConnectionInfo.GetConnectionInfo`。
2. 把 active docs / v2 migration wording 升级成“源码声明已是编译期 deprecated”。
3. 给剩余 intentional direct-core `GetConnectionInfo` 测试加局部 warning quarantine，保持 compile noise 可控。
4. 新增 focused shell contract，守住：
   - compiler-deprecated declaration
   - docs wording
   - residual intentional test 的 warning suppression

## Verification

```bash
bash tests/scripts/test_getconnectioninfo_compiler_deprecated_contract.sh
bash tests/scripts/test_getconnectioninfo_public_wording_deemphasis_contract.sh
bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh
mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract
git diff --check
```

## Expected Outcome

- `GetConnectionInfo` 在 source/doc/compiler 三层都明确成为 compatibility-only mirror
- 现存 intentional direct-core residual tests 不会因为新 deprecation 而重新放大 compile noise
- 这条 route 之后不再需要继续做 wording/compiler archaeology，可以开始真正切到下一条 mirror / slimming 选择

## Result

- `src/fafafa.ssl.base.pas` 现在把 `ISSLConnection.GetConnectionInfo` 声明成：
  - `deprecated 'Use ISSLConnectionInfo.GetConnectionInfo'`
- `API_REFERENCE` 与 `INTERFACE_DESIGN_V2` 现在都明确写出：
  - `GetConnectionInfo` 在 core 上仅兼容保留
  - 当前源码声明已经是编译期 `deprecated`
- residual direct-core tests 现在都带局部 warning quarantine：
  - `tests/contract/test_backend_contract.pas`
  - `tests/winssl/test_winssl_connection_info.pas`
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
- 新增 focused source contract：
  - `tests/scripts/test_getconnectioninfo_compiler_deprecated_contract.sh`

## Route Impact

- 这批之后，`GetConnectionInfo` 的第一条真正 public slimming slice 已经落地
- 默认下一步不该再重复做这条 getter 的 wording/deprecation 清扫
- 若继续沿 mirror 路线推进，应转去下一条 mirror 的 feasibility / slimming 选择
