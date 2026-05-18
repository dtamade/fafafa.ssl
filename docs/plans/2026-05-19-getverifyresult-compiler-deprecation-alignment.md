# `GetVerifyResult` / `GetVerifyResultString` Compiler Deprecation Alignment

## Goal

把 `ISSLConnection.GetVerifyResult` / `GetVerifyResultString` 从“owner path 与 residual 分类已经清楚”继续收成真正的 compiler-level compatibility-only surface：源码声明进入 `deprecated`，参考文档同步记录，intentional fallback / residual proof 做局部 warning quarantine。

## Scope

本批只处理 compiler-surface 对齐，不改 runtime 语义：

- `src/fafafa.ssl.base.pas`
- `docs/reference/API_REFERENCE.md`
- `docs/reference/INTERFACE_DESIGN_V2.md`
- `src/fafafa.ssl.connection.builder.pas`
- `src/fafafa.ssl.tls.pas`
- `examples/fafafa.examples.tcp.pas`
- `tests/connection/test_ssl_client_connection.pas`
- `tests/contract/test_backend_contract.pas`
- 当前 verify-result residual allowlist 下的 intentional proof 文件
- `tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不改 `ISSLCertificateVerification` owner path
- 不重开 residual archaeology
- 不修改 backend 实现逻辑

## Why This Batch

当前 repo 真相已经足够清楚：

- ordinary docs / generic examples / generic tests 已切到 `ISSLCertificateVerification`
- direct core residual file set 已完成 subgroup freeze
- `GetConnectionInfo` / `GetContext` / `GetStateString` / `GetSelectedALPNProtocol` 都已经走完 compiler-deprecated alignment

所以 `GetVerifyResult` / `GetVerifyResultString` 现在真正缺的，不是继续扫 residual 文件，而是最后一层 compiler-surface truth。

## Planned Changes

1. 在 `src/fafafa.ssl.base.pas` 中把 `ISSLConnection.GetVerifyResult` / `GetVerifyResultString` 标成 compiler `deprecated`。
2. 在 `API_REFERENCE.md` / `INTERFACE_DESIGN_V2.md` 中把这两个 core getter 明确记录为编译期 deprecated compatibility mirror。
3. 给 intentional fallback / residual proof 文件补 local/file-scoped warning quarantine，避免 compile noise。
4. 新增 focused shell contract，锁住 source/doc/deprecation-quarantine truth。

## Verification

```bash
bash -n tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh
bash tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh
bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh
bash tests/scripts/test_isslcertificateverification_generic_examples_contract.sh
mkdir -p tmp/test_connection_builder_hostname_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_builder_hostname_precedence -FEtmp/test_connection_builder_hostname_precedence -otmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/test_connection_builder_hostname_precedence/test_connection_builder_hostname_precedence
mkdir -p tmp/test_tls_connector_hostname_override_precedence && fpc -B -Fu./src -Fu./tests -FUtmp/test_tls_connector_hostname_override_precedence -FEtmp/test_tls_connector_hostname_override_precedence -otmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence tests/test_tls_connector_hostname_override_precedence.pas && ./tmp/test_tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence
mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract
git diff --check
```

## Expected Outcome

- `GetVerifyResult` / `GetVerifyResultString` 在 source/doc/compiler 三层都被明确为 compatibility-only mirror
- intentional fallback / residual proof compile noise 被局部 quarantine
- 这条 verify-result route 不再需要继续做 wording / residual archaeology
