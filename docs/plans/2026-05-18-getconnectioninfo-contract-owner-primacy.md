# `GetConnectionInfo` Contract Owner Primacy

## Goal

把 `GetConnectionInfo` 从“direct core getter 仍在扩散使用”的状态，收回到当前路线图已经声明的 owner/mirror 结构：

- `ISSLConnectionInfo.GetConnectionInfo` 是默认 owner
- `ISSLConnection.GetConnectionInfo` 只保留为 `v1.x` compatibility-core mirror

## Scope

- `tests/contract/test_backend_contract.pas`
- `tests/scripts/test_isslconnectioninfo_getconnectioninfo_contract_owner_contract.sh`
- `tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
- `tests/test_connection_builder_hostname_precedence.pas`
- `tests/test_openssl_connection_info_cipher_contract.pas`
- `tests/test_wolfssl_connection_info_macsize_contract.pas`
- `tests/test_mbedtls_connection_info_ciphersuite_contract.pas`
- `tests/test_freepascal_server_accept_skeleton.pas`
- `tests/test_freepascal_client_session_resumption.pas`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不改生产 `GetConnectionInfo` 实现
- 不重开 backend completeness helper 审查
- 不重跑重型门禁

## Planned Changes

1. 把 `Contract 19` 改成 owner-primacy 叙事：
   - 先走 `ISSLConnectionInfo.GetConnectionInfo`
   - 再验证 core getter 只是 mirror
2. 把新近 completeness / proof 测试改成优先通过 `ISSLConnectionInfo` 读取 connection info。
3. 收缩 residual allowlist：
   - direct core `GetConnectionInfo` 只保留在真正需要 mirror/core-surface proof 的文件

## Verification

```bash
bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_contract_owner_contract.sh
bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh
mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract
git diff --check
```

## Expected Outcome

- `GetConnectionInfo` contract 层明确承认 `ISSLConnectionInfo` owner primacy
- 新增 completeness proof 不再继续扩大 direct core residual surface
- 路线图可以更自然地切向更强的 wording / slimming route，而不是被 stale allowlist 拖住

## Result

- `Contract 19` 现在已改成 owner-primacy：
  - 先验证 `ISSLConnectionInfo.GetConnectionInfo`
  - 再验证 `ISSLConnection.GetConnectionInfo` 只是 mirror
- residual direct-core `GetConnectionInfo` surface 现在已收缩到 5 个命中，只剩：
  - `tests/contract/test_backend_contract.pas`
  - `tests/winssl/test_winssl_connection_info.pas`
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
- FreePascal / OpenSSL / WolfSSL / MbedTLS 的 completeness proof 与 shared builder proof 都已切到 `ISSLConnectionInfo` owner path
- focused verification 已通过：
  - 2 个 shell contracts
  - `tests/contract/test_backend_contract.pas`
  - `tests/test_connection_builder_hostname_precedence.pas`
  - `tests/test_freepascal_server_accept_skeleton.pas`
  - `tests/test_freepascal_client_session_resumption.pas`
  - `tests/test_mbedtls_connection_info_ciphersuite_contract.pas`
  - `tests/test_openssl_connection_info_cipher_contract.pas`
  - `tests/test_wolfssl_connection_info_macsize_contract.pas`

## Route Impact

- 这批之后，`GetConnectionInfo` 不再卡在 stale residual allowlist 上
- 默认下一步应进入更强的 owner / deprecation wording route
- 若还想继续收 residual，只值得继续审：
  - WinSSL direct-core `GetConnectionInfo` tests 是否属于 intentional core-surface proof
