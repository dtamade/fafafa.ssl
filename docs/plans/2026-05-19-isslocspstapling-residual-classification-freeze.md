# `ISSLOCSPStapling` Residual Classification Freeze

## Goal

把 `GetOCSPStaplingEnabled` / `GetOCSPResponse` / `IsOCSPResponseVerified` / `GetOCSPResponseStatus` 当前剩余的 direct core usage 冻成清晰 allowlist：ordinary guidance 继续走 `ISSLOCSPStapling` owner path，而 core getter 只允许保留在 backend-specific runtime / contract proof 中。

## Scope

本批只处理 source comments、focused source contract、intent 标注与台账：

- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.connection.base.pas`
- `docs/reference/API_REFERENCE.md`
- `tests/mbedtls/test_mbedtls_ocsp_capability.pas`
- `tests/openssl/test_ocsp_connection_verification_regression.pas`
- `tests/test_openssl_connection_ocsp_storectx_issuer_contract.pas`
- `tests/test_wolfssl_ocsp_stapling_contract.pas`
- `tests/scripts/test_isslocspstapling_residual_classification_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改 public signature
- 不改 backend runtime 行为
- 不重跑重型 compile / full test gates

## Why This Batch

`ISSLOCSPStapling` 的 ordinary docs 已经在上一批切到 owner path，但 residual grep 仍会命中 4 个 direct core test 文件：

- `tests/mbedtls/test_mbedtls_ocsp_capability.pas`
- `tests/openssl/test_ocsp_connection_verification_regression.pas`
- `tests/test_openssl_connection_ocsp_storectx_issuer_contract.pas`
- `tests/test_wolfssl_ocsp_stapling_contract.pas`

它们都更像 backend-specific runtime / contract proof，而不是 ordinary guidance 漂移。当前最小正确动作不是再迁移一次 owner path，而是把这 4 个 residual files 正式冻结，避免后续反复考古同一组 `GetOCSP*` 命中。

## Planned Changes

1. 在 `src/fafafa.ssl.base.pas` 的 OCSP core getter 注释中补 preferred-access / owner / compatibility note。
2. 在 `src/fafafa.ssl.connection.base.pas` 的基类注释里补出当前 OCSP residual surface 只剩 backend-specific proof。
3. 在 4 个 residual files 中补 `INTENTIONAL_OCSP_CORE_SURFACE` 标注。
4. 新增 focused source contract，守住：
   - canonical/source truth 继续把 `ISSLOCSPStapling` 作为 owner surface
   - direct core `GetOCSP*` 只出现在上述 4 个 residual files

## Verification

```bash
bash -n tests/scripts/test_isslocspstapling_residual_classification_contract.sh
bash tests/scripts/test_isslocspstapling_residual_classification_contract.sh
git diff --check
```

## Expected Outcome

- `ISSLOCSPStapling` 的 residual direct-core surface 被 freeze 成稳定 allowlist
- 后续不再重复拉起 OCSP ordinary-guidance / residual archaeology
- 默认下一步可以回到更大的 backend implementation-completeness 审查
