# `GetSelectedALPNProtocol` Residual Classification Freeze

## Goal

把 `GetSelectedALPNProtocol` 当前剩余的 direct core usage 冻成清晰 allowlist：活跃文档与普通测试继续走 `ISSLConnectionInfo.GetSelectedALPNProtocol`，而 core getter 只允许保留在 connection-info contract mirror proof 与 backend-specific runtime files 中。

## Scope

本批只处理 source comments、focused source contract 与台账：

- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.connection.base.pas`
- `tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改 public signature
- 不改 backend runtime tests
- 不重跑 Pascal compile gates

## Why This Batch

`GetSelectedALPNProtocol` 的 active docs 与 ordinary tests 已经不再教 direct core getter。当前剩余命中已经收缩到：

- `tests/contract/test_backend_contract.pas` 的 mirror proof
- `tests/mbedtls/test_mbedtls_alpn.pas`
- `tests/winssl/test_winssl_alpn_sni.pas`
- `tests/winssl/test_winssl_connection_edge_cases.pas`

这已经足够小，适合直接冻结成 allowlist，避免下一批继续重复扫同一批 residual hits。

## Planned Changes

1. 在 `src/fafafa.ssl.base.pas` 的 `GetSelectedALPNProtocol` 注释中补 preferred-access / owner 说明。
2. 在 `src/fafafa.ssl.connection.base.pas` 的基类注释里补出 `GetSelectedALPNProtocol` 当前剩余 residual surface。
3. 新增 focused source contract，守住：
   - active docs 与 ordinary tests 继续走 `ISSLConnectionInfo.GetSelectedALPNProtocol`
   - direct core `GetSelectedALPNProtocol` 只出现在 contract mirror proof 与上述 backend-specific runtime files

## Verification

```bash
bash -n tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh
bash tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh
git diff --check
```

## Expected Outcome

- `GetSelectedALPNProtocol` 的 residual direct-core surface 被 freeze 成稳定 allowlist
- 下一批可以决定是讨论更强 client-owner/deprecation wording，还是切到下一条 mirror
