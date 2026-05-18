# `ISSLCertificateVerification` Residual Classification Freeze

## Goal

把 `GetVerifyResult` / `GetVerifyResultString` 当前剩余的 direct core usage 冻成清晰 allowlist：active docs、generic examples、generic example tests 继续优先走 `ISSLCertificateVerification` owner path，而 direct core getter 只允许保留在 helper fallback、contract mirror proof 与 backend-specific runtime / contract residuals 中。

## Scope

本批只处理 source comments、focused source contract 与台账：

- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.connection.base.pas`
- `tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改 public signature
- 不改 backend runtime / contract 测试语义
- 不重跑重型 Pascal compile gates

## Why This Batch

`ISSLCertificateVerification` 的 ordinary docs/tests/generic examples 已经都切到 owner path。当前剩余 direct core getter 已收缩到三类：

- shared/local helper fallback
- `tests/contract/test_backend_contract.pas` 的 optional/core mirror proof
- backend-specific runtime / contract residuals

这已经足够小，适合直接冻结成 allowlist，避免下一批继续重复扫同一批 residual hits。

## Planned Changes

1. 在 `src/fafafa.ssl.base.pas` 的 `GetVerifyResult` / `GetVerifyResultString` 注释里补 preferred-access / owner 说明。
2. 在 `src/fafafa.ssl.connection.base.pas` 的基类注释里补出当前 residual direct-core surface 的性质。
3. 新增 focused source contract，守住：
   - active docs 不再 reintroduce direct core certificate-verification guidance
   - `examples/` 只剩 shared helper fallback
   - `tests/examples/` 不再 reintroduce direct core getter
   - `tests/connection/` 只剩本地 helper fallback
   - `tests/contract/` / backend-specific runtime / contract residual file set 保持稳定 allowlist

## Verification

```bash
bash -n tests/scripts/test_isslcertificateverification_residual_classification_contract.sh
bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh
git diff --check
```

## Expected Outcome

- `ISSLCertificateVerification` 的 residual direct-core surface 被 freeze 成稳定 allowlist
- 下一批可以更安心地继续 backend-specific runtime / residual deprecation lane，而不用再反复扫 ordinary guidance
