# `ISSLCertificateVerification` WinSSL Runtime Residual Freeze

## Goal

把 `tests/winssl/` 里当前仅剩的 3 个 direct core `GetVerifyResult` / `GetVerifyResultString` 在线命中，正式冻结成带意图说明的 WinSSL-specific runtime proof，而不是继续让它们看起来像“普通 guidance 漂移”。

## Scope

本批只处理 WinSSL residual proof 的注释、focused source contract 与台账：

- `tests/winssl/test_winssl_error_mapping_online.pas`
- `tests/winssl/test_winssl_hostname_mismatch_online.pas`
- `tests/winssl/test_winssl_revocation_online.pas`
- `tests/scripts/test_isslcertificateverification_winssl_runtime_residual_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不改 backend 实现
- 不改握手或验证行为
- 不重开 generic examples / active-guidance owner path
- 不重跑重型 Windows runtime gate

## Why This Batch

广义 residual allowlist 已经证明：

- `tests/winssl/` 当前 direct core `GetVerifyResult*` 命中只剩这 3 个文件
- 它们都是在线证书错误映射 / hostname mismatch / revocation proof
- ordinary docs、generic examples、generic tests 早已切到 `ISSLCertificateVerification` owner path

当前真正缺的不是“再改一次 owner path”，而是把这 3 个 WinSSL residual 文件明确标注成 intentional runtime proof，避免后续继续把它们当成 accidental drift 反复拉起。

## Planned Changes

1. 在 3 个 WinSSL online runtime tests 的 direct core `GetVerifyResult*` 读取点前补 `INTENTIONAL_VERIFY_RESULT_CORE_SURFACE` 注释。
2. 新增 focused source contract，锁住：
   - `tests/winssl/` 的 direct core verify-result file set 仍然只等于这 3 个文件
   - 每个文件都带 intentional residual 注释
   - 每个文件都继续显式覆盖 `GetVerifyResult` 与 `GetVerifyResultString`
3. 把 closeout 同步回 `task_plan.md` / `findings.md` / `progress.md`。

## Verification

```bash
bash -n tests/scripts/test_isslcertificateverification_winssl_runtime_residual_contract.sh
bash tests/scripts/test_isslcertificateverification_winssl_runtime_residual_contract.sh
bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh
git diff --check
```

## Expected Outcome

- `WinSSL` verify-result residual trio 被正式冻结成 intentional online runtime proof
- 后续不再需要反复重扫这 3 个文件
- 下一刀可以直接转向 `MbedTLS` residual cluster
