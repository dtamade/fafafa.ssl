# `ISSLCertificateVerification` OCSP Runtime Duo Freeze

## Goal

把当前剩余的 `OpenSSL` / `WolfSSL` server-side OCSP stapling runtime direct core verify-result 读取面，正式冻结成 backend-specific diagnostic duo，而不是继续让它们看起来像 generic guidance 漂移。

## Scope

本批只处理两份 OCSP stapling runtime proof 的注释、focused source contract 与台账：

- `tests/openssl/test_openssl_server_ocsp_stapling_runtime.pas`
- `tests/wolfssl/test_wolfssl_server_ocsp_stapling_runtime.pas`
- `tests/scripts/test_isslcertificateverification_ocsp_runtime_duo_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不改 OpenSSL / WolfSSL 实现
- 不改 stapling 行为或握手语义
- 不把服务端 runtime diagnostics 重写成 generic owner path
- 不重跑重型 Pascal compile/runtime gates

## Why This Batch

当前 broad residual allowlist 继续显示：

- `tests/openssl/` 只剩 `test_openssl_server_ocsp_stapling_runtime.pas`
- `tests/wolfssl/` 只剩 `test_wolfssl_server_ocsp_stapling_runtime.pas`
- 这两个文件都属于 backend-specific server-side OCSP stapling runtime diagnostics

因此这批最小安全动作不是改行为，而是把这对残余文件的“保留原因”明确写死，并用 focused contract 守住文件集与 diagnostics 覆盖。

## Planned Changes

1. 在这两个 OCSP stapling runtime 文件中补统一的 `INTENTIONAL_VERIFY_RESULT_CORE_SURFACE` 注释。
2. 新增 focused source contract，锁住：
   - current direct-core OCSP runtime residual duo 仍然只等于这两个文件
   - 两个文件都带 intentional residual 注释
   - `OpenSSL` 文件继续保留 `GetVerifyResultString`
   - `WolfSSL` 文件继续保留 `GetVerifyResult` 与 `GetVerifyResultString`
3. 把 closeout 同步回 `task_plan.md` / `findings.md` / `progress.md`。

## Verification

```bash
bash -n tests/scripts/test_isslcertificateverification_ocsp_runtime_duo_contract.sh
bash tests/scripts/test_isslcertificateverification_ocsp_runtime_duo_contract.sh
bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh
git diff --check
```

## Expected Outcome

- `OpenSSL/WolfSSL` server-side OCSP stapling runtime verify-result duo 被正式冻结成 backend-specific diagnostics
- 后续不再把这两个文件当作 generic guidance 漂移反复拉起
- 下一刀可以继续转向 root-test residual subgroup
