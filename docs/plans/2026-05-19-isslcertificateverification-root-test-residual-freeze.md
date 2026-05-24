# `ISSLCertificateVerification` Root-Test Residual Freeze

## Goal

把当前 `tests/*.pas` 根层里剩余的 direct core `GetVerifyResult` / `GetVerifyResultString` 文件集，正式冻结成 runtime / backend-contract residual subgroup，而不是继续让这些 root-level contracts 看起来像 generic guidance 漂移。

## Scope

本批只处理 root-test residual subgroup 的注释、focused source contract 与台账：

- `tests/test_freepascal_backend_basic.pas`
- `tests/test_freepascal_client_cert_verify_flags_runtime.pas`
- `tests/test_freepascal_client_certificate_flight_requirements.pas`
- `tests/test_freepascal_client_chain_trust_runtime.pas`
- `tests/test_freepascal_client_ct_sct_surface.pas`
- `tests/test_freepascal_client_ocsp_stapling_runtime.pas`
- `tests/test_freepascal_client_online_ocsp_runtime.pas`
- `tests/test_freepascal_server_accept_skeleton.pas`
- `tests/test_mbedtls_framework.pas`
- `tests/test_openssl_connection_verify_result_contract.pas`
- `tests/test_wolfssl_framework.pas`
- `tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不改任何 backend 实现
- 不改测试行为或断言语义
- 不把 root-level runtime/contracts 重写成 generic owner path
- 不重跑重型 compile/runtime gates

## Why This Batch

当前 broad residual allowlist 已经被连续三批缩窄到：

- 已冻结的 WinSSL runtime trio
- 已冻结的 MbedTLS residual cluster
- 已冻结的 OpenSSL/WolfSSL OCSP runtime duo
- 当前还未单独冻结的，主要就是 `tests/*.pas` 根层 residual subgroup

这些文件虽然位于根层，但真实定位都属于：

- FreePascal runtime contracts
- OpenSSL / WolfSSL / MbedTLS backend framework or verify-result contracts

所以这批最小安全动作不是改 owner path，而是把 root-test subgroup 的“保留原因”写死，并用 focused contract 锁住文件集。

## Planned Changes

1. 在当前尚未明确标注的 root-test residual 文件里补统一的 `INTENTIONAL_VERIFY_RESULT_CORE_SURFACE` 注释。
2. 新增 focused source contract，锁住：
   - current `tests/*.pas` direct-core verify-result residual file set
   - 每个文件都带 intentional residual 注释
   - 各文件继续保留各自预期的 verify-result coverage
3. 把 closeout 同步回 `task_plan.md` / `findings.md` / `progress.md`。

## Verification

```bash
bash -n tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh
bash tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh
bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh
git diff --check
```

## Expected Outcome

- root-test verify-result residual subgroup 被正式冻结成 runtime / backend-contract proof
- 后续不再把这组根层文件当作 generic guidance 漂移反复拉起
- `ISSLCertificateVerification` 当前剩余 residual 面会进一步逼近“全部已分类冻结”

## Follow-up Narrowing

- `tests/test_mbedtls_framework.pas` 已在后续批次迁到
  `ISSLCertificateVerification` owner path，并移出 root residual allowlist。
- `tests/test_freepascal_backend_basic.pas` 的 TLS 1.2 fail-closed 文本断言
  已在 `2026-05-24-isslcertificateverification-freepascal-basic-owner-path.md`
  迁到 `ISSLCertificateVerification.GetVerifyResultString`。
- 当前 root-test direct-core verify-result residual subgroup 已从原冻结名单继续缩到
  9 个文件；以
  `tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh`
  为最新 file-set truth。
