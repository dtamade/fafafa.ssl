# `ISSLCertificateVerification` MbedTLS Residual Cluster Freeze

## Goal

把 `MbedTLS` 当前剩余的 direct core `GetVerifyResult` / `GetVerifyResultString` 文件集，正式冻结成 backend-specific runtime/contract proof cluster，而不是继续让这些 backend 诊断程序看起来像普通 guidance 漂移。

## Scope

本批只处理 `MbedTLS` residual cluster 的注释、focused source contract 与台账：

- `tests/mbedtls/benchmark_handshake_simple.pas`
- `tests/mbedtls/test_mbedtls_safe.pas`
- `tests/mbedtls/test_mbedtls_simple_connection.pas`
- `tests/mbedtls/test_mbedtls_lowlevel.pas`
- `tests/mbedtls/test_mbedtls_cert_chain.pas`
- `tests/mbedtls/test_mbedtls_cert_errors.pas`
- `tests/mbedtls/test_mbedtls_cert_verify_flags.pas`
- `tests/test_mbedtls_framework.pas`
- `tests/scripts/test_isslcertificateverification_mbedtls_residual_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不改 backend 实现
- 不改握手或验证语义
- 不把 backend-specific proof 重写成 generic owner path
- 不重跑重型 Pascal compile/runtime gates

## Why This Batch

当前 broad residual allowlist 已经说明：

- `tests/mbedtls/` 与 `tests/test_mbedtls_framework.pas` 是 `MbedTLS` 侧全部 direct core verify-result 命中
- 这些文件都是 backend-specific benchmark / runtime diagnostics / verify-flags / framework contracts
- ordinary docs、generic examples、generic tests 和 WinSSL residual trio 都已经各自收口或冻结

因此这批最小安全动作不是继续改 owner path，而是把 `MbedTLS` residual cluster 的“保留原因”明确写死，并加 focused contract 锁住文件集。

## Planned Changes

1. 在上述 8 个 `MbedTLS` residual 文件中补统一的 `INTENTIONAL_VERIFY_RESULT_CORE_SURFACE` 注释。
2. 新增 focused source contract，锁住：
   - `MbedTLS` verify-result residual file set 仍然只等于这 8 个文件
   - 每个文件都带 intentional residual 注释
   - 每个文件继续保留各自预期的 direct core verify-result coverage
3. 把 closeout 同步回 `task_plan.md` / `findings.md` / `progress.md`。

## Verification

```bash
bash -n tests/scripts/test_isslcertificateverification_mbedtls_residual_contract.sh
bash tests/scripts/test_isslcertificateverification_mbedtls_residual_contract.sh
bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh
git diff --check
```

## Expected Outcome

- `MbedTLS` verify-result residual cluster 被正式冻结成 backend-specific runtime/contract proof
- 后续不再把这组文件当作 generic guidance 漂移反复拉起
- 下一刀可以继续转向 root-test / OpenSSL / WolfSSL 剩余 residual subgroup
