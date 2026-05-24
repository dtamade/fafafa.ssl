# `ISSLCertificateVerification` FreePascal Online OCSP Owner Path

## Goal

把 `tests/test_freepascal_client_online_ocsp_runtime.pas` 中在线 OCSP
fail-closed 文本断言的 `GetVerifyResultString` 读取，从 deprecated
`ISSLConnection` core mirror 迁到 `ISSLCertificateVerification` owner path。

## Architecture Rationale

这份根层测试覆盖 FreePascal TLS 1.3 在线 OCSP runtime parity：

- good status 必须允许握手
- revoked status 必须 fail-closed
- OCSP 响应签名验证失败必须 fail-closed
- responder / delegated responder 验证失败必须暴露清晰错误文本

当前 direct core 命中都只是失败后的诊断文本读取，不是
`ISSLConnection.GetVerifyResultString` 与
`ISSLCertificateVerification.GetVerifyResultString` 的 mirror 等价 proof。
因此本轮预分类为 `owner-migrate`。

本批保持：

- 不改 public API
- 不改 OCSP / TLS 1.3 runtime 行为
- 不削弱 fail-closed 断言
- 只把普通 runtime proof 的读取路径迁到 owner interface

## Files

- `tests/test_freepascal_client_online_ocsp_runtime.pas`
- `tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh`
- `tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
- `tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 运行 starting contracts，确认当前 6-file residual allowlist 仍与源码一致。
2. 在目标测试中新增本地 helper：
   `GetCertificateVerifyResultString(const AConn: ISSLConnection): string`。
3. helper 通过 `Supports(AConn, ISSLCertificateVerification, LCertVerify)`
   取得 owner interface，再读取 `LCertVerify.GetVerifyResultString`。
4. 替换 revoked、signature、responder/delegated 三组失败文本断言里的
   `LConn.GetVerifyResultString`。
5. 移除目标文件的 `INTENTIONAL_VERIFY_RESULT_CORE_SURFACE` 注释和
   deprecated-warning quarantine。
6. 从 root residual、broad residual、compiler-deprecated quarantine contracts
   中移除目标文件。
7. 运行 focused contracts、目标 Pascal 编译/运行和 hygiene check。

## Verification

```bash
bash tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh
bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh
bash tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh
bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh
mkdir -p tmp/test_freepascal_online_ocsp_owner/units tmp/test_freepascal_online_ocsp_owner/bin
/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_online_ocsp_owner/units -FEtmp/test_freepascal_online_ocsp_owner/bin tests/test_freepascal_client_online_ocsp_runtime.pas
tmp/test_freepascal_online_ocsp_owner/bin/test_freepascal_client_online_ocsp_runtime
git diff --check
```

## Expected Result

- `tests/test_freepascal_client_online_ocsp_runtime.pas` 不再属于 direct-core
  verify-result residual allowlist。
- root-test residual set 从 6 个文件缩到 5 个文件。
- `ISSLConnection.GetVerifyResultString` 的 compiler-deprecated quarantine
  面进一步缩小。

## Execution Result

- `tests/test_freepascal_client_online_ocsp_runtime.pas` 已迁到
  `ISSLCertificateVerification.GetVerifyResultString`。
- 该文件已从 root residual、broad residual、compiler-deprecated quarantine
  allowlist 中移除。
- Focused verification passed:
  - `bash tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh`
  - `bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
  - `bash tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh`
  - `bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
- Pascal compile/run passed:
  - `/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_online_ocsp_owner/units -FEtmp/test_freepascal_online_ocsp_owner/bin tests/test_freepascal_client_online_ocsp_runtime.pas`
  - `tmp/test_freepascal_online_ocsp_owner/bin/test_freepascal_client_online_ocsp_runtime`
- Result: root-test direct-core verify-result residual set is now 5 files.
