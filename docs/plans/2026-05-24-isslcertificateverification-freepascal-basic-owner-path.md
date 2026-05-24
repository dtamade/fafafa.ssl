# `ISSLCertificateVerification` FreePascal Basic Owner Path

## Goal

把 `tests/test_freepascal_backend_basic.pas` 中 TLS 1.2 fail-closed 文本断言的
`GetVerifyResultString` 读取，从 deprecated `ISSLConnection` core mirror
迁到 `ISSLCertificateVerification` owner path。

## Architecture Rationale

这份根层测试不是 core mirror 等价 contract；它只需要确认
FreePascal backend 在 TLS 1.2 client/server 失败路径上返回
`unsupported` 文本。因此继续通过 direct core mirror 读取验证结果，
会把普通 runtime 断言误放进 residual allowlist。

本批保持：

- 不改 public API
- 不改 backend runtime 行为
- 不削弱 fail-closed 断言
- 只把普通测试迁到 owner interface

## Files

- `tests/test_freepascal_backend_basic.pas`
- `tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh`
- `tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
- `tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 确认当前 residual contracts 仍然绿色，证明起点与 allowlist 一致。
2. 在 `tests/test_freepascal_backend_basic.pas` 中通过
   `Supports(..., ISSLCertificateVerification, LCertVerify)` 读取
   `GetVerifyResultString`。
3. 移除该文件的 intentional direct-core residual 注释与 deprecated-warning
   quarantine。
4. 从 root residual、broad residual、compiler-deprecated quarantine contracts
   中移除该文件。
5. 运行 focused contracts、编译并运行目标 Pascal 测试。

## Verification

```bash
bash tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh
bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh
bash tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh
bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh
/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc -B -Fu./src -Fu./tests -FUtmp/test_freepascal_backend_basic_owner/units -FEtmp/test_freepascal_backend_basic_owner/bin tests/test_freepascal_backend_basic.pas
tmp/test_freepascal_backend_basic_owner/bin/test_freepascal_backend_basic
git diff --check
```

## Expected Result

- `tests/test_freepascal_backend_basic.pas` 不再属于 direct-core
  verify-result residual allowlist。
- root-test residual set 从 10 个文件缩到 9 个文件。
- `ISSLConnection.GetVerifyResultString` 的 compiler-deprecated quarantine
  面进一步缩小。

