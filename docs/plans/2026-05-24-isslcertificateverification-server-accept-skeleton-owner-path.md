# `ISSLCertificateVerification` FreePascal Server Accept Skeleton Owner Path

## Goal

把 `tests/test_freepascal_server_accept_skeleton.pas` 中 server accept 失败
文本断言的 `GetVerifyResultString` 读取，从 deprecated `ISSLConnection`
core mirror 迁到 `ISSLCertificateVerification` owner path。

## Architecture Rationale

这份根层测试的重点仍然是 TLS 1.3 server accept skeleton：

- 连接信息与 ALPN/版本/cipher 断言应继续通过 `ISSLConnectionInfo`
  读取
- `GetVerifyResultString` 只是在 accept 失败后提供诊断文本
- 该 failure text 不是 core mirror 等价 proof

因此它和前面几个普通 runtime proof 一样，适合迁到
`ISSLCertificateVerification` owner path，而不是继续冻结成 direct-core
residual。

本批保持：

- 不改 public API
- 不改 server accept 行为
- 不改 `GetConnectionInfo` / `GetSelectedALPNProtocol` 的 owner path
- 只把 verify-result 文本读取迁到 owner interface

## Files

- `tests/test_freepascal_server_accept_skeleton.pas`
- `tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh`
- `tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
- `tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 运行 starting contracts，确认当前 5-file residual allowlist 仍与源码一致。
2. 在目标测试中新增本地 helper：
   `GetCertificateVerifyResultString(const AConn: ISSLConnection): string`。
3. helper 通过 `Supports(AConn, ISSLCertificateVerification, LCertVerify)`
   取得 owner interface，再读取 `LCertVerify.GetVerifyResultString`。
4. 替换 accept 失败断言里的 `LConn.GetVerifyResultString`。
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
mkdir -p tmp/test_freepascal_server_accept_owner/units tmp/test_freepascal_server_accept_owner/bin
/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_server_accept_owner/units -FEtmp/test_freepascal_server_accept_owner/bin tests/test_freepascal_server_accept_skeleton.pas
tmp/test_freepascal_server_accept_owner/bin/test_freepascal_server_accept_skeleton
git diff --check
```

## Expected Result

- `tests/test_freepascal_server_accept_skeleton.pas` 不再属于 direct-core
  verify-result residual allowlist。
- root-test residual set 从 5 个文件缩到 4 个文件。
- `ISSLConnection.GetVerifyResultString` 的 compiler-deprecated quarantine
  面进一步缩小。
