# 2026-05-24 MbedTLS Framework Owner-Surface Warning Closeout

## Goal

关闭 `tests/test_mbedtls_framework.pas`
在 focused 编译时暴露出来的 5 条
compiler-deprecated warning：

- `ISSLContext.SetServerName`
- `ISSLContext.GetServerName`
- `ISSLConnection.GetVerifyResult`
- `ISSLConnection.GetVerifyResultString`

并把这份 framework 测试
从“intentional residual / compatibility surface”
收回到当前 active owner-path truth：

- `ISSLClientConnection`
- `ISSLCertificateVerification`

## Scope

- Add:
  - `docs/plans/2026-05-24-mbedtls-framework-owner-surface-warning-closeout.md`
  - `tests/scripts/test_mbedtls_framework_owner_surface_contract.sh`
- Update:
  - `tests/test_mbedtls_framework.pas`
  - `tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
  - `tests/scripts/test_backend_framework_context_level_sni_labels_contract.sh`
  - `tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh`
  - `tests/scripts/test_isslcertificateverification_mbedtls_residual_contract.sh`
  - `tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
  - `tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

本批不做：

- 不重开 FreePascal early-data / PSK / VerifyEx 已关闭路线
- 不改生产 backend 实现
- 不扩散到 `test_wolfssl_framework.pas`
  的 residual cleanup

## Architecture Truth

- `tests/test_mbedtls_framework.pas`
  不是 compatibility archaeology 文件，
  而是当前活跃的 backend framework coverage；
  它不应该继续依赖
  context-level SNI
  或 direct-core verify-result mirrors
- context-level SNI 的 compatibility truth
  仍由其它显式分类测试承担；
  这份 MbedTLS framework 测试
  应回到 connection-level SNI
- verify-result owner
  已经固定到
  `ISSLCertificateVerification`；
  direct core getters
  只该留在
  intentional residual / mirror-proof
  allowlists 中
- `TInterfacedObject` 风格的连接对象
  一旦进入 interface 引用计数路径，
  就不能再混用手动 `Free`；
  否则会触发 double-destroy / invalid pointer

## Steps

1. 修改 `tests/test_mbedtls_framework.pas`：
   - 迁移到 `ISSLClientConnection.SetServerName`
   - 迁移到 `ISSLCertificateVerification.GetVerifyResult*`
   - 删除不再真实的 residual/warning quarantine 注释
2. 处理 helper-loss contract 的对象生命周期：
   - 连接对象改交给 interface 持有
   - 不再混用 interface 引用和手动 `Free`
3. 收窄相关 allowlist / classification 脚本：
   - 去掉 `tests/test_mbedtls_framework.pas`
     在 residual verify-result 集合里的成员资格
   - 去掉它在 direct context-SNI active classification 里的成员资格
4. 新增 focused 编译契约：
   - 编译 `tests/test_mbedtls_framework.pas`
   - 若 build log 再出现上述 deprecated warning 则失败
5. 跑 focused contracts
   与最小 CI gate
   收口

## Verification

```bash
bash tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh
bash tests/scripts/test_backend_framework_context_level_sni_labels_contract.sh
bash tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh
bash tests/scripts/test_isslcertificateverification_mbedtls_residual_contract.sh
bash tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh
bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh
bash tests/scripts/test_mbedtls_framework_owner_surface_contract.sh
bash scripts/run_minimal_ci_gate.sh --fast-local
git diff --check
```

## Expected Result

- `tests/test_mbedtls_framework.pas`
  focused 编译不再发出这 5 条 deprecated warning
- 这份 framework 测试
  不再被 residual allowlist
  和 direct context-SNI classification
  当成 intentional old-surface 成员
- 新增 focused contract
  能直接拦住这条 warning drift

## Outcome

- PASS
- `tests/test_mbedtls_framework.pas`
  已迁移到
  `ISSLClientConnection` / `ISSLCertificateVerification`
  owner path。
- focused 编译 warning 已归零，
  且 runtime 重新稳定。
- 第一版改动曾在 helper-loss contract
  里引入 interface/manual-free 混用的
  `EInvalidPointer`；
  现已通过 interface 托管生命周期修正。
