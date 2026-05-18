# `ISSLCertificateVerification` Active Guidance De-emphasis

## Goal

把普通文档与通用 integration/contract 测试里仍把证书验证结果当作 `ISSLConnection` 核心入口的调用点切到 `ISSLCertificateVerification` owner path，让这组能力面先从“普通 guidance 仍直连 core”收缩到“仅 backend-specific runtime / contract 残留”。

## Scope

本批只处理 active guidance、focused contract 与台账：

- `docs/INTEGRATION_GUIDE.md`
- `docs/reference/API_DOCUMENTATION.md`
- `tests/integration/test_cross_backend_consistency_contract.pas`
- `tests/integration/test_cross_backend_errors_contract.pas`
- `tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改生产实现
- 不修改 backend-specific certificate-verification runtime tests
- 不修改 `tests/contract/test_backend_contract.pas`
- 不重跑重型 repo gate

## Why This Batch

当前 `ISSLCertificateVerification` 的 cross-backend owner truth 已经存在：

- `tests/contract/test_backend_contract.pas` 已锁住：
  - `Supports(LConn, ISSLCertificateVerification, ...)`
  - `GetVerifyResult`
  - `GetVerifyResultString`
  - `GetPeerCertificateChain`

但普通 guidance 里仍有 direct core 残余：

- `docs/INTEGRATION_GUIDE.md` 的阻塞/非阻塞握手失败示例仍直接使用 `Conn.GetVerifyResultString`
- `docs/INTEGRATION_GUIDE.md` 的排错条目仍写 `Conn.GetVerifyResult / Conn.GetVerifyResultString`
- `docs/reference/API_DOCUMENTATION.md` 的 CT 示例仍在失败路径直接使用 `Conn.GetVerifyResultString`
- `tests/integration/test_cross_backend_consistency_contract.pas` 与 `tests/integration/test_cross_backend_errors_contract.pas` 仍直接读取 verify result mirrors

这些文件更像“公开推荐路径”，比 backend-specific runtime tests 更适合优先收掉。

## Planned Changes

1. 把 `INTEGRATION_GUIDE` 的握手失败路径与排错条目切到 `ISSLCertificateVerification`。
2. 把 `API_DOCUMENTATION` 的 CT 示例失败路径切到 `ISSLCertificateVerification`。
3. 在两份通用 integration/contract 测试里新增 helper，改走 `ISSLCertificateVerification` owner path。
4. 新增 focused contract，防止普通 docs/tests 重新把 verify-result guidance 教回 direct core。

## Verification

```bash
bash -n tests/scripts/test_isslcertificateverification_active_guidance_contract.sh
bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh
mkdir -p tmp/test_cross_backend_consistency_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_consistency_contract -FEtmp/test_cross_backend_consistency_contract -otmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract tests/integration/test_cross_backend_consistency_contract.pas && ./tmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract
mkdir -p tmp/test_cross_backend_errors_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_errors_contract -FEtmp/test_cross_backend_errors_contract -otmp/test_cross_backend_errors_contract/test_cross_backend_errors_contract tests/integration/test_cross_backend_errors_contract.pas && ./tmp/test_cross_backend_errors_contract/test_cross_backend_errors_contract
git diff --check
```

## Expected Outcome

- ordinary docs stop teaching direct core verify-result getters
- generic cross-backend integration/contract tests stop treating `GetVerifyResult / GetVerifyResultString` as default core path
- remaining direct core certificate-verification residuals stay intentionally confined to backend-specific runtime / contract proof

## Result

- `docs/INTEGRATION_GUIDE.md` 的握手失败示例与排错条目现在统一改成：
  - `Supports(Conn, ISSLCertificateVerification, CertVerify)`
  - `CertVerify.GetVerifyResult`
  - `CertVerify.GetVerifyResultString`
- `docs/reference/API_DOCUMENTATION.md` 的 CT 示例失败路径现在先走：
  - `Supports(Conn, ISSLCertificateVerification, CertVerify)`
  - `raise Exception.Create(CertVerify.GetVerifyResultString)`
- `tests/integration/test_cross_backend_consistency_contract.pas`
  现在通过：
  - `GetVerificationResult(AConn)`
  - `ISSLCertificateVerification.GetVerifyResult`
  读取 verify code
- `tests/integration/test_cross_backend_errors_contract.pas`
  现在通过：
  - `GetVerificationResult(AConn)`
  - `GetVerificationResultString(AConn)`
  读取 verify mirrors
- 新增 focused contract：
  - `tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`

## Route Impact

- `ISSLCertificateVerification` 的 cross-backend owner truth 早已存在；这批之后 ordinary docs/tests 也不再继续把 verify-result core getters 当推荐主路径
- 默认下一步不该再重复做 certificate-verification active-guidance 清扫
- 若继续沿同类 optional-owner surface 推进，应切到下一组 ordinary guidance 仍偏 core 的接口，或回到更大的 interface-design completeness 选择
