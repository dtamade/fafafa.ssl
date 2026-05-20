# API Documentation CertVerify Owner-Path Closeout（2026-05-21）

## Goal

收掉 `docs/reference/API_DOCUMENTATION.md` 里证书验证排错示例仍在直接教学
`Connection.GetVerifyResult` / `Connection.GetVerifyResultString`
的活跃漂移，并修复 focused contract 仍把这条 direct-core 写法当成正确答案的问题。

## Why now

- `INTEGRATION_GUIDE`、`OCSP_USAGE_GUIDE`、`CT_IMPLEMENTATION_GUIDE`
  已经转向
  `ISSLCertificateVerification`
  owner path
- `API_DOCUMENTATION.md`
  的 CT 示例里也已经有正确的
  `Supports(..., ISSLCertificateVerification, CertVerify)`
  写法
- 但错误处理 / 故障排查片段还在直调
  `Connection.GetVerifyResult*`
- 与此同时
  `tests/scripts/test_active_connection_api_docs_truth_contract.sh`
  还在给这套旧写法站岗

## Scope

- `docs/reference/API_DOCUMENTATION.md`
- `tests/scripts/test_active_connection_api_docs_truth_contract.sh`
- `docs/plans/2026-05-21-api-documentation-certverify-owner-path-closeout.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals

- 不修改 runtime source
- 不扩到 examples / backend tests / 更广泛的 certificate-verification 残余面
- 不重开 `ISSLConnection` mirror removal 设计

## Architecture Truth

- `GetVerifyResult` / `GetVerifyResultString`
  当前的默认 owner
  是
  `ISSLCertificateVerification`
- `ISSLConnection`
  上仍保留 compatibility mirror，
  但活跃文档的新代码示例应优先教学 owner path
- `API_DOCUMENTATION`
  既然已经是活跃 reference 文档，
  就不该在同一页里同时教：
  - CT 示例走 `CertVerify`
  - troubleshooting 又退回 direct core mirror

## Steps

1. 先把 `test_active_connection_api_docs_truth_contract.sh` 改成 owner-path truth。
2. 跑 contract 拿到预期 RED。
3. 最小修改 `API_DOCUMENTATION.md` 的错误处理 / 故障排查片段：
   - 通过 `Supports(Connection, ISSLCertificateVerification, CertVerify)` 获取验证信息
   - 不再直调 `Connection.GetVerifyResult*`
4. 跑 focused contract 与相关 certificate-verification guidance contract。
5. 更新台账后提交。

## Commands

```bash
bash -n tests/scripts/test_active_connection_api_docs_truth_contract.sh
bash tests/scripts/test_active_connection_api_docs_truth_contract.sh
bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh
git diff --check
git status --short
```

## Expected Result

- `API_DOCUMENTATION.md` 的证书验证排错示例与同页 CT 示例重新说成同一张图
- `test_active_connection_api_docs_truth_contract.sh` 不再给 direct-core mirror 站错岗
- 后续再出现这类 direct-core 文档回退时，focused contract 会直接报警
