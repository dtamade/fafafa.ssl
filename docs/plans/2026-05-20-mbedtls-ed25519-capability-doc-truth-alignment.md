# MbedTLS Ed25519 Capability Doc Truth Alignment

## Goal

收口 `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
里
`Ed25519`
这一行把上游 MbedTLS 3.x 理论能力
误写成 fafafa.ssl 当前已发布能力的问题，
让专页重新回到当前 source / public-surface truth：

- 当前 backend 没有 published `Ed25519`-specific capability / metadata surface
- 当前 `TMbedTLSCertificate.GetPublicKeyAlgorithm`
  仍返回：
  - `RSA`
- 当前 `TMbedTLSCertificate.GetSignatureAlgorithm`
  仍返回：
  - `SHA256withRSA`

## Scope

- 新增 focused shell contract，锁住当前 source / dedicated-doc truth
- 最小修正 `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
- 更新 `task_plan.md` / `findings.md` / `progress.md`

不做：

- 不补 MbedTLS `Ed25519` 实现
- 不扩到 `x25519` / async / session / Windows 等其他话题
- 不重开 broader certificate metadata redesign

## Architecture Truth

- `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
  当前仍写：
  - `| Ed25519 | ⚠️ 部分 | MbedTLS 3.x |`
- `src/fafafa.ssl.mbedtls.lib.pas`
  当前 capability record 只发布：
  - `sslKexRSA`
  - `sslKexDHE_RSA`
  - `sslKexECDHE_RSA`
  - `sslKexECDHE_ECDSA`
- `src/fafafa.ssl.mbedtls.certificate.pas`
  当前对外证书算法 metadata 仍返回默认值：
  - `GetPublicKeyAlgorithm -> 'RSA'`
  - `GetSignatureAlgorithm -> 'SHA256withRSA'`

这说明：

- 不应把上游 MbedTLS 3.x 的理论 `Ed25519` 能力
  直接写成 fafafa.ssl 当前 backend 的 published truth
- 当前更准确的专页表述应是：
  - `Ed25519 | ❌ 当前 capability 不发布 | ...`

## Files

- `src/fafafa.ssl.mbedtls.lib.pas`
- `src/fafafa.ssl.mbedtls.certificate.pas`
- `docs/reference/MBEDTLS_BACKEND_CAPABILITY_MATRIX.md`
- `tests/scripts/test_mbedtls_ed25519_capability_doc_truth_contract.sh`
- `docs/plans/2026-05-20-mbedtls-ed25519-capability-doc-truth-alignment.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 新增 focused shell contract
2. 先跑 contract，确认当前 dedicated doc 先 RED
3. 最小修正 MbedTLS dedicated matrix：
   - `Ed25519`
4. 重新跑 focused verification
5. 更新 planning files，准备 commit / push

## Verification

```bash
bash -n tests/scripts/test_mbedtls_ed25519_capability_doc_truth_contract.sh
bash tests/scripts/test_mbedtls_ed25519_capability_doc_truth_contract.sh
git diff --check
```

## Expected Outcome

- MbedTLS 专页不再把
  `Ed25519`
  讲成“部分支持”
- 专页会明确：
  - 当前 backend 没有 published `Ed25519`-specific capability / metadata surface
  - 当前 `GetPublicKeyAlgorithm / GetSignatureAlgorithm`
    仍停在 RSA 默认值
  - 不要把上游 MbedTLS 3.x 理论能力
    当成 fafafa.ssl 当前 backend truth
