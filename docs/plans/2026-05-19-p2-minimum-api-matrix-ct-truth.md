# P2 Minimum API Matrix CT Truth

## Goal

收紧 `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md` 里关于 CT
（Certificate Transparency）的字段映射口径，消除“顶部结论说 CT 有直接字段表达，
但 CT 行和特别说明又说没有默认直接字段映射”的自相矛盾。

## Architecture

这批保持 docs-only：

- 新增 focused shell contract，冻结 P2 最低 API 矩阵里的 CT 映射 truth
- 只修改 `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
- 不改生产实现
- 不扩大到其它 capability 文档

## Files

- Add: `docs/plans/2026-05-19-p2-minimum-api-matrix-ct-truth.md`
- Add: `tests/scripts/test_p2_minimum_api_matrix_ct_truth_contract.sh`
- Modify: `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

当前 `P2_MINIMUM_API_CAPABILITY_MATRIX.md` 存在一个高风险自相矛盾：

- 顶部结论写：
  - `TSSLBackendCapabilities` 已能直接表达 **PKCS12 / CT**
- 但 CT 行和特别说明又写：
  - `无默认直接字段映射`
  - `SupportsCertificateTransparency` / `CertTransparencySupport`
    不应被当成这组底层 API 的直接映射

这会直接误导调用方对 OpenSSL backend CT public capability 的判断，属于
capability/source truth 漂移，而不是普通文案问题。

## Verification

```bash
bash -n tests/scripts/test_p2_minimum_api_matrix_ct_truth_contract.sh
bash tests/scripts/test_p2_minimum_api_matrix_ct_truth_contract.sh
npx prettier --write docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md
git diff --check
```

## Expected Outcome

- 顶部结论不再宣称 CT 有直接字段表达
- CT 行继续明确：
  - 这里只代表底层 OpenSSL CT binding / validator 可用性
  - 不等于默认 backend 已发布 connection-level CT public capability
- 读者不会再把 `SupportsCertificateTransparency` /
  `CertTransparencySupport` 误当成这张 P2 底层 API 表的直接映射
