# 2026-05-19 Security Best Practices Pinning Helper Truth

## Goal

继续沿着 specialized guide completeness 主线推进，收口 `docs/guides/security-best-practices.md` 里 certificate pinning 示例仍在教授不存在 helper 的问题：

- `LoadCertificateFromFile(...)`

当前这条示例实际在走 `TPinValidator` + `PX509` 的 OpenSSL raw certificate handle 路径，因此应回到当前真实入口：

- `LoadCertificateFromPEM(...)`
- `X509_free(...)`

## Scope

- 只修 `security-best-practices` 中 pinning 示例的 helper/raw API truth
- 用 focused shell contract 锁住：
  - 不再使用不存在的 `LoadCertificateFromFile(...)`
  - 明确这是 OpenSSL raw certificate handle 路径
  - 示例使用 `LoadCertificateFromPEM(...)`
  - 示例释放 `PX509` 句柄
- 不修改 runtime 实现
- 不扩到其它 security guide / pinning API 文档

## Files

- `docs/guides/security-best-practices.md`
- `tests/scripts/test_security_best_practices_pinning_helper_truth_contract.sh`
- `docs/plans/2026-05-19-security-best-practices-pinning-helper-truth.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `src/fafafa.ssl.cert.pinning.pas` 中 `TPinValidator.ExtractPublicKeyHash(...)` 当前直接接收：
  - `PX509`
- `src/fafafa.ssl.openssl.api.pem.pas` 当前公开的文件加载 helper 是：
  - `LoadCertificateFromPEM(...)`
- 当前源码里没有：
  - `LoadCertificateFromFile(...)`
- 因为示例直接持有 `PX509`，所以示例也应显式：
  - `X509_free(...)`

## Steps

1. 新增 focused contract，让旧 helper 名先 RED。
2. 修正 `security-best-practices` 中 pinning 示例，回到当前 OpenSSL raw helper truth。
3. 同步台账，跑轻量验证并提交。

## Commands

```bash
bash -n tests/scripts/test_security_best_practices_pinning_helper_truth_contract.sh
bash tests/scripts/test_security_best_practices_pinning_helper_truth_contract.sh
git diff --check
```

## Expected Result

- `security-best-practices` 不再继续教授不存在的证书加载 helper
- pinning 示例重新锚回 `LoadCertificateFromPEM(...)` + `X509_free(...)`
- specialized guide completeness 再向前收一层

## Result

- 已完成。
- `docs/guides/security-best-practices.md` 中的 certificate pinning 示例现在已回到当前 OpenSSL raw helper truth：
  - `LoadCertificateFromPEM(...)`
  - `X509_free(...)`
- 文档现在明确说明这条示例是：
  - OpenSSL raw certificate handle 路径
  - 不是 backend-neutral helper
- 活跃文档不再继续教授不存在的：
  - `LoadCertificateFromFile(...)`

## Verification

```bash
bash -n tests/scripts/test_security_best_practices_pinning_helper_truth_contract.sh
bash tests/scripts/test_security_best_practices_pinning_helper_truth_contract.sh
git diff --check
```

- 结果：全部通过
