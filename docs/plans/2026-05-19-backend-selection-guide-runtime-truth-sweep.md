# Backend Selection Guide Runtime Truth Sweep

## Goal

收掉 `docs/BACKEND_SELECTION_GUIDE.md` 里仍会误导 builder/selector 使用者的旧入口心智，重点对齐：

- `WithSecurityFirst` 不等于默认已满足 FIPS
- `RequirePKCS11Support` 依赖当前已发布的 runtime-aware capability
- “政府/金融系统”场景不能把 `FIPS + PKCS#11` 写成当前默认 shipped backends 自动就能满足

## Scope

这批只改一份 active guide 与对应静态合同：

- `docs/BACKEND_SELECTION_GUIDE.md`
- `tests/scripts/test_backend_selection_guide_runtime_truth_contract.sh`

不改生产代码，不重开 selector 算法。

## Why This Batch

虽然前几批已经收口了：

- OpenSSL `SupportsPKCS11` runtime-aware source truth
- OpenSSL 默认构建 `SupportsFIPSMode=False`
- migration/reference/platform docs 的相关口径

但 `BACKEND_SELECTION_GUIDE.md` 仍保留 3 个高风险入口：

- `RequirePKCS11Support` 只写“要求支持 PKCS#11”，没有写明可能因为当前已注册 backend capability 不满足而失败
- `WithSecurityFirst` 只写 TLS1.3/密码/评分，没有明确说明它不等于默认 FIPS 路线
- “政府/金融系统”场景仍直接给出 `RequirePKCS11Support` 示例，缺少“当前默认 shipped backends 不保证自动满足 FIPS + PKCS#11”的边界说明

## Verification

```bash
bash -n tests/scripts/test_backend_selection_guide_runtime_truth_contract.sh
bash tests/scripts/test_backend_selection_guide_runtime_truth_contract.sh
git diff --check
```

## Expected Outcome

- `BACKEND_SELECTION_GUIDE` 不再传播静态品牌能力心智
- `RequirePKCS11Support` / `WithSecurityFirst` / 政府金融场景都回到当前 runtime-aware truth
