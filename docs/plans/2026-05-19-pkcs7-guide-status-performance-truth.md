# PKCS7 Guide Status And Performance Truth

## Goal

把 `docs/guides/PKCS7_USER_GUIDE.md` 里的固定状态、固定性能数字、固定通过率快照从当前指南正文 truth 中移除，并把页面重新锚回当前 OpenSSL PKCS#7 surface 与可执行验证入口。

## Architecture

这批保持 docs-only：

- 新增 focused shell contract，冻结 `PKCS7_USER_GUIDE` 的 status/performance truth 边界
- 只修改 `docs/guides/PKCS7_USER_GUIDE.md`
- 不改生产实现
- 不扩大到其它 quickstart / architecture / performance 页面

## Files

- Add: `docs/plans/2026-05-19-pkcs7-guide-status-performance-truth.md`
- Add: `tests/scripts/test_pkcs7_guide_status_performance_truth_contract.sh`
- Modify: `docs/guides/PKCS7_USER_GUIDE.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

`PKCS7_USER_GUIDE.md` 当前还把下列历史快照直接写成页面 truth：

- `Production Ready (100% 测试通过)`
- 固定 `2 ms` 签名/加密/解密性能
- 固定 `500 ops/s` 吞吐量
- 固定 `158/158` 测试通过率

这类 specialized guide drift 比一般“预期输出”更危险，因为它会让调用方把某次历史跑数误解成当前长期状态结论。

同时，当前真实口径已经在 `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md` 明确：

- `PKCS7` 当前没有一对一 capability 字段
- 支持判断依赖 `LoadPKCS7Functions`
- 模块加载状态使用 `osmPKCS7`
- 还要结合当前 focused tests

## Verification

```bash
bash -n tests/scripts/test_pkcs7_guide_status_performance_truth_contract.sh
bash tests/scripts/test_pkcs7_guide_status_performance_truth_contract.sh
npx prettier --write docs/guides/PKCS7_USER_GUIDE.md
git diff --check
```

## Expected Outcome

- `PKCS7_USER_GUIDE.md` 保留：
  - 当前 OpenSSL PKCS#7 raw API + helper surface
  - 当前验证入口文件和命令
  - 当前成功标准
  - 重要 BIO ownership 规则
- 但不再把：
  - `Production Ready`
  - `100%`
  - `158/158`
  - 固定 `2 ms`
  - 固定 `500 ops/s`
  写成当前正文 truth
