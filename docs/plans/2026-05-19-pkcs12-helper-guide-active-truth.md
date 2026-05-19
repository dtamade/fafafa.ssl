# 2026-05-19 PKCS12 Helper Guide Active Truth

## Goal

继续沿着接口/文档完整性主线推进，收口 `docs/guides/PKCS12_USER_GUIDE.md` 仍在教授不存在旧 API 的问题，并把高入口 PKCS#12 helper 路径补回当前 public truth：

- `LoadCertificateFromFile(...)`
- `LoadPrivateKeyFromFile(...)`

这两个名字当前并不在源码中存在；当前真正可用的入口是：

- 高入口 helper：
  - `TPKCS12Manager`
  - `TPKCS12Options`
  - `DefaultPKCS12Options`
- OpenSSL raw helper：
  - `LoadCertificateFromPEM(...)`
  - `LoadPrivateKeyFromPEM(...)`

## Scope

- 只修 PKCS#12 活跃指南/API 参考的当前 helper truth
- 用 focused shell contract 锁住：
  - `PKCS12_USER_GUIDE` 不再教授不存在的 `LoadCertificateFromFile` / `LoadPrivateKeyFromFile`
  - `PKCS12_USER_GUIDE` 明确区分：
    - `TPKCS12Manager` 高入口 helper
    - OpenSSL raw PKCS#12 / PEM helper
  - `API_REFERENCE` 补出当前 façade re-export 的 `TPKCS12Manager` / `DefaultPKCS12Options`
- 不修改 runtime 实现
- 不扩到其它证书/安全指南页面

## Files

- `docs/guides/PKCS12_USER_GUIDE.md`
- `docs/reference/API_REFERENCE.md`
- `tests/scripts/test_pkcs12_helper_guide_active_truth_contract.sh`
- `docs/plans/2026-05-19-pkcs12-helper-guide-active-truth.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `src/fafafa.ssl.pas` 当前 façade 已公开导出：
  - `TPKCS12Options`
  - `TPKCS12Manager`
  - `DefaultPKCS12Options`
- `src/fafafa.ssl.cert.advanced.pas` 当前高入口 helper surface 是：
  - `TPKCS12Manager.CreatePKCS12(...)`
  - `TPKCS12Manager.CreatePKCS12ToFile(...)`
  - `TPKCS12Manager.LoadFromPKCS12(...)`
  - `TPKCS12Manager.LoadFromPKCS12File(...)`
- `src/fafafa.ssl.openssl.api.pem.pas` 当前 raw PEM helper 是：
  - `LoadCertificateFromPEM(...)`
  - `LoadPrivateKeyFromPEM(...)`
- 当前源码里没有：
  - `LoadCertificateFromFile(...)`
  - `LoadPrivateKeyFromFile(...)`

## Steps

1. 新增 focused contract，让旧 PKCS#12 指南用法先 RED。
2. 修正 `PKCS12_USER_GUIDE` 示例和文字说明，回到当前 helper/raw API truth。
3. 在 `API_REFERENCE` 里补出 `TPKCS12Manager` / `DefaultPKCS12Options` 的高入口参考说明。
4. 同步台账，跑轻量验证并提交。

## Commands

```bash
bash -n tests/scripts/test_pkcs12_helper_guide_active_truth_contract.sh
bash tests/scripts/test_pkcs12_helper_guide_active_truth_contract.sh
git diff --check
```

## Expected Result

- `PKCS12_USER_GUIDE` 不再继续教授源码中不存在的文件加载 helper
- PKCS#12 高入口 helper 与 OpenSSL raw API 的边界重新说清楚
- `API_REFERENCE` 能直接给出当前 façade 上的 PKCS#12 helper 入口

## Result

- 已完成。
- `docs/guides/PKCS12_USER_GUIDE.md` 现在已经明确区分：
  - 高入口 helper：
    - `TPKCS12Manager`
    - `DefaultPKCS12Options`
  - OpenSSL raw API：
    - `fafafa.ssl.openssl.api.pkcs12`
    - `fafafa.ssl.openssl.api.pem`
- PKCS#12 指南不再继续使用源码中不存在的：
  - `LoadCertificateFromFile(...)`
  - `LoadPrivateKeyFromFile(...)`
- `docs/reference/API_REFERENCE.md` 现在已补出 façade 上当前公开的：
  - `DefaultPKCS12Options`
  - `TPKCS12Manager.CreatePKCS12(...)`
  - `TPKCS12Manager.CreatePKCS12ToFile(...)`
  - `TPKCS12Manager.LoadFromPKCS12(...)`
  - `TPKCS12Manager.LoadFromPKCS12File(...)`

## Verification

```bash
bash -n tests/scripts/test_pkcs12_helper_guide_active_truth_contract.sh
bash tests/scripts/test_pkcs12_helper_guide_active_truth_contract.sh
git diff --check
```

- 结果：全部通过
