# 2026-05-19 Optional Backends PKCS12 Capability Truth

## Goal

继续沿着 capability / public-surface truth 主线推进，收口 `MbedTLS` / `WolfSSL` 的 `SupportsPKCS12` 假阳性，并同步全局 PKCS#12 口径：

- `MbedTLS` / `WolfSSL` 当前仍把 `SupportsPKCS12` 发布为 `True`
- 但现有 shipped context surface 只看到：
  - `LoadCertificate*`
  - `LoadPrivateKey*`
  的 PEM / DER / PKCS#8 路径
- 当前看不到任何 public `PKCS#12/PFX` create / parse / import surface
- 同时 active docs 还存在口径冲突：
  - `docs/guides/FAQ.md` 仍写“PKCS#12 支持计划中”
  - `docs/guides/PKCS12_USER_GUIDE.md` 则写“通过 OpenSSL 后端提供完整支持”

## Scope

- 只处理 optional backends 的 PKCS#12 capability truth：
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`
  - `docs/BACKEND_CAPABILITY_MATRIX.md`
  - `docs/guides/FAQ.md`
  - `docs/guides/PKCS12_USER_GUIDE.md`
  - `docs/reference/API_REFERENCE.md`
- 用 focused shell contract + Pascal runtime contract 锁住 capability truth
- 不补做 `MbedTLS` / `WolfSSL` 的 PKCS#12 runtime 实现
- 不重开 OpenSSL PKCS#12 helper API 设计
- 不重做 WinSSL PFX/P12 import 实现；只在全局文档里保持其 partial-publication truth

## Files

- `src/fafafa.ssl.mbedtls.lib.pas`
- `src/fafafa.ssl.wolfssl.lib.pas`
- `docs/BACKEND_CAPABILITY_MATRIX.md`
- `docs/guides/FAQ.md`
- `docs/guides/PKCS12_USER_GUIDE.md`
- `docs/reference/API_REFERENCE.md`
- `tests/scripts/test_optional_backends_pkcs12_capability_truth_contract.sh`
- `tests/test_optional_backends_pkcs12_capability_truth_contract.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `OpenSSL`
  - `SupportsPKCS12=True`
  - 当前 published surface = `PKCS12_create` / `PKCS12_parse` / `d2i/i2d_PKCS12_bio` 等 helper API
- `WinSSL`
  - `SupportsPKCS12=True`
  - 当前 published surface = `PFX/P12` private-key/certificate bundle import
- `FreePascal`
  - `SupportsPKCS12=False`
  - 当前没有 shipped PKCS#12 bundle surface
- `MbedTLS` / `WolfSSL`
  - `SupportsPKCS12=False`
  - 当前 shipped context path 仅覆盖 PEM / DER / PKCS#8 certificate/private-key loading
  - 没有 public PKCS#12 create / parse / import surface

## Steps

1. 补 focused contract，让 `MbedTLS` / `WolfSSL` 的 capability 假阳性与全局文档冲突先 RED。
2. 把 optional backends 的 `SupportsPKCS12` 收回到真实范围。
3. 同步全局 PKCS#12 文档口径：
   - `OpenSSL` = 完整 helper/API
   - `WinSSL` = partial PFX/P12 import
   - `FreePascal` / `MbedTLS` / `WolfSSL` = 当前不发布
4. 跑 focused contracts，回写台账并提交。

## Commands

```bash
bash -n tests/scripts/test_optional_backends_pkcs12_capability_truth_contract.sh
bash tests/scripts/test_optional_backends_pkcs12_capability_truth_contract.sh
mkdir -p tmp/test_optional_backends_pkcs12_capability_truth && \
  fpc -B -Fu./src -Fu./tests \
    -FUtmp/test_optional_backends_pkcs12_capability_truth \
    -FEtmp/test_optional_backends_pkcs12_capability_truth \
    -otmp/test_optional_backends_pkcs12_capability_truth/test_optional_backends_pkcs12_capability_truth_contract \
    tests/test_optional_backends_pkcs12_capability_truth_contract.pas && \
  ./tmp/test_optional_backends_pkcs12_capability_truth/test_optional_backends_pkcs12_capability_truth_contract
git diff --check
```

## Expected Result

- `MbedTLS` / `WolfSSL` 不再把 `SupportsPKCS12` 发布成已实现 capability
- 全局文档不再对 PKCS#12 给出互相冲突的说法
- 调用方对当前 PKCS#12 backend truth 的心智重新统一为：
  - `OpenSSL` 全量
  - `WinSSL` PFX/P12 partial path
  - 其它 backend 当前不发布
