# OpenSSL CT Capability Truth Re-Tightening Plan

**Goal:** 收紧 `OpenSSL` 后端对外发布的 Certificate Transparency capability truth，让 `IsFeatureSupported(sslFeatCertificateTransparency)`、`SupportsCertificateTransparency`、`CertTransparencySupport` 与当前真实 public connection surface 重新保持一致。

**Architecture:** 这批不新做 OpenSSL CT connection surface，不扩到 `TOpenSSLConnection` 的新 optional interface，也不改 FreePascal 现有 CT runtime。只处理一条已经被静态审查钉实的回漂：
- `tests/openssl/test_openssl_features.pas`：先补 focused RED，证明当前默认 OpenSSL runtime 下依然错误发布 CT capability / feature。
- `src/fafafa.ssl.openssl.backed.pas`：把 CT 从当前默认 public capability 发布中收紧回 `False/None`，并让 `sslFeatCertificateTransparency` 与之对齐。
- `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`：把“底层 CT binding 可用”与“默认 backend public capability 已发布”分开写清，避免继续直接映射。

**Files:**
- Modify: `tests/openssl/test_openssl_features.pas`
- Modify: `src/fafafa.ssl.openssl.backed.pas`
- Modify: `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - prove current OpenSSL CT capability drift

Run:

```bash
mkdir -p tmp/test_openssl_features_units && \
fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_features_units -FEtmp/test_openssl_features_units \
  -otmp/test_openssl_features_units/test_openssl_features tests/openssl/test_openssl_features.pas && \
./tmp/test_openssl_features_units/test_openssl_features
```

Add checks:
- default `OpenSSL` capability must keep `SupportsCertificateTransparency=False`
- default `OpenSSL` capability must keep `CertTransparencySupport=sslSupportNone`
- `IsFeatureSupported(sslFeatCertificateTransparency)` must stay `False`
- created connection must not expose `ISSLCertificateTransparency` / `ISSLCertificateTransparencyValidation`

Expected RED before production fix:
- current code still reports CT capability as present when OpenSSL CT module is loaded

## Task 2: GREEN - tighten capability truth instead of expanding implementation

Change:
- keep low-level CT binding presence as internal readiness only
- stop publishing CT as default OpenSSL public capability / required-feature truth
- preserve existing docs that already say current OpenSSL backend does not expose CT connection surface

Constraints:
- do not add new `TOpenSSLConnection` CT interface implementation
- do not widen scope to selector redesign or serializer format changes
- do not reopen FreePascal CT runtime work

## Task 3: Verification

Run:

```bash
mkdir -p tmp/test_openssl_features_units && \
fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_features_units -FEtmp/test_openssl_features_units \
  -otmp/test_openssl_features_units/test_openssl_features tests/openssl/test_openssl_features.pas && \
./tmp/test_openssl_features_units/test_openssl_features
python3 scripts/compile_all_modules.py
git diff --check
```

## Definition Of Done

- `OpenSSL` default capability no longer claims CT public support without a real connection surface
- `sslFeatCertificateTransparency` no longer misleads backend selector / callers on OpenSSL
- focused OpenSSL contract and full compile stay green
- docs / planning files record the new truth so this line不再反复拉起

## Execution Result

- focused RED 先证明了真实漂移边界不是“默认初始化马上出错”，而是：
  - 一旦 `osmCT` 被标记为已加载
  - `OpenSSL` backend 就会错误把低层 CT binding readiness 抬成 public capability / feature truth
- 最小 GREEN 没有扩写 `TOpenSSLConnection`：
  - `sslFeatCertificateTransparency` 固定回 `False`
  - `SupportsCertificateTransparency` 固定回 `False`
  - `CertTransparencySupport` 固定回 `sslSupportNone`
  - `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md` 改为明确区分“底层 CT API 可用”与“默认 backend public capability 已发布”
- 验证结果：
  - `tests/openssl/test_openssl_features.pas`：PASS
  - `tests/contract/test_backend_contract.pas`：`135 total / 111 passed / 0 failed / 24 skipped`
  - `python3 scripts/compile_all_modules.py`：`187/187 成功`
  - `git diff --check`：PASS
