# OpenSSL PKCS#11 Capability Runtime Truth

## Goal

继续沿着 capability/public-surface 审查主线推进，把 `OpenSSL SupportsPKCS11` 从当前的 unconditional truth 收紧到现成 runtime truth source，避免它继续把“仓库里有 shipped loader path”误写成“当前运行时一定具备 PKCS#11 backend readiness”。

## Architecture

这批保持很窄：

- 只处理 `OpenSSL SupportsPKCS11`
- 复用现成 runtime truth source：
  - `TPKCS11BackendFactory.IsBackendAvailable(btAuto)`
- 不新增 PKCS#11 实现
- 不扩 builder / selector API
- 不重开 WinSSL / TPM 路线

## Files

- Modify: `src/fafafa.ssl.openssl.backed.pas`
- Modify: `tests/openssl/test_openssl_features.pas`
- Modify: `docs/BACKEND_CAPABILITY_MATRIX.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

当前仓库已经形成了这样的边界：

- `TOpenSSLContext.LoadPrivateKeyFromPKCS11(...)` 是真实 shipped loader path
- 但 `SupportsPKCS11` 仍是硬编码 `True`
- `TPKCS11BackendFactory.IsBackendAvailable(btAuto)` 已经提供了更细的 runtime readiness truth
  - Provider path:
    - `OSSL_PROVIDER_load`
    - `OSSL_STORE_open`
    - `OSSL_STORE_expect`
  - ENGINE path:
    - `ENGINE_by_id`
    - `ENGINE_init`
    - `ENGINE_load_private_key`

因此下一刀不该砍掉 OpenSSL 的 PKCS#11 能力，而应让 capability 跟随当前 auto backend readiness。

## Verification

```bash
mkdir -p tmp/test_openssl_features_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_features_units -FEtmp/test_openssl_features_units -otmp/test_openssl_features_units/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/test_openssl_features_units/test_openssl_features
python3 scripts/compile_all_modules.py
git diff --check
```

## Expected Outcome

- `OpenSSL SupportsPKCS11` 不再是 unconditional `True`
- capability truth 改为跟随 `TPKCS11BackendFactory.IsBackendAvailable(btAuto)`
- active backend capability doc 不再把 OpenSSL PKCS#11 写成完全脱离 runtime readiness 的绝对值
