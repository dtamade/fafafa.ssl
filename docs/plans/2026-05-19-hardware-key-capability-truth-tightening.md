# Hardware-Key Capability Truth Tightening

## Goal

收紧当前 `PKCS11/TPM` capability truth，使其回到仓库里已经真正发布的 public/runtime surface，避免 backend selector 因假阳性 capability 把后端选错。

## Architecture

保持这批足够窄，只处理当前已经压实的 capability 假阳性：

- `OpenSSL`
  - 保留已 shipped 的 `PKCS#11` capability truth
  - 收紧没有 shipped TPM public/runtime path 的 `SupportsTPM`
- `WinSSL`
  - 收紧 `SupportsPKCS11`
  - 收紧 `SupportsTPM`
  - 同步修正活跃 capability 说明文档

这批不做：

- 新增 PKCS#11 / TPM runtime 实现
- 扩 builder API
- 重开更大的 selector 打分设计
- 扫描所有历史文档

## Files

- Modify: `src/fafafa.ssl.openssl.backed.pas`
- Modify: `src/fafafa.ssl.winssl.lib.pas`
- Modify: `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md`
- Modify: `tests/openssl/test_openssl_features.pas`
- Add: `tests/test_auto_backend_tpm_capability_truth_contract.pas`
- Add: `tests/scripts/test_hardware_key_capability_truth_contract.sh`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

静态审查已经确认：

- `src/fafafa.ssl.openssl.context.pas` 确实存在 shipped `LoadPrivateKeyFromPKCS11(...)` 路径
- 但 `src/fafafa.ssl.openssl.backed.pas` 仍把 `SupportsTPM` 直接发布为 `True`
- `src/fafafa.ssl.winssl.lib.pas` 仍把 `SupportsPKCS11` / `SupportsTPM` 直接发布为 `True`
- `src/fafafa.ssl.backend.selector.pas` 会直接消费这些 capability 字段做 required-match 与 platform-score 判定
- `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md` 还把“智能卡 / TPM”写成已支持

因此这不是“文档有点旧”，而是 selector / capability / active docs 一起失真。

## Steps

1. 加 focused contracts：
   - OpenSSL runtime contract：`SupportsTPM` 必须为 `False`
   - auto-selector downstream contract：要求 `TPM` 时必须报 `no suitable backend`
   - source/doc static contract：WinSSL 不再发布 `PKCS11/TPM` capability，WinSSL active doc 不再写成已支持
2. 先跑 RED，确认当前 capability 假阳性真实存在。
3. 最小修源码与 WinSSL 活跃文档。
4. 复跑 focused 验证，再跑 `python3 scripts/compile_all_modules.py` 收口。

## Verification

```bash
bash -n tests/scripts/test_hardware_key_capability_truth_contract.sh
bash tests/scripts/test_hardware_key_capability_truth_contract.sh
mkdir -p tmp/test_openssl_features_units && fpc -B -Fu./src -Fu./tests -FUtmp/test_openssl_features_units -FEtmp/test_openssl_features_units -otmp/test_openssl_features_units/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/test_openssl_features_units/test_openssl_features
mkdir -p tmp/test_auto_backend_tpm_truth_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_auto_backend_tpm_truth_units -FEtmp/test_auto_backend_tpm_truth_units -otmp/test_auto_backend_tpm_truth_units/test_auto_backend_tpm_truth_contract tests/test_auto_backend_tpm_capability_truth_contract.pas && ./tmp/test_auto_backend_tpm_truth_units/test_auto_backend_tpm_truth_contract
python3 scripts/compile_all_modules.py
git diff --check
```

## Expected Outcome

- `OpenSSL` 不再把“无已发布 TPM surface”的状态写成 capability 支持
- `WinSSL` 不再把“平台潜在能力”误写成 `PKCS11/TPM` public capability
- auto backend selection 在 `RequireTPM` 类需求下不再被假阳性 capability 带偏
- WinSSL 活跃 capability 文档回到 current shipped truth
