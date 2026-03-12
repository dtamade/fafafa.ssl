# 2026-03-10 pure Pascal client certificate pinning path

## Goal
- 让 pure Pascal / FreePascal 客户端真正消费 `AddCertificatePin*` / `SetCertificatePinningEnabled(...)`。
- 收口“接口存在但 runtime 不校验 pin”的 completeness 缺口。

## Architecture
- 先写 scripted client-handshake RED：
  - certificate pin 命中时握手成功
  - certificate pin 错误时握手失败
  - public-key pin 命中时握手成功
- 然后最小实现：
  - 在 pure Pascal client post-handshake validation 中加入 pinning
  - 证书 pin：`SHA256(cert DER)`
  - 公钥 pin：`SHA256(SPKI DER)`
  - 语义对齐现有 `TPinValidator`：pinning enabled 且无 pin 时失败
- 最后回归当前 client focused suites。

## Files
- `src/fafafa.ssl.freepascal.context.material.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `tests/test_freepascal_client_certificate_pinning_path.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## RED
- `fpc -Fu./src tests/test_freepascal_client_certificate_pinning_path.pas -otmp/test_fp_client_pinning && ./tmp/test_fp_client_pinning`
- Expected:
  - pure Pascal 当前还没消费 pinning，因此错误 pin 不会失败

## GREEN
- 只接纯 Pascal runtime pin validation。
- 不把这波扩大成 pin-management UI 或 multi-pin policy 重构。

## Verification
- `fpc -Fu./src tests/test_freepascal_client_certificate_pinning_path.pas -otmp/test_fp_client_pinning && ./tmp/test_fp_client_pinning`
- `fpc -Fu./src tests/test_freepascal_client_chain_verification_path.pas -otmp/test_fp_client_chain && ./tmp/test_fp_client_chain`
- `fpc -Fu./src tests/test_freepascal_client_system_roots_runtime.pas -otmp/test_fp_system_roots_runtime`
- `python3 -u scripts/compile_all_modules.py`
