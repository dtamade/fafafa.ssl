# 2026-03-10 pure Pascal password protected private key runtime

## Goal
- 让 pure Pascal / FreePascal backend 不再只是“truthfully unsupported”，而是在真实运行路径上支持一条最小可用的 password-protected private key 解密链。
- 让已有 `LoadPrivateKey(..., APassword)` / `LoadPrivateKeyPEM(..., APassword)` / `SetPasswordCallback(...)` 在 pure Pascal server signer 路径上真正生效。

## Scope
- `src/fafafa.ssl.freepascal.keydecrypt.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `tests/test_freepascal_password_callback_runtime_path.pas`
- `tests/test_freepascal_password_protected_private_key_truth.pas`
- `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Root Cause
- 之前 pure Pascal 只保存 `FPasswordCallback`，但加载路径完全不消费它。
- server signer 只吃 clear PKCS#8 / PKCS#1 / SEC1 材料；因此真正的修复点必须在 context 装载时，把加密私钥先规范化成 clear private-key material。
- 当前仓库里没有现成的 pure Pascal encrypted-key helper，所以这波补了一条最小纯 Pascal 解密链：
  - `PBES2`
  - `PBKDF2-HMAC-SHA256`
  - `AES-CBC`

## Verification
- `fpc -Fu./src -Fu./examples tests/test_freepascal_password_callback_runtime_path.pas -otmp/test_fp_password_callback_runtime && ./tmp/test_fp_password_callback_runtime`
- `fpc -Fu./src tests/test_freepascal_password_protected_private_key_truth.pas -otmp/test_fp_password_key_truth && ./tmp/test_fp_password_key_truth`
- `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept`
- `python3 -u scripts/compile_all_modules.py`
