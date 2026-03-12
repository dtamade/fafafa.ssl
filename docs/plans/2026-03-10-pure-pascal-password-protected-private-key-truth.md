# 2026-03-10 pure Pascal password-protected private key truth

## Goal
- 收口 pure Pascal / FreePascal backend 在“密码保护私钥”上的能力漂移。
- 不再让 capability 写成支持、而 runtime 静默忽略 password / callback。

## Architecture
- 先写 focused RED：
  - `SupportsPasswordProtectedKeys` 当前必须反映真实能力
  - `LoadPrivateKeyPEM(...)` 对加密 PEM 私钥必须显式报 `unsupported`
- 然后最小实现：
  - pure Pascal capability 改为 `SupportsPasswordProtectedKeys = False`
  - context 在 `LoadPrivateKey(...)` / `LoadPrivateKeyPEM(...)` 路径对加密 PEM 私钥显式拒绝
- 这波不假装完成 encrypted private key 解密，也不把它包装成“password callback 已实现”。

## Files
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `tests/test_freepascal_password_protected_private_key_truth.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## RED
- `fpc -Fu./src tests/test_freepascal_password_protected_private_key_truth.pas -otmp/test_fp_password_key_truth && ./tmp/test_fp_password_key_truth`
- Expected:
  - capability currently still claims password-protected keys are supported
  - encrypted PEM private key load is still silently accepted

## GREEN
- 显式 truth-sync，不做未完成的 encrypted-key 实现。

## Verification
- `fpc -Fu./src tests/test_freepascal_password_protected_private_key_truth.pas -otmp/test_fp_password_key_truth && ./tmp/test_fp_password_key_truth`
- `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`
- `python3 -u scripts/compile_all_modules.py`
