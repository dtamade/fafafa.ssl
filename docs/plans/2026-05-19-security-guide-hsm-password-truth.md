# Security Guide HSM And Password-Key Truth

## Goal

把 `docs/guides/SECURITY_GUIDE.md` 里的 HSM 与密码保护私钥示例重新锚回当前 public API / backend capability truth，避免它继续示范不存在的 PKCS#11/HSM 接口。

## Architecture

这批保持 docs-only：

- 新增 focused shell contract，冻结 `SECURITY_GUIDE` 的 HSM / password-key truth
- 只修改 `docs/guides/SECURITY_GUIDE.md`
- 不改生产实现
- 不扩到其它安全主题

## Files

- Add: `docs/plans/2026-05-19-security-guide-hsm-password-truth.md`
- Add: `tests/scripts/test_security_guide_hsm_password_truth_contract.sh`
- Modify: `docs/guides/SECURITY_GUIDE.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

当前 `SECURITY_GUIDE.md` 的密钥管理段落仍在输出一组直接误导调用代码的旧示例：

- 使用不存在的：
  - `LoadPKCS11Engine(...)`
  - `LoadKeyFromHSM(...)`
  - `LContext.SetPrivateKey(...)`
- 把 `LContext.LoadPrivateKey('server.key', 'strong-password')` 直接写成 generic truth，
  却没有交代：
  - 需要先检查 `SupportsPasswordProtectedKeys`
  - `WinSSL` 当前只有 PFX/P12 password path
  - `FreePascal` / `WolfSSL` 当前 non-empty `APassword` 会 fail-closed

这类文档错误会把调用者直接带去写不存在或跨 backend 不成立的代码，比“统计数字过期”更危险。

## Verification

```bash
bash -n tests/scripts/test_security_guide_hsm_password_truth_contract.sh
bash tests/scripts/test_security_guide_hsm_password_truth_contract.sh
bash tests/scripts/test_api_inventory_pkcs11_high_entry_truth_contract.sh
npx prettier --write docs/guides/SECURITY_GUIDE.md
git diff --check
```

## Expected Outcome

- `SECURITY_GUIDE.md` 不再引用不存在的 HSM/PKCS#11 helper API
- HSM 示例回到当前 published truth：
  - `OpenSSL` backend
  - PKCS#11 URI
  - runtime-aware `SupportsPKCS11`
- 密码保护私钥示例回到当前 capability truth：
  - 先检查 `SupportsPasswordProtectedKeys`
  - 不把某条 backend-specific path 冒充成 generic truth
