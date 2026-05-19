# Hardware-Key Contract Runtime Truth Resync

## Goal

修复 `tests/scripts/test_hardware_key_capability_truth_contract.sh` 的滞后真相，使它不再把旧的
`Result.SupportsPKCS11 := True;` 当成 OpenSSL 当前 capability truth，而是改为守护已经发布的
runtime-aware truth。

## Architecture

这批只做测试/文档合同收口：

- 不改生产实现
- 保留：
  - OpenSSL shipped `LoadPrivateKeyFromPKCS11(...)` public path
  - WinSSL `PKCS11/TPM` 非发布 capability truth
- 新增守护：
  - OpenSSL `SupportsPKCS11` 必须跟随
    `TPKCS11BackendFactory.IsBackendAvailable(btAuto)`
  - 旧的 unconditional `Result.SupportsPKCS11 := True;` 不得回流
  - `docs/BACKEND_CAPABILITY_MATRIX.md` 中 OpenSSL PKCS#11 runtime-readiness 说明必须保留

## Files

- Modify: `tests/scripts/test_hardware_key_capability_truth_contract.sh`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

上一批已经把 OpenSSL `SupportsPKCS11` 从 unconditional `True` 收紧为 runtime truth，
但旧的 `hardware-key` shell contract 仍要求源码里出现 `Result.SupportsPKCS11 := True;`。

这意味着：

- 测试 truth 已落后于当前实现 truth
- 若不收口，后续 rerun 会持续红测
- 更糟的是，它会鼓励把源码误改回旧的静态 capability 口径

## Verification

```bash
bash -n tests/scripts/test_hardware_key_capability_truth_contract.sh
bash tests/scripts/test_hardware_key_capability_truth_contract.sh
git diff --check
```

## Expected Outcome

- `hardware-key` shell contract 不再要求旧的 `Result.SupportsPKCS11 := True;`
- 合同改为守护 OpenSSL 当前 runtime-aware PKCS#11 capability truth
- OpenSSL / WinSSL 的硬件密钥 capability doc/source truth 再次统一
