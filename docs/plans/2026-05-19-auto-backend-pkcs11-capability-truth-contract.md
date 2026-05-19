# Auto-Backend PKCS11 Capability Truth Contract

## Goal

给 `RequirePKCS11Support` / auto-backend selection 补一条 runtime-aware focused contract，证明 selector / builder 的下游结果确实跟随当前已发布的 PKCS#11 capability truth，而不是继续停留在“总是失败”或“总是成功”的旧环境假设。

## Architecture

这批只加 focused proof，不改生产实现：

- 新增一条 `tests/test_auto_backend_pkcs11_capability_truth_contract.pas`
- 当前 contract 通过实际已注册 backend 的 capability truth 推导期望结果
- 不修改 selector 算法
- 不修改 builder 行为
- 不重开 TPM / WinSSL / PKCS#11 loader 实现

## Files

- Add: `tests/test_auto_backend_pkcs11_capability_truth_contract.pas`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

上一轮已经收口：

- OpenSSL `SupportsPKCS11` 改为 runtime-aware truth
- `hardware-key` shell contract 也已经同步到新 truth

但 selector / builder 下游目前只有：

- `RequireTPM` 的 focused contract

还没有对应的：

- `RequirePKCS11Support` runtime-aware contract

如果这条 proof 缺位，后续仍可能出现：

- capability source 已经对
- 但 auto-backend selection 的行为被旧假设带偏

## Verification

```bash
mkdir -p tmp/test_auto_backend_pkcs11_truth_units && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_auto_backend_pkcs11_truth_units \
  -FEtmp/test_auto_backend_pkcs11_truth_units \
  -otmp/test_auto_backend_pkcs11_truth_units/test_auto_backend_pkcs11_capability_truth_contract \
  tests/test_auto_backend_pkcs11_capability_truth_contract.pas && \
./tmp/test_auto_backend_pkcs11_truth_units/test_auto_backend_pkcs11_capability_truth_contract

git diff --check
```

## Expected Outcome

- 若当前有已注册 backend 发布 `SupportsPKCS11=True`，则 auto-backend selection 必须成功
- 若当前没有任何已注册 backend 发布 `SupportsPKCS11=True`，则 auto-backend selection 必须失败
- selector / builder 的下游结果与当前 capability truth 再次闭环
