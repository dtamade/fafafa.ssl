# Auto-Backend System Cert Store Capability Truth Contract

## Goal

给 `RequireSystemCertStore` / auto-backend selection 补一条 runtime-aware focused contract，证明 selector / builder 的下游结果确实跟随当前已发布的 `SupportsSystemCertStore` capability truth，而不是继续停留在“总是成功”或“总是失败”的旧环境假设。

## Architecture

这批只补 focused proof，不改生产实现：

- 新增一条 `tests/test_auto_backend_system_cert_store_capability_truth_contract.pas`
- contract 通过当前已注册 backend 的 capability truth 推导期望结果
- 同时验证：
  - `SelectBestBackend(...)`
  - `TSSLContextBuilder.Create.WithAutoBackendSelection(...).TryBuildClient(...)`
- focused contract 会把最低分数门槛清零，只验证
  `RequireSystemCertStore` 这条 requirement truth，
  避免被 `optBalanced` 默认评分阈值误伤
- 不修改 selector 算法
- 不修改 builder 行为
- 不重开 system-roots loader / store runtime 实现

## Files

- Add: `docs/plans/2026-05-20-auto-backend-system-cert-store-capability-truth-contract.md`
- Add: `tests/test_auto_backend_system_cert_store_capability_truth_contract.pas`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

当前已经有几条相关 truth：

- `SupportsSystemCertStore` 是 selector 会直接消费的 published capability
- `FreePascal` 当前会按运行时目录检测发布该 capability
- `WinSSL` 当前固定发布该 capability
- `RequirePKCS11Support` / `RequireTPM` 已有 focused downstream contract

但 `RequireSystemCertStore` 还缺一条直接 proof。若这条 proof 缺位，后续仍可能出现：

- capability source 已经对
- 但 selector / builder 下游行为继续停留在旧环境假设

## Verification

```bash
mkdir -p tmp/test_auto_backend_system_cert_store_truth_units && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_auto_backend_system_cert_store_truth_units \
  -FEtmp/test_auto_backend_system_cert_store_truth_units \
  -otmp/test_auto_backend_system_cert_store_truth_units/test_auto_backend_system_cert_store_capability_truth_contract \
  tests/test_auto_backend_system_cert_store_capability_truth_contract.pas && \
./tmp/test_auto_backend_system_cert_store_truth_units/test_auto_backend_system_cert_store_capability_truth_contract

git diff --check
```

## Expected Outcome

- 若当前有已注册 backend 发布 `SupportsSystemCertStore=True`，则：
  - `SelectBestBackend(...)` 必须成功
  - 选中的 backend 也必须发布 `SupportsSystemCertStore=True`
  - auto-backend builder 必须成功
- 若当前没有任何已注册 backend 发布 `SupportsSystemCertStore=True`，则：
  - selector 必须失败
  - builder 也必须失败
- selector / builder 的下游结果与当前 capability truth 再次闭环
