# Auto-Backend Hardware-Accel Preference Truth Contract

## Goal

给 `PreferHardwareAccel` / auto-backend selection 补一条 runtime-aware focused contract，证明 `HasHardwareAcceleration` 不只是停留在 capability record 上，而是真的进入了 selector 的 score，并且 builder 下游会沿用同一个 selection truth。

## Architecture

这批只补 focused proof，不改生产实现：

- 新增一条 `tests/test_auto_backend_hardware_accel_preference_truth_contract.pas`
- contract 使用两组 requirements：
  - baseline：`CreateDefaultRequirements(optBalanced)`，并把最低分数门槛清零
  - preferred：baseline + `PlatformPreferences.PreferHardwareAccel := True`
- 同时验证：
  - `SelectBestBackends(...)` 在两组 requirements 下对每个 backend 的 score 变化
  - `SelectBestBackend(...)` 是否返回 preferred 排序后的第一名
  - `TSSLContextBuilder.Create.WithAutoBackendSelection(...).TryBuildClient(...)`
    是否沿用 selector 选中的 backend
- 不修改 selector 算法
- 不修改 builder 行为
- 不重开 backend capability source truth

## Files

- Add: `docs/plans/2026-05-20-auto-backend-hardware-accel-preference-truth-contract.md`
- Add: `tests/test_auto_backend_hardware_accel_preference_truth_contract.pas`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

当前 selector / builder 主线已经有几条 focused truth：

- `RequirePKCS11Support`
- `RequireTPM`
- `RequireSystemCertStore`
- `ISSLLibrary.IsFeatureSupported(...)` consumer parity

但 `PreferHardwareAccel` 还缺一条直接 downstream proof。若这条 proof 缺位，后续仍可能出现：

- capability source 里继续发布 `HasHardwareAcceleration`
- 但 selector score / 排序 / builder 下游没有真实消费这条 truth

相比 `PreferOSNative`，这条 preference 在当前 Linux 环境就能同时观察到：

- `HasHardwareAcceleration=True` 的 backend
- `HasHardwareAcceleration=False` 的 backend

因此更适合作为下一条本地可验证的 focused contract。

## Verification

```bash
mkdir -p tmp/test_auto_backend_hardware_accel_truth_units && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_auto_backend_hardware_accel_truth_units \
  -FEtmp/test_auto_backend_hardware_accel_truth_units \
  -otmp/test_auto_backend_hardware_accel_truth_units/test_auto_backend_hardware_accel_preference_truth_contract \
  tests/test_auto_backend_hardware_accel_preference_truth_contract.pas && \
./tmp/test_auto_backend_hardware_accel_truth_units/test_auto_backend_hardware_accel_preference_truth_contract

git diff --check
```

## Expected Outcome

- 在 baseline 与 preferred 两组 requirements 下：
  - qualifying backend 集合应保持一致
  - `HasHardwareAcceleration=True` 的 backend score
    应按当前公式获得平台偏好加分
  - `HasHardwareAcceleration=False` 的 backend score
    应保持不变
- `SelectBestBackend(...)` 必须返回 preferred 排序后的第一名
- builder 必须成功，并沿用 selector 选中的 backend
- `HasHardwareAcceleration` 的 published truth 与
  selector / builder downstream truth 再次闭环
