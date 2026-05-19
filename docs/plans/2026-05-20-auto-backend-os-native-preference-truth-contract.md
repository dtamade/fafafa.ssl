# Auto-Backend OS-Native Preference Truth Contract

## Goal

给 `PreferOSNative` / auto-backend selection 补一条 runtime-aware focused contract，证明 `BackendImplType = sslImplOSNative` 不只是停留在 capability record 上，而是真的进入了 selector 的 score / 排序，并且 builder 下游会沿用同一个 selection truth。

## Architecture

这批只补 focused proof，不改生产实现：

- 新增一条 `tests/test_auto_backend_os_native_preference_truth_contract.pas`
- 由于当前 Linux 环境没有真实可用的 OS-native backend，这批合同使用
  受控 mock runtime：
  - mock `sslOpenSSL` backend：`sslImplCLibrary`
  - mock `sslWinSSL` backend：`sslImplOSNative`
- contract 使用两组 requirements：
  - baseline：`CreateDefaultRequirements(optBalanced)`，并把最低分数门槛清零
  - preferred：baseline + `PlatformPreferences.PreferOSNative := True`
- 同时验证：
  - `SelectBestBackends(...)` 在两组 requirements 下对每个 backend 的 score 变化
  - baseline top candidate 与 preferred top candidate 的切换
  - `SelectBestBackend(...)` 是否返回 preferred 排序后的第一名
  - `TSSLContextBuilder.Create.WithAutoBackendSelection(...).TryBuildClient(...)`
    是否沿用 selector 选中的 backend
- 不修改 selector 算法
- 不修改 builder 行为
- 不重开真实 WinSSL runtime proof

## Files

- Add: `docs/plans/2026-05-20-auto-backend-os-native-preference-truth-contract.md`
- Add: `tests/test_auto_backend_os_native_preference_truth_contract.pas`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

当前 selector / builder focused downstream proof 已补齐：

- `RequirePKCS11Support`
- `RequireTPM`
- `RequireSystemCertStore`
- `PreferHardwareAccel`

但 `PreferOSNative` 还缺一条直接 proof。若这条 proof 缺位，后续仍可能出现：

- capability source 继续发布 `BackendImplType = sslImplOSNative`
- 但 selector score / 排序 / builder 下游没有真实消费这条 preference truth

与 `PreferHardwareAccel` 不同，这条 preference 在当前 Linux 环境缺少真实正例：

- `WinSSL` 不可用
- 真实可用 backend 里没有 live `sslImplOSNative`

因此更稳的做法不是做半截 negative-only proof，
而是使用受控 mock runtime 把 selection truth 明确钉住。

## Verification

```bash
mkdir -p tmp/test_auto_backend_os_native_truth_units && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_auto_backend_os_native_truth_units \
  -FEtmp/test_auto_backend_os_native_truth_units \
  -otmp/test_auto_backend_os_native_truth_units/test_auto_backend_os_native_preference_truth_contract \
  tests/test_auto_backend_os_native_preference_truth_contract.pas && \
./tmp/test_auto_backend_os_native_truth_units/test_auto_backend_os_native_preference_truth_contract

git diff --check
```

## Expected Outcome

- 在受控 mock runtime 下：
  - baseline 时由 `sslImplCLibrary` backend 领先
  - 开启 `PreferOSNative` 后，`sslImplOSNative` backend
    因当前公式获得固定平台偏好加分并反超
- `SelectBestBackend(...)` 必须返回 preferred 排序后的第一名
- builder 必须成功，并沿用 selector 选中的 backend
- `BackendImplType = sslImplOSNative` 的 published truth 与
  selector / builder downstream truth 再次闭环
