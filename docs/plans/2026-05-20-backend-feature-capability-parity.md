# Backend Feature Capability Parity（2026-05-20）

## Goal
- 给 `ISSLLibrary.IsFeatureSupported(...)` 与 `ISSLLibrary.GetCapabilities`
  补一条端到端 parity proof，确保当前 `TSSLFeature` 枚举所暴露的 feature
  不会在 runtime feature probe 和 capability record 发布之间再次分叉。
- 这批要锁住的 truth：
  - 对 `sslFeatSNI` / `sslFeatALPN` / `sslFeatSessionCache` /
    `sslFeatSessionTickets` / `sslFeatRenegotiation` /
    `sslFeatOCSPStapling` / `sslFeatCertificateTransparency`
  - 当前 published truth 必须以 capability record 中对应的
    `*Support <> sslSupportNone` 为准
  - `IsFeatureSupported(...)` 必须与这套 published truth 保持一致，
    不允许 selector / caller 看到两套 runtime 结论

## Why now
- capability dual-truth 的 producer / serializer / diff / selector 路线已经分别收紧。
- 但目前还缺一条直接的 runtime consumer proof：
  - `IsFeatureSupported(...)`
  - `GetCapabilities`
  是否真的在所有 live backend 上同口径。
- 如果这层 proof 缺失，调用方仍可能在 library runtime query 和 capability record
  之间读到不同结论，后续又会把这条线反复拉起。

## Scope
- `tests/test_backend_feature_capability_parity_contract.pas`
- `docs/plans/2026-05-20-backend-feature-capability-parity.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals
- 不改 capability record 结构。
- 不重开 serializer / diff / docs truth 已闭合批次。
- 不扩大到 `EarlyDataSupport` / `ZeroRTTSupport` /
  `PostHandshakeAuthSupport` 等当前不在 `TSSLFeature` 枚举里的 support-only 字段。

## Approach
1. 新增 focused runtime contract：
   - 对每个 available backend 读取：
     - `LLib.IsFeatureSupported(AFeature)`
     - `LLib.GetCapabilities`
   - 逐项验证 `TSSLFeature` 枚举当前 7 条 feature 的 published truth parity
2. 先跑合同：
   - 若直接 RED，最小修正相关 backend 的 runtime probe 或 capability 发布面
   - 若直接 GREEN，保留该 proof 作为后续 capability 路线的稳定基线
3. 更新 planning files，并记录这条 proof gap 已闭环。

## Commands
```bash
mkdir -p tmp/test_backend_feature_capability_parity_contract && \
  fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_backend_feature_capability_parity_contract \
  -FEtmp/test_backend_feature_capability_parity_contract \
  -otmp/test_backend_feature_capability_parity_contract/test_backend_feature_capability_parity_contract \
  tests/test_backend_feature_capability_parity_contract.pas && \
  ./tmp/test_backend_feature_capability_parity_contract/test_backend_feature_capability_parity_contract
git diff --check
git status --short
```

## Expected Outputs
- `IsFeatureSupported(...)` 与 `GetCapabilities` 对 `TSSLFeature`
  枚举当前覆盖的 7 条 feature 保持同一套 published truth
- 后续 capability dual-truth 路线不再缺少这层 runtime consumer parity proof

## Closeout
- focused contract 已在 Linux 本机编译并运行通过：
  - `OpenSSL`
  - `WolfSSL`
  - `MbedTLS`
  - `FreePascal Native`
- `Windows Schannel` 在当前非 Windows 环境被正确标记为
  `[SKIP] not available`，不影响这条合同在可运行 backend 上验证
- 结果表明当前 `ISSLLibrary.IsFeatureSupported(...)` 与
  `GetCapabilities` 对 `TSSLFeature` 当前 7 条 feature
  维持同一套 published truth
- 本批没有发现新的 backend source drift；
  closeout 仅补上 focused runtime proof，并清掉合同自身的
  `unreachable code` warning 噪音
