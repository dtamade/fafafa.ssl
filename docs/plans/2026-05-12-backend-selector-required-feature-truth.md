# Backend Selector Required-Feature Truth

## Goal

修复 `src/fafafa.ssl.backend.selector.pas` 对 `TSSLRequirements.RequiredFeatures` 的评估漂移：当前 selector 只检查 `SNI/ALPN`，其余 `SessionCache/SessionTickets/Renegotiation/OCSPStapling/CertificateTransparency` 等必需功能没有真正参与筛选。

## Architecture

- 保持 capability producer 不动：这批不重写各后端 `GetCapabilities`。
- 先写 focused RED 合同，证明 public selector 会把不满足 `sslFeatRenegotiation` 的后端也放进结果，且 required-feature 维度没有真正参与计数/过滤。
- 最小修法只改 selector：
  - 增加本地 helper，把 `TSSLFeature` 映射到 capability truth
  - 对带 support-level 的功能，以 support-level 作为真相源
  - requirement 语义按“功能存在即可”处理，因此 `stable / experimental / deprecated` 都算满足，只有 `none` 不满足
- 不改 serializer/diff/docs；这批只收口 public backend-selection 真实行为。

## Files

- Add: `docs/plans/2026-05-12-backend-selector-required-feature-truth.md`
- Add: `tests/test_backend_selector_required_feature_truth.pas`
- Modify: `src/fafafa.ssl.backend.selector.pas`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Steps

1. 更新 working-memory，锁定范围为 selector required-feature truth。
2. 写 focused RED 合同：
   - 用最小 `TLS12 + single feature` requirement 基线隔离 feature 参与度
   - 验证 selector 的 required 统计会把该 feature 一起算进去
   - 验证返回的每个候选后端都必须真实具备对应 support-level `<> sslSupportNone`
3. 最小实现 selector feature helper，并收紧 required-feature 评估。
4. 跑 focused 回归、diff hygiene、review 后提交。

## Verification

1. `fpc -Fu./src -Fu./tests tests/test_backend_selector_required_feature_truth.pas -otmp/test_backend_selector_required_feature_truth && ./tmp/test_backend_selector_required_feature_truth`
2. `git diff --check`
3. `git status --short`

## Risks

- 不要把 requirement 语义误改成“只有 stable 才算满足”；deprecated 仍然表示功能存在。
- 不要顺手重构 capability serializer/diff，这会扩大批次。
- 不要让 selector 重新依赖旧布尔字段作为主真相。
- 不要在 focused 测试里继续复用 `CreateDefaultRequirements(optBalanced)`，否则默认最低分门槛会掩盖单 feature requirement 的真实过滤行为。
