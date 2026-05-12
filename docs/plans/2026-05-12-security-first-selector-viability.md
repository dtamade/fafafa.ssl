# Security-First Selector Viability

## Goal

修复 `CreateSecurityFirstRequirements` 默认门槛和当前 shipped capability/security-score 真值的脱节，避免推荐的 security-first 选择模板在常见 OpenSSL 可用场景下直接落成“无可选后端”。

## Architecture

- 保持评分函数不动：这批不重算 `GetSecurityScore(...)` 的全局权重。
- 先写 focused RED 合同，证明：
  - 存在满足 security-first 硬性协议/算法 requirement 的可用后端
  - 但 `CreateSecurityFirstRequirements.MinSecurityScore` 仍高于这些后端的真实安全分
  - 因而 `SelectBestBackend(...)` 返回空
- 最小修法只改 requirement 模板阈值，让默认 security-first 模板重新与当前能力矩阵真值一致。
- 需要同步文档里的阈值描述，避免代码/文档继续漂移。

## Current Evidence

- runtime diagnosis before the fix:
  - available backends: `OpenSSL`, `FreePascal`
  - both satisfy the hard security-first protocol/cipher/hash/key-exchange requirement sets
  - strongest eligible backend security score on this host: `80`
  - current template threshold before fix: `85`
  - selector result before fix: none
- expected smallest safe change:
  - lower the requirement template threshold to the currently attainable shipped truth
  - keep the TLS 1.3 / modern cipher/hash / forward secrecy hard requirements unchanged

## Files

- Add: `docs/plans/2026-05-12-security-first-selector-viability.md`
- Add: `tests/test_backend_selector_security_first_viability.pas`
- Modify: `src/fafafa.ssl.backend.selector.pas`
- Modify: `docs/BACKEND_SELECTION_GUIDE.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Steps

1. 更新 working-memory，锁定范围为 security-first viability。
2. 写 focused RED 合同：
   - 找到满足 security-first 硬性协议/算法 requirement 的可用后端
   - 证明这些后端的最高真实安全分仍低于当前 `MinSecurityScore`
   - 证明 `SelectBestBackend(CreateSecurityFirstRequirements, ...)` 因此返回空
3. 最小实现：把 security-first 模板阈值收口到当前 shipped truth。
4. 跑 focused 回归、相邻 smoke、diff hygiene、review 后提交。

## Verification

1. `fpc -Fu./src -Fu./tests tests/test_backend_selector_security_first_viability.pas -otmp/test_backend_selector_security_first_viability && ./tmp/test_backend_selector_security_first_viability`
2. `fpc -Fu./src -Fu./tests tests/test_backend_selector_basic.pas -otmp/test_backend_selector_basic && ./tmp/test_backend_selector_basic`
3. `fpc -Fu./src -Fu./tests tests/test_builder_integration.pas -otmp/test_builder_integration && ./tmp/test_builder_integration`
4. `git diff --check`
5. `git status --short`

## Risks

- 不要为了让 `security-first` 选得出来而削弱 TLS 1.3 / 现代 cipher/hash / forward secrecy 的硬要求。
- 不要在没有 fresh evidence 的情况下调整全局 `GetSecurityScore(...)` 打分体系，这会扩大批次。
- 不要只改测试；文档里写死的 `85` 也必须跟代码一起收口。
- 相邻 builder smoke 里如果继续出现 `Server context requires a certificate`，应视为下一批 server-context 语义问题，不要在本批顺手混进来。
