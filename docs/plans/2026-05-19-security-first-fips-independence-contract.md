# Security-First FIPS Independence Contract

## Goal

在“接口设计 + 各 backend 实现完整性”总目标下，补齐 `WithSecurityFirst` / `CreateSecurityFirstRequirements` 的行为层 proof gap，直接证明：

- security-first 模板默认不是 FIPS 偏好快捷方式
- selector 不会把 “存在 FIPS backend” 误当成 security-first 的默认前提
- builder 的 `WithSecurityFirst` 路径可以在 non-FIPS backend 上成立

## Architecture

- 不改 production selector / builder 逻辑，先补 focused contract。
- 这次合同使用 mock backends 控制能力矩阵，避免受当前主机/CI 是否恰好发布 FIPS backend 影响。
- 合同要同时覆盖两层事实：
  - requirement truth：`PreferFIPSCompliant = False`
  - downstream truth：`WithSecurityFirst` 选择/构建成功不依赖 FIPS backend

## Current Evidence

- active guide truth 已经收口到：
  - `WithSecurityFirst` 不等于默认 FIPS 路线
  - `RequirePKCS11Support` 是 runtime-aware requirement
- 但现有 focused tests 仍缺两条直接证明：
  - `CreateSecurityFirstRequirements.PlatformPreferences.PreferFIPSCompliant` 必须为 `False`
  - 当 FIPS backend 只是“可选存在”而非“默认偏好”时，security-first 仍可优先选择 non-FIPS backend

## Files

- Add: `docs/plans/2026-05-19-security-first-fips-independence-contract.md`
- Add: `tests/test_security_first_fips_independence_contract.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps

1. 记录本批计划，明确 scope 只收口 security-first/FIPS proof gap。
2. 写 environment-independent contract：
   - 注册两个 mock backends：
     - 一个 non-FIPS backend，综合 security/performance 更适合 security-first
     - 一个 FIPS-capable backend，只在显式 `PreferFIPSCompliant=True` 时翻盘
   - 证明默认 `CreateSecurityFirstRequirements` 不启用 FIPS 偏好
   - 证明默认 selector 选择 non-FIPS backend
   - 证明显式打开 `PreferFIPSCompliant` 后，selector 才切换到 FIPS backend
   - 证明 `WithSecurityFirst` builder 路径实际构建出的 context 来自 non-FIPS backend
3. 运行 focused 编译执行与 diff hygiene。
4. 同步 `task_plan.md` / `findings.md` / `progress.md` 后提交推送。

## Verification

1. `mkdir -p tmp/test_security_first_fips_units && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_security_first_fips_units -FEtmp/test_security_first_fips_units -otmp/test_security_first_fips_units/test_security_first_fips_independence_contract tests/test_security_first_fips_independence_contract.pas && ./tmp/test_security_first_fips_units/test_security_first_fips_independence_contract`
2. `git diff --check`
3. `git status --short`

## Risks

- 不要重新引入“security-first 默认等于 FIPS”的旧心智。
- 不要依赖当前 Linux/Windows 主机恰好有没有 FIPS backend；合同必须可重复。
- 不要为了补证明去扩大到 builder/server/SNI 其他问题，那是下一批范围。
