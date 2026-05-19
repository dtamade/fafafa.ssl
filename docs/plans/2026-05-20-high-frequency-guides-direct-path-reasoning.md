# High-Frequency Guides Direct-Path Reasoning（2026-05-20）

## Goal
- 把几份高频 active 页面中 direct `CreateConnection(...)` 的“使用原因”讲清楚，避免读者把这些场景化示例误解成 generic facade 主路径。
- 当前要锁住的 truth：
  - `COMMON_PITFALLS` 里 direct path 是为了对比“没设 SNI vs 正确设 SNI”的 pitfall
  - `security-best-practices` 里 direct path 是为了把连接级 hostname/SNI 责任显式写出来
  - `ERROR_HANDLING_BEST_PRACTICES` 里 direct path 是因为示例关注 socket/URL ownership 与异常/Result 边界

## Why now
- generic guides、landing quickstarts、backend quickstarts、diagnostics guides
  已经分别把主路径 / direct path / diagnostic override 说清楚。
- 但剩下这几份被频繁打开的页面虽然示例本身没错，仍缺一句：
  - 为什么这里要用 direct `ISSLConnection`
  - 以及如果不需要这层低层控制，普通新代码应回到哪条主路径

## Scope
- `docs/guides/COMMON_PITFALLS.md`
- `docs/guides/security-best-practices.md`
- `docs/guides/ERROR_HANDLING_BEST_PRACTICES.md`
- `tests/scripts/test_high_frequency_guides_direct_path_reasoning_contract.sh`
- `docs/plans/2026-05-20-high-frequency-guides-direct-path-reasoning.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals
- 不修改 Pascal source。
- 不删除 direct `CreateConnection(...)` 示例。
- 不重做已经通过的 SNI/connector 主路径 contract。

## Approach
1. 新增 focused shell contract，冻结：
   - `COMMON_PITFALLS`
     必须说明 direct path 只是为了把 SNI pitfall 对比写清楚；普通客户端仍可优先 `TSSLConnector`
   - `security-best-practices`
     必须说明 direct path 是为了把 hostname/SNI 的连接级责任显式展开；不需要低层控制时可继续用 connector
   - `ERROR_HANDLING_BEST_PRACTICES`
     必须说明 direct path 是因为示例正在讨论 URL->socket ownership + exception/result 边界
2. 先跑合同拿到 RED。
3. 做最小文档修复。
4. 跑 focused 合同与相关旧合同。
5. 更新 planning files 后提交推送。

## Commands
```bash
bash -n tests/scripts/test_high_frequency_guides_direct_path_reasoning_contract.sh
bash tests/scripts/test_high_frequency_guides_direct_path_reasoning_contract.sh
git diff --check
git status --short
```

## Expected Outputs
- 高频页面不再把 direct path 误教成“看起来像主路径的普通示例”
- 三页都能明确说出为什么这里要直接下探 `ISSLConnection`
- 将来如果这些页面又回漂，focused contract 会立即报警

