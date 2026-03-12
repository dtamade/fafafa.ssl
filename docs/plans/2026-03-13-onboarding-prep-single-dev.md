# 2026-03-13 onboarding prep (single developer)

## Goal
- 用最短路径把“能改、敢改、改得可验证”搭起来：能编译、能跑最小门禁、知道入口与回归路径。
- 明确下一波要推进的工作切片（优先 pure Pascal client / builder / backend selection 其一）。

## Current Truth Snapshot
- OS: Linux (Debian)
- Toolchain:
  - `fpc`: 3.3.1
  - `openssl`: 3.5.4
  - `python3`: 3.13.5
- Git:
  - branch: `master`
  - workspace 可能包含大量本地变更（建议先决定是否需要收口到“可复现基线”）

## Workspace Hygiene (推荐先做一次选择)
### Option A: 保留当前工作区（最快）
- 适合：你正在延续已有批次，不想打断。
- 风险：后续定位回归/对齐 contract 时，容易混入历史噪音。

### Option B: 做一个干净基线（推荐）
- 适合：今天目标是熟悉项目 + 准备开发节奏。
- Suggested commands:
  - `git status -sb`
  - `git stash push -u -m "wip: pre-onboarding snapshot (2026-03-13)"`（如需保留当前状态）
  - `git switch -c onboarding/2026-03-13`（可选：把 onboarding 与实现批次隔开）

## Read Order (90 分钟内完成)
1. `docs/testing/CURRENT_HEALTH.md`（最短健康门禁与意义）
2. `docs/README.md` + `docs/DOCUMENTATION_INDEX.md`（文档导航）
3. `docs/reference/ARCHITECTURE.md`（Core/Advanced/Backend contract 的“真相源”）
4. `docs/reference/API_CONTRACT_CURRENT_INDEX.md`（当前 contract 入口索引）
5. `docs/reference/API_REFERENCE.md`（接口面概览）
6. `docs/reference/PURE_PASCAL_CLIENT_M1_CHECKLIST.md`（pure Pascal client 现状与缺口）

## Code Map (先记住 8 个入口)
- Core facade:
  - `src/fafafa.ssl.pas`
  - `src/fafafa.ssl.factory.pas`
  - `src/fafafa.ssl.context.builder.pas`
- Connection base:
  - `src/fafafa.ssl.connection.base.pas`
- Backends:
  - `src/fafafa.ssl.openssl.connection.pas`
  - `src/fafafa.ssl.winssl.connection.pas`
  - `src/fafafa.ssl.freepascal.connection.pas`

## Local Verification Loop (建议固化成肌肉记忆)
### Fast health gate (仓库默认)
- `python3 -u scripts/compile_all_modules.py`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`

### Focused test run (单文件快跑)
- `fpc -Fu./src tests/test_<name>.pas -otmp/test_<name> && ./tmp/test_<name>`

### Style check (改核心单元后)
- `python3 scripts/check_code_style.py src`

## Expected Outputs
- `compile_all_modules.py` 输出 `PASS` 且统计数字稳定上升/一致。
- `run_minimal_ci_gate.sh --fast-local` 输出 `PASS`（若失败，优先修“本地门禁 contract 漂移”，再做功能开发）。

## Decision Checkpoint (今天结束前做一个选择)
Pick one focus for the first real batch:
- A) pure Pascal client M1：补齐 checklist 缺口（hostname/system roots/custom CA/pinning 等）
- B) builder/factory/runtime contract：收口配置语义、减少 drift 面
- C) OpenSSL/WinSSL backend：对齐特定平台能力与回归面

## Next (after focus chosen)
- 在 `docs/plans/` 新建下一波的执行计划：`YYYY-MM-DD-<topic>.md`
- 每个 batch 跑完后更新：
  - `progress.md`：命令与结果
  - `findings.md`：关键决策与 root cause
  - `task_plan.md`：下一队列
