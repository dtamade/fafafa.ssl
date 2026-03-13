# 2026-03-13：Wave B Gate fast-local（保持工作区干净）

## Goal

- 让 `scripts/run_wave_b_ci_gate.sh` 支持 **不污染 git 工作区** 的本地执行模式（输出落到 `./tmp` 或指定目录）。
- 保持默认行为不变：不传新参数时，仍输出到 `test-reports/`（便于长期归档与跨平台汇总脚本复用）。

## Scope

- 生产脚本：
  - `scripts/run_wave_b_ci_gate.sh`
- 合同测试：
  - `tests/scripts/test_wave_b_ci_gate_fast_local_clean_worktree_contract.sh`
- 文档同步（如需要）：
  - `docs/plans/PHASE3_MINIMAL_CI_GATE_DRAFT.md`（补充 Wave B 的 fast-local 入口）
- 记录文件（证据与决策）：
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Non-Goals

- 不改变 Wave B gate 的判定逻辑（compile/modules/examples/purity/bench 的 PASS/FAIL 语义不变）。
- 不引入 worktree / 不做分支重写；本批次直接落到 `master` 并 push 到 `origin/master`。

## Design

新增两类输出隔离入口：

- `--fast-local`
  - 默认把 reports 目录切到 `tmp/wave_b_ci_gate_reports_<run_id>/`（run_id 仍为 `YYYYMMDD_HHMMSS`）。
  - 用于本地快速验证（不会写入仓库中跟踪/可见的 `test-reports/`）。
- `--reports-dir DIR`
  - 显式指定 reports 根目录（**相对项目根目录**）。
  - 所有默认产物（summary/logs/examples json）都落在该目录下。

覆盖规则（优先级从高到低）：
1) 显式参数 `--summary-out` / `--examples-report`（仍按“相对项目根目录”解析）
2) `--reports-dir`
3) `--fast-local` 默认目录
4) 旧默认目录 `test-reports/`

安全约束：
- `--reports-dir` / `--summary-out` / `--examples-report` 均要求是 **相对路径**（防止把产物散落到项目外部）。

## Steps

1) 修改 `scripts/run_wave_b_ci_gate.sh`：
   - 增加参数解析：`--fast-local`、`--reports-dir`
   - 将 logs/summary/examples report 的默认路径改为基于 `REPORTS_DIR`
   - `mkdir -p "$REPORTS_DIR"`（而非硬编码 `test-reports/`）
2) 增加合同 `tests/scripts/test_wave_b_ci_gate_fast_local_clean_worktree_contract.sh`：
   - 对比 `git status --porcelain` 前后输出一致
   - 运行 `bash scripts/run_wave_b_ci_gate.sh --fast-local --dry-run`（覆盖 log + summary 的写入路径）
3) 回归验证：
   - `bash -n scripts/run_wave_b_ci_gate.sh`
   - `bash tests/scripts/test_wave_b_ci_gate_fast_local_clean_worktree_contract.sh`
   - 按需跑全量门禁：`bash scripts/run_minimal_ci_gate.sh --fast-local` + `bash scripts/run_wave_b_ci_gate.sh --fast-local ...`
4) 更新 `task_plan.md` / `findings.md` / `progress.md`（记录：问题、修复、证据命令）。
5) 推送：
   - `git push origin master`
   - `git push origin archive/pre-onboarding-snapshot-2026-03-13`

## Expected Outputs / Acceptance

- `bash scripts/run_wave_b_ci_gate.sh --fast-local --dry-run` 执行后：
  - `git status --porcelain` 无变化
  - 产物落在 `tmp/` 下（被 `.gitignore` 覆盖）
- 不带 `--fast-local/--reports-dir` 时：
  - summary/logs 仍生成到 `test-reports/`，保持与现有 cross-platform 汇总脚本兼容

