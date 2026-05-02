# Secondary build/test guidance convergence（2026-03-25）

## Goal
- 收口次级文档中的过期默认 build/test guidance，避免继续把 `build_linux.sh`、不存在的 `run_tests_linux.sh`，或历史 `B127` troubleshooting 页面当成当前默认入口。
- 统一这批 active guidance 到当前 canonical 命令：
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local`

## Architecture
- 这是一个 docs-only family，不触碰 `src/`、脚本实现，或历史 Wave 报告内容。
- 历史入口仍然保留，但只作为参考：
  - `build_linux.sh` 只保留为兼容/历史入口，不再作为默认文档路径
  - `docs/test_reports/WAVE_C_B127_LOCAL_GUARD_TROUBLESHOOTING_2026-02-09.md` 只保留为历史 troubleshooting 手册
- 本批只收口 3 个直接影响当前默认动作的次级文档，不把 `scripts/` / `test-reports/` 的后续 drift 混进来。

## Files
- `docs/plans/2026-03-25-secondary-build-test-guidance-convergence.md`
- `docs/AGENTS.md`
- `docs/guides/LINUX_QUICKSTART.md`
- `docs/guides/QUICKSTART_30SEC.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 复扫这 3 个目标文档里的旧入口命中，确认本批 drift 仍然集中在：
   - `build_linux.sh`
   - `run_tests_linux.sh`
   - `WAVE_C_B127...`
2. 更新 `docs/AGENTS.md`：
   - 默认编译入口改为 `python3 scripts/compile_all_modules.py`
   - 默认本地最小门禁改为 `bash scripts/run_minimal_ci_gate.sh --fast-local`
   - `build_linux.sh` 只保留历史兼容定位，不再作为首句默认命令
3. 更新 `docs/guides/LINUX_QUICKSTART.md`：
   - 增加当前 Wave C current-chain / default command 提示
   - 把各发行版安装后的默认动作统一到当前 build/gate 链
   - 移除不存在的 `run_tests_linux.sh` active guidance
   - FAQ / 项目结构 / 下一步 也同步收口
4. 更新 `docs/guides/QUICKSTART_30SEC.md`：
   - “下一步”改成先看 canonical Wave C closeout/current-chain
   - local-first 异常处理改成先看 current-chain，再按需回看历史 `B127`
5. 更新 `task_plan.md` / `findings.md` / `progress.md`，记录本批范围、结论与验证证据。

## Commands
```bash
rg -n "build_linux\\.sh|run_tests_linux\\.sh|WAVE_C_B127|B127" docs/AGENTS.md docs/guides/LINUX_QUICKSTART.md docs/guides/QUICKSTART_30SEC.md
rg -n "python3 scripts/compile_all_modules\\.py|bash scripts/run_minimal_ci_gate\\.sh --fast-local|bash scripts/run_phase2_performance_baseline\\.sh --dry-run --fast-local" docs/AGENTS.md docs/guides/LINUX_QUICKSTART.md docs/guides/QUICKSTART_30SEC.md
git diff --check -- docs/plans/2026-03-25-secondary-build-test-guidance-convergence.md docs/AGENTS.md docs/guides/LINUX_QUICKSTART.md docs/guides/QUICKSTART_30SEC.md task_plan.md findings.md progress.md
```

## Expected Outputs
- `docs/AGENTS.md` 不再把 `build_linux.sh` 作为默认 build 入口。
- `docs/guides/LINUX_QUICKSTART.md` 不再推荐不存在的 `run_tests_linux.sh`，默认动作统一到当前 build/gate 链。
- `docs/guides/QUICKSTART_30SEC.md` 不再把 `B127` 当成 first-stop 诊断入口，而是明确 current-chain 优先、历史页次之。
- focused grep 与 `git diff --check` 通过。
