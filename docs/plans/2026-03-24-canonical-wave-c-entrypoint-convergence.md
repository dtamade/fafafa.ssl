# Canonical Wave C entrypoint convergence（2026-03-24）

## Goal
- 收口主入口文档里的 Wave C 默认导航与当前工程命令，避免继续把历史 B121/B127 手册或过时的 `build_linux.sh` 当成默认推进入口。
- 统一以下 5 个入口文件的默认指向：
  - `README.md`
  - `docs/README.md`
  - `docs/DOCUMENTATION_INDEX.md`
  - `docs/guides/GETTING_STARTED.md`
  - `docs/guides/QUICKSTART.md`

## Architecture / Approach
1. 保持范围为 docs-only family：
   - 不改 `src/`
   - 不改脚本行为
   - 只修正文档导航和当前 build/test guidance
2. 默认导航统一到：
   - `docs/test_reports/WAVE_C_CLOSEOUT_STATUS_2026-03-18.md`
   - `docs/test_reports/WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md`
3. 历史 `B121` / `B127` 保留可达，但只允许以“历史参考/归档参考”身份出现。
4. active build/test guidance 统一强调：
   - `python3 scripts/compile_all_modules.py`
   - `bash scripts/run_minimal_ci_gate.sh --fast-local`
   - `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local`

## Files
- `docs/plans/2026-03-24-canonical-wave-c-entrypoint-convergence.md`
- `tests/scripts/test_canonical_wave_c_entrypoint_convergence_contract.sh`
- `README.md`
- `docs/README.md`
- `docs/DOCUMENTATION_INDEX.md`
- `docs/guides/GETTING_STARTED.md`
- `docs/guides/QUICKSTART.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused shell contract，锁住这 5 个入口文件的 canonical Wave C 导航与当前命令口径。
2. 运行 RED，确认当前树先在缺少 canonical 入口段落或仍保留旧 active guidance 的文件上失败。
3. 最小更新 5 个文档：
   - README 明确默认导航顺序与历史参考定位
   - docs README 增加当前工程入口与默认命令
   - 文档索引显式区分 canonical current-chain 与 historical pages
   - GETTING_STARTED / QUICKSTART 移除 `build_linux.sh` 作为 active build/test guidance
4. 重跑 contract、focused grep 与 diff hygiene。

## Expected Outputs
- 5 个主入口文档都把当前 Wave C closeout/current-chain 页面作为默认导航。
- `B121` / `B127` 仍然可达，但只以历史参考身份出现。
- `GETTING_STARTED.md` 与 `QUICKSTART.md` 的“下一步”不再把 `build_linux.sh` 当成默认 build/test 命令。
- focused contract 通过，且 `git diff --check` 干净。
