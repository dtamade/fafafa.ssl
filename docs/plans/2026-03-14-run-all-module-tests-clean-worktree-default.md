# run_all_module_tests：默认不污染工作区（2026-03-14）

## Goal
- 本地执行 `scripts/run_all_module_tests.sh` 时，**默认不改写仓库内已跟踪的 `bin/` 二进制**（避免 `git status` 变脏）。
- 修复脚本 usage/文档对默认输出目录的漂移，并提供一个**轻量可审查**的 `--dry-run` 入口用于契约测试。

## Non-Goals
- 不移除/不重命名仓库中现有 `bin/` 产物（它们仍可用于复现与手工验证）。
- 不改动任何模块测试逻辑（仅调整输出目录与增加 dry-run）。

## Approach
1) 调整 `scripts/run_all_module_tests.sh` 默认输出目录：
   - 默认 `BIN_DIR` 改为 `./tmp/` 下的 run_id 隔离目录（避免覆盖 `bin/` 中的跟踪产物）。
   - 默认 `REPORTS_DIR` 与现状保持一致（仍在 `./tmp/` 下的 run_id 隔离目录）。
2) 新增 `--dry-run`：
   - 仅打印解析后的配置（run_id、modules、BIN_DIR、REPORTS_DIR、FPC_UNIT_OUTPUT_DIR 等）并退出 0。
   - `--dry-run` 不要求 `fpc` 存在，也不创建目录/文件（便于在无编译器环境下跑脚本契约）。
3) 契约测试：
   - 增加脚本契约，断言 `--dry-run` 下默认 `BIN_DIR` 位于 `./tmp/`，且不会导致 `git status --porcelain` 变化。
4) 文档同步：
   - 更新 `docs/testing/P2_OFFLINE_FIXTURE_GUIDE.md` 中对报告路径的示例，明确：
     - 默认输出在 `./tmp/`（run_id 隔离）
     - 若需要落盘证据到 `test-reports/`，请显式传 `--reports-dir test-reports`。

## Files
- `scripts/run_all_module_tests.sh`
- `tests/scripts/test_run_all_module_tests_dry_run_paths_contract.sh`
- `docs/testing/P2_OFFLINE_FIXTURE_GUIDE.md`

## Step-by-step
1) 运行契约（无需 FPC）：
   - `bash tests/scripts/test_run_all_module_tests_dry_run_paths_contract.sh`
2) Shell 语法检查：
   - `bash -n scripts/run_all_module_tests.sh`
3) 可选：本地最小门禁（不污染工作区）：
   - `bash scripts/run_minimal_ci_gate.sh --fast-local --skip-compile`

## Expected Outputs / Acceptance
- `bash scripts/run_all_module_tests.sh --dry-run` 输出的 `Binary output dir:` 位于 `./tmp/`。
- `--dry-run` 前后 `git status --porcelain` 不变化。
- `bash -n scripts/run_all_module_tests.sh` 通过。
