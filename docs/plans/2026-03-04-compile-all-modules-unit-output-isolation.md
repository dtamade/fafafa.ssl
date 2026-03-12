# compile_all_modules unit-output isolation (2026-03-04)

## Goal
为 `scripts/compile_all_modules.py` 增加隔离的 FPC unit 输出目录（`-FU`），降低并发运行多个 `fpc` 编译任务时共享产物目录导致的链接/产物竞争风险。

## Architecture / Scope
- 在 `compile_all_modules.py` 中抽出可测试的命令构建函数。
- 统一在编译命令中注入 `-FU<unit_output_dir>`。
- 默认每次运行自动创建临时隔离目录；支持显式指定目录并支持保留临时目录用于排障。
- 新增脚本合同测试，验证命令构建必须包含隔离 `-FU` 参数。

## Files
- Modify: `scripts/compile_all_modules.py`
- Add: `tests/scripts/test_compile_all_modules_unit_output_isolation_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：新增合同测试，断言 `compile_all_modules.py` 提供的命令构建接口输出包含 `-FU<isolated_dir>`。
2. RED 验证：运行合同测试，预期失败（接口尚不存在）。
3. GREEN：实现 `build_fpc_command`、隔离 unit 目录解析与生命周期管理，并接入编译流程。
4. Regression：
   - `bash tests/scripts/test_compile_all_modules_unit_output_isolation_contract.sh`
   - `python3 -m py_compile scripts/compile_all_modules.py`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- 新合同测试通过，能稳定断言命令中存在隔离 `-FU` 参数。
- `compile_all_modules.py` 全量编译通过（目标 >= 98%，当前目标 100%）。
- 默认运行日志显示本次使用的隔离 unit 输出目录。
