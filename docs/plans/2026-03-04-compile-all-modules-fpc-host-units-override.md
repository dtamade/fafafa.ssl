# compile_all_modules fpc host/units override (2026-03-04)

## Goal
让 `scripts/compile_all_modules.py` 支持可配置 FPC 执行器与 units base，降低对默认本机布局的依赖。

## Architecture / Scope
- `scripts/compile_all_modules.py`
  - 新增 `FAFAFA_FPC_EXE` 环境变量默认值。
  - 新增 `FAFAFA_FPC_UNITS_BASE` 环境变量默认值。
  - 新增 CLI 参数：
    - `--fpc-exe`
    - `--fpc-units-base`
  - `build_fpc_command` 支持注入 `fpc_exe` 与 `unit_paths`。

## Files
- Modify: `scripts/compile_all_modules.py`
- Add: `tests/scripts/test_compile_all_modules_fpc_host_units_override_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：
   - 新增 contract，锁定 env/CLI 覆盖与命令构造语义。
2. GREEN：
   - 最小改造 `compile_all_modules.py`，保持编译流程与现有隔离能力。
3. Regression：
   - 新合同。
   - 既有 `test_compile_all_modules_unit_output_isolation_contract.sh`。
   - `python3 -m py_compile` + 合同语法检查。

## Expected Outputs
- 支持通过 env 或 CLI 指定 FPC 可执行与 unit base。
- 既有 `-FU` 隔离合同保持通过。
