# build_examples_linux dynamic fpc resolution (2026-03-04)

## Goal
提升 `scripts/build_examples_linux.sh` 在不同 Linux 环境的可移植性，避免对隐式 `fpc`/固定 unit 目录布局的强依赖。

## Architecture / Scope
- `scripts/build_examples_linux.sh`
  - 新增 `FPC` env 覆盖 + `command -v` 探测 + fallback candidates。
  - 新增 `FPC_UNITS` 可选覆盖；未指定时按版本候选目录探测。
  - 编译命令统一改为通过 `"$FPC"` 执行。
  - 失败日志改为项目内 `tmp/` 路径，避免固定 `/tmp/example_build.log` 冲突。

## Files
- Modify: `scripts/build_examples_linux.sh`
- Add: `tests/scripts/test_build_examples_linux_dynamic_fpc_resolution_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：
   - 新增静态合同，锁定动态 FPC/FPC_UNITS 解析语义和变量化执行。
2. GREEN：
   - 最小改造 `build_examples_linux.sh`，不改变示例编译主流程。
3. Regression：
   - 新合同通过。
   - `bash -n` 校验脚本与合同语法。

## Expected Outputs
- 脚本支持通过 `FPC` / `FPC_UNITS` 覆盖编译器与单元目录布局。
- 缺省环境下仍可自动探测常见 FPC 安装路径并继续编译示例。
