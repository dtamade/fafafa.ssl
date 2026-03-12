# compile_all + coverage_report fpc path hardening (2026-03-04)

## Goal
移除 `scripts/compile_all.sh` 与 `scripts/coverage_report.sh` 中的用户机器硬编码 FPC 路径，改为动态解析（env 覆盖 + PATH 发现 + 候选回退）。

## Architecture / Scope
- `scripts/compile_all.sh`
  - `FPC` 支持环境变量覆盖，默认 `command -v fpc`。
  - PATH 未命中时从候选路径回退。
  - `FPC_UNITS` 改为可配置变量，按存在目录拼接 `-Fi/-Fu` 选项。
- `scripts/coverage_report.sh`
  - 同步引入 `FPC` 动态解析策略。
  - `FPC_UNITS` 改为可配置变量，并构建可选 unit flags 数组。

## Files
- Modify: `scripts/compile_all.sh`
- Modify: `scripts/coverage_report.sh`
- Add: `tests/scripts/test_compile_all_dynamic_fpc_resolution_contract.sh`
- Add: `tests/scripts/test_coverage_report_dynamic_fpc_resolution_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：
   - 新增 2 条静态 contract，锁定动态 FPC 解析语义并禁止硬编码 `/home/dtamade/freePascal/fpc`。
2. GREEN：
   - 最小改造两个脚本的 FPC/FPC_UNITS 解析与编译调用。
3. Regression：
   - 运行 2 条新 contract。
   - `bash -n` 校验两个脚本与新合同语法。

## Expected Outputs
- 两脚本不再写死用户路径。
- 支持通过 `FPC` / `FPC_UNITS` 环境变量注入自定义编译器布局。
