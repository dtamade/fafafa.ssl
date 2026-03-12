# legacy shell tooling path hardening (2026-03-04)

## Goal
消除遗留 shell 工具脚本中的用户机器硬编码路径，提升跨环境可执行性。

## Architecture / Scope
- `scripts/fix_unit_names.sh`
  - 使用 `SCRIPT_DIR/PROJECT_ROOT` 推导仓库根目录。
  - 移除固定 `cd /home/dtamade/projects/fafafa.ssl`。
- `scripts/lazbuild_all.sh`
  - 支持 `LAZBUILD` 环境变量覆盖。
  - 默认通过 `command -v lazbuild` 解析可执行路径。
  - 增加常见候选路径回退，移除固定 home 路径。

## Files
- Modify: `scripts/fix_unit_names.sh`
- Modify: `scripts/lazbuild_all.sh`
- Add: `tests/scripts/test_fix_unit_names_dynamic_project_root_contract.sh`
- Add: `tests/scripts/test_lazbuild_all_dynamic_lazbuild_resolution_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：
   - 新增 2 条静态 contract，验证脚本目录推导/工具解析逻辑存在且无硬编码用户路径。
2. GREEN：
   - 对两个脚本做最小改造，保留原有行为路径。
3. Regression：
   - 运行 2 条新 contract。
   - 运行 `bash -n` 检查两脚本与新合同语法。

## Expected Outputs
- 两个脚本不再依赖 `/home/dtamade/...` 固定路径。
- lazbuild 可通过环境变量或 PATH 自动发现。
