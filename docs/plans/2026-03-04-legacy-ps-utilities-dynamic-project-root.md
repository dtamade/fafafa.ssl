# legacy powershell utilities dynamic project root (2026-03-04)

## Goal
清理遗留 PowerShell 工具脚本中的硬编码仓库路径，改为参数化 `ProjectRoot` + 脚本目录推导默认值，确保脚本可在任意工作区路径复用。

## Architecture / Scope
- `scripts/rename_openssl_files.ps1`
  - 增加 `-ProjectRoot` 参数。
  - 默认通过 `$MyInvocation.MyCommand.Path` 推导项目根。
- `scripts/update_unit_declarations.ps1`
  - 增加 `-ProjectRoot` 参数。
  - 默认通过 `$MyInvocation.MyCommand.Path` 推导项目根。
- `scripts/update_uses_references.ps1`
  - 增加 `-ProjectRoot` 参数。
  - 默认通过 `$MyInvocation.MyCommand.Path` 推导项目根。
  - 路径相对输出逻辑不再依赖单一 `\` 分隔符。

## Files
- Modify: `scripts/rename_openssl_files.ps1`
- Modify: `scripts/update_unit_declarations.ps1`
- Modify: `scripts/update_uses_references.ps1`
- Add: `tests/scripts/test_rename_openssl_files_dynamic_project_root_contract.sh`
- Add: `tests/scripts/test_update_unit_declarations_dynamic_project_root_contract.sh`
- Add: `tests/scripts/test_update_uses_references_dynamic_project_root_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：
   - 新增 3 条静态 contract，要求脚本具备 `ProjectRoot` 参数、脚本目录推导、且不含历史硬编码路径。
2. GREEN：
   - 三个脚本最小改造：统一参数解析和默认项目根推导，移除硬编码路径。
3. Regression：
   - 运行 3 条新 contract。
   - 运行既有 `test_validate_all_modules_dynamic_project_root_contract.sh`（确保模式一致）。
   - 对新增 bash contract 做 `bash -n` 语法检查。

## Expected Outputs
- 3 个遗留脚本均不再绑定 `D:\projects\Pascal\lazarus\My\libs\fafafa.ssl`。
- contract 全部通过，且与既有 dynamic project root 合同保持一致。
