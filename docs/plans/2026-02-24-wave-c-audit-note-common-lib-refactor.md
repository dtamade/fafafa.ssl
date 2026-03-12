# 2026-02-24 Wave C 审计口径公共库重构（B147/B148/B149）

## Goal
- 抽取 B147/B148/B149 重复的审计提示逻辑为共享 Bash 库，降低重复实现和漂移风险。
- 保持现有门禁判定与报告字段不变，仅做架构收敛。

## Architecture / Scope
- 新增共享库：`scripts/wave_c_audit_note_common.sh`
- 接入脚本：
  - `scripts/check_wave_c_ci_reenable_submission_pack.sh`
  - `scripts/generate_wave_c_ci_reenable_approval_brief.sh`
  - `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
- 新增合同：
  - `tests/scripts/test_wave_c_audit_note_common_contract.sh`

## Files
- `scripts/wave_c_audit_note_common.sh`
- `scripts/check_wave_c_ci_reenable_submission_pack.sh`
- `scripts/generate_wave_c_ci_reenable_approval_brief.sh`
- `scripts/run_wave_c_ci_reenable_submission_bundle.sh`
- `tests/scripts/test_wave_c_audit_note_common_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：新增 `test_wave_c_audit_note_common_contract.sh`，先验证公共库缺失导致失败。
2. GREEN：实现共享库函数并让 B147/B148/B149 三脚本统一调用。
3. Regression：回归关键 B147/B148/B149 合同与 `bash -n` 语法检查。

## Expected Outputs
- 三脚本不再各自维护重复映射/同步/一致性/白名单逻辑。
- 新公共库合同与既有关键合同全绿。
