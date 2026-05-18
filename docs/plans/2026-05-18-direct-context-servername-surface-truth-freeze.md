# Direct Context ServerName Surface Truth Freeze

## Goal

把 direct `ISSLContext.SetServerName/GetServerName` 收成一个不再反复失真的 `v1.x` compatibility-only context API：

- 不改 backend 实现
- 不改当前 intentional test coverage
- 用 source/doc contract 固定“deprecated but still present”的真实边界

## Architecture

- 维持当前 runtime/source reality：
  - `ISSLContext.SetServerName/GetServerName` 继续存在于 public interface
  - 它们已经是 compiler `deprecated`
  - 生产源码中不再允许新增真实 context-level caller
- 明确 `v1.x` surface freeze：
  - 保留这组 API，避免破坏现有源码兼容
  - active docs 不得再把 direct context setter/getter 当普通 client 指导路径
  - active tests 里的 direct context 命中继续由现有分类合同守住
- 新增 focused source/doc contract：
  - 钉住 deprecated declaration message
  - 钉住 production source 无新增 direct context caller
  - 钉住 active docs 不得重新出现 `Ctx.SetServerName(...)` 之类指导语义

## Files

- Add: `docs/plans/2026-05-18-direct-context-servername-surface-truth-freeze.md`
- Add: `tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
- Update: `docs/reference/API_REFERENCE.md`
- Update: `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
- Update: `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Commands

1. `bash -n tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
2. `bash tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
3. `bash tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
4. `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
5. `git diff --check`

## Expected Outputs

- contract script syntax valid
- contract proves direct context ServerName API is frozen as deprecated compatibility-only surface
- existing active-test classification contracts stay green
- patch remains whitespace-clean
