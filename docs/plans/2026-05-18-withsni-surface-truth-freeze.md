# WithSNI Surface Truth Freeze

## Goal

把 `TSSLContextBuilder.WithSNI(...)` 收成一个不再反复失真的 `v1.x` compatibility-only fluent surface：

- 不改 runtime
- 不改现有 intentional compatibility coverage
- 用 source/doc contract 固定它“deprecated but still present”的真实边界

## Architecture

- 维持当前 runtime/source reality：
  - `WithSNI(...)` 继续存在于 builder fluent surface
  - 它已经是 compiler `deprecated`
  - `BuildClient` / `BuildServer` 继续 warning + ignore
- 明确 `v1.x` surface freeze：
  - 保留当前命名/挂载位置，避免破坏现有源码兼容
  - active docs 不得重新把 `.WithSNI(...)` 当普通 builder 示例
  - active tests 里继续只允许 allowlist compatibility coverage
- 新增 focused source/doc contract：
  - 钉住 source declaration count 与 compatibility comment
  - 钉住 active docs 只允许 `API_REFERENCE` 提及 `WithSNI(...)`
  - 继续依赖现有 allowlist contract 守住 tests

## Files

- Add: `docs/plans/2026-05-18-withsni-surface-truth-freeze.md`
- Add: `tests/scripts/test_withsni_surface_truth_contract.sh`
- Update: `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
- Update: `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Commands

1. `bash -n tests/scripts/test_withsni_surface_truth_contract.sh`
2. `bash tests/scripts/test_withsni_surface_truth_contract.sh`
3. `bash tests/scripts/test_withsni_compiler_deprecated_contract.sh`
4. `bash tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
5. `git diff --check`

## Expected Outputs

- contract script syntax valid
- contract proves `WithSNI(...)` is frozen as a deprecated compatibility-only fluent surface
- existing compiler-deprecation and compatibility allowlist contracts stay green
- patch remains whitespace-clean
