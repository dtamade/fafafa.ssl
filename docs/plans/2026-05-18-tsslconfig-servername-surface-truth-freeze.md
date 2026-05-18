# TSSLConfig ServerName Surface Truth Freeze

## Goal

把 `TSSLConfig.ServerName` 收成一个不再反复失真的 `v1.x` compatibility-only surface：

- 不改 runtime 行为
- 不把它重新抬回普通 client 配置入口
- 用 source/doc contract 固定它当前的兼容地位

## Architecture

- 维持当前 runtime truth：
  - generic factory client path = warning + ignore
  - server path = reject
  - OpenSSL direct-library default-config path = warning + ignore / reject
- 明确 `v1.x` public-surface freeze：
  - `TSSLConfig.ServerName` 继续留在 record 上，避免破坏现有源码兼容
  - 但 active docs 只允许在 `API_REFERENCE` 的 compatibility note 中提及它
  - source comment / runtime warning / API note 必须同向指向 per-connection SNI
- 新增 focused source/doc contract：
  - 钉住 source comment
  - 钉住 factory / OpenSSL direct-library warning wording
  - 钉住 active docs 不得把 `TSSLConfig.ServerName` 教回普通主路径

## Files

- Add: `docs/plans/2026-05-18-tsslconfig-servername-surface-truth-freeze.md`
- Add: `tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
- Update: `docs/reference/API_REFERENCE.md`
- Update: `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
- Update: `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Commands

1. `bash -n tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
2. `bash tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
3. `bash tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
4. `git diff --check`

## Expected Outputs

- contract script syntax valid
- contract proves `TSSLConfig.ServerName` source/doc truth is frozen to compatibility-only
- existing builder/config compatibility allowlist stays green
- patch remains whitespace-clean
