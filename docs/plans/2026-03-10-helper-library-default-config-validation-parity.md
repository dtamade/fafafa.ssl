# 2026-03-10 helper library default config validation parity

## Goal
- 让 `tests/helpers/*.inc` 的 fake library 在 `SetDefaultConfig` 上对齐真实 backend 的 owner normalization 与 request-only field validation。
- 避免 helper fixture 接受真实 backend 会拒绝的非法 library default config。

## Architecture
- 真实 backend 的 `SetDefaultConfig` 都会做三件事：owner 字段归一化、library-default 字段校验、配置规范化。
- 当前 helper fixture 仍然直接吞掉 `AConfig`，因此会错误接受 `CertificateFile` / `CAFile` / 非默认 `HandshakeTimeout` 等 request-only 字段，也不会把 `LibraryType` / `ContextType` 归一化。
- 最小修复是在 helper fixture 的 `SetDefaultConfig` 中复用 `TSSLFactory.NormalizeLibraryDefaultOwnerFields`、`ValidateLibraryDefaultConfigFields` 与 `NormalizeConfig`。

## Files
- `docs/plans/2026-03-10-helper-library-default-config-validation-parity.md`
- `tests/test_helper_library_default_config_validation_parity.pas`
- `tests/helpers/test_fake_default_backend_fixture.inc`
- `tests/helpers/test_backend_store_fake_fixture.inc`
- `docs/PLANS_CURRENT_INDEX.md`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. Add a focused RED contract for helper `SetDefaultConfig` parity.
2. Verify RED by compiling/running the new Pascal test.
3. Patch helper fixtures to normalize/validate library defaults.
4. Re-run focused helper regressions.
5. Update working memory and current summary.

## Expected Verification
- `fpc -Fu./src tests/test_helper_library_default_config_validation_parity.pas -otmp/test_helper_library_default_config_validation_parity`
- `./tmp/test_helper_library_default_config_validation_parity`
- `fpc -Fu./src tests/test_helper_create_context_default_config_consistency.pas -otmp/test_helper_create_context_default_config_consistency`
- `./tmp/test_helper_create_context_default_config_consistency`
- `fpc -Fu./src tests/test_helper_fake_log_callback_snapshot_parity.pas -otmp/test_helper_fake_log_callback_snapshot_parity`
- `./tmp/test_helper_fake_log_callback_snapshot_parity`
- `fpc -Fu./src tests/config/test_config_snapshot_clone.pas -otmp/test_config_snapshot_clone`
- `./tmp/test_config_snapshot_clone`
- `fpc -Fu./src tests/config/test_config_import_export.pas -otmp/test_config_import_export`
- `./tmp/test_config_import_export`
- `fpc -Fu./src tests/test_context_builder_backend_store_consistency.pas -otmp/test_context_builder_backend_store_consistency`
- `./tmp/test_context_builder_backend_store_consistency`
