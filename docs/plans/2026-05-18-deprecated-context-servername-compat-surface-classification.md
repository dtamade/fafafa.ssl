# Deprecated Context ServerName Compatibility Surface Classification

## Goal

把 final public surface cleanup prep 的第一刀收成一条纯静态、可复用的护栏：

- 普通 smoke / edge-case 测试不再顺手示范 deprecated `WithSNI(...)` 或 `TSSLConfig.ServerName`
- 剩余必须保留的 builder/config compatibility coverage 全部显式标成 `INTENTIONAL_COMPAT`
- 用 focused source contract 守住“compatibility-only surface 只存在于 allowlist 测试里”

## Architecture

- 迁掉普通测试里已经没有必要的旧入口：
  - `tests/test_quick.pas`
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
- 保留但显式分类：
  - builder/config compatibility warning / import-export / snapshot / merge / runtime-clarification suites
  - `TSSLConfig.ServerName` 的 public record field-surface coverage
- 新增 focused contract：
  - 允许名单里的文件必须带 `INTENTIONAL_COMPAT`
  - 普通测试若重新出现 `.WithSNI(...)` 或 `*Config*.ServerName :=`，直接红灯

## Files

- Add: `docs/plans/2026-05-18-deprecated-context-servername-compat-surface-classification.md`
- Add: `tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
- Update: selected tests under `tests/` and `tests/config/`
- Update: `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
- Update: `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps

1. Find remaining `WithSNI(...)` / builder-config `ServerName :=` usage in active tests.
2. Remove deprecated surface from ordinary smoke / edge-case flows where it is no longer part of the tested behavior.
3. Add `INTENTIONAL_COMPAT` labels to the remaining compatibility-only coverage.
4. Add a focused contract that confines these deprecated builder/config surfaces to the explicit allowlist.
5. Re-run only the new contract plus one ordinary smoke compile/run to confirm the guidance cleanup did not break normal builder usage.

## Expected Outputs

- Deprecated builder/config ServerName guidance no longer leaks through ordinary tests
- Remaining compatibility-only surface is durable, searchable, and explicitly classified
- Next session can move straight to API-shape decisions for:
  - `TSSLConfig.ServerName`
  - `WithSNI(...)`
  - direct `ISSLContext.SetServerName/GetServerName`
