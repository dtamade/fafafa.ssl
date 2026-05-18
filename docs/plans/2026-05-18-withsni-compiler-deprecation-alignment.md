# WithSNI Compiler Deprecation Alignment

## Goal

把 `TSSLContextBuilder.WithSNI(...)` 收成真正的源码级 compatibility-only surface：

- public declaration 进入编译期 `deprecated`
- intentional compatibility tests 对这条已知 deprecated surface 做局部 warning quarantine
- focused contract 守住 `WithSNI(...)` 的 compiler-deprecated truth

## Architecture

- 不改 runtime 行为：
  - `BuildClient` / `BuildServer` 继续 `warning + ignore`
- 收紧 source truth：
  - `ISSLContextBuilder.WithSNI(...)` declaration -> compiler `deprecated`
  - `TSSLContextBuilderImpl.WithSNI(...)` declaration -> compiler `deprecated`
- 收紧 workflow truth：
  - 仍故意使用 `.WithSNI(...)` 的 compatibility tests 做局部 warning suppression
  - 普通测试继续禁止重新示范 `.WithSNI(...)`

## Files

- Add: `docs/plans/2026-05-18-withsni-compiler-deprecation-alignment.md`
- Add: `tests/scripts/test_withsni_compiler_deprecated_contract.sh`
- Update: `src/fafafa.ssl.context.builder.pas`
- Update: `docs/reference/API_REFERENCE.md`
- Update: selected intentional compatibility tests under `tests/` and `tests/config/`
- Update: `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
- Update: `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`
