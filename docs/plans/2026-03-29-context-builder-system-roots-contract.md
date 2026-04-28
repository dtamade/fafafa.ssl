# 2026-03-29 Context Builder System Roots Contract

## Goal
- 用最小改动修复 `TSSLContextBuilder` 的两处 system-roots 契约漂移：
  - `BuildClient + WithBackend(...) + WithSystemRoots` 必须使用和 context 相同 backend 的 certificate store
  - `BuildServer + WithSystemRoots` 必须真正加载并注入 system roots，而不是只在 validation 中被视为“已配置 CA”

## Architecture
- Runtime path:
  - `src/fafafa.ssl.context.builder.pas`
  - `src/fafafa.ssl.factory.pas`
- Focused regression:
  - `tests/config/test_context_builder_system_roots_contract.pas`
- Supporting verification:
  - `tests/config/test_context_builder_try.pas`
  - `tests/config/test_preset_configurations.pas`

## Files
- Add: `docs/plans/2026-03-29-context-builder-system-roots-contract.md`
- Add: `tests/config/test_context_builder_system_roots_contract.pas`
- Modify: `src/fafafa.ssl.context.builder.pas`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Steps
1. RED
   - Add a focused contract test with mock backends and inspectable mock contexts/stores.
   - Prove:
     - client explicit backend currently injects the wrong backend store
     - server build currently does not inject any system-root store
2. GREEN
   - Resolve the concrete backend used by `BuildClient` / `BuildServer`
   - Use that same backend when creating the certificate store
   - Add missing `WithSystemRoots` handling to `BuildServer`
3. VERIFY
   - Re-run the focused contract
   - Re-run existing builder suites
   - Re-run `python3 scripts/compile_all_modules.py`

## Commands
- `mkdir -p tmp/context_builder_system_roots_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_system_roots_contract -FEtmp/context_builder_system_roots_contract -otmp/context_builder_system_roots_contract/test_context_builder_system_roots_contract tests/config/test_context_builder_system_roots_contract.pas && ./tmp/context_builder_system_roots_contract/test_context_builder_system_roots_contract`
- `mkdir -p tmp/context_builder_try && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_try -FEtmp/context_builder_try -otmp/context_builder_try/test_context_builder_try tests/config/test_context_builder_try.pas && ./tmp/context_builder_try/test_context_builder_try`
- `mkdir -p tmp/preset_configurations && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/preset_configurations -FEtmp/preset_configurations -otmp/preset_configurations/test_preset_configurations tests/config/test_preset_configurations.pas && ./tmp/preset_configurations/test_preset_configurations`
- `python3 scripts/compile_all_modules.py`

## Expected Outputs
- Focused contract first fails on:
  - wrong backend store for explicit client backend
  - missing server-side system-root injection
- After fix:
  - focused contract passes
  - builder suites stay green
  - compile gate stays green

## Done
- focused contract RED observed, then GREEN after fix
- `test_context_builder_try` => PASS
- `test_preset_configurations` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
