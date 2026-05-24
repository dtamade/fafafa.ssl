# TSSLContextConfig Surface Adoption

## Goal

Deliver the first real implementation slice of `TSSLConfig` scope surgery by
adding an additive, context-safe public configuration surface:

- `TSSLContextConfig`
- `CreateDefaultContextConfig(...)`
- `ContextConfigFromSSLConfig(...)`
- `SSLConfigFromContextConfig(...)`
- `TSSLFactory.CreateContext(const TSSLContextConfig)`

This keeps `TSSLConfig` fully compatible in `v1.x` while giving new code a
narrower surface that does not carry library-scoped defaults,
connection-scoped hints, or compatibility-only SNI fields.

## Boundary

This batch is additive only:

- no public field removals
- no backend behavior changes
- no change to `TSSLFactory.CreateContext(const TSSLConfig)`
- no change to option-bridge precedence

The projection from legacy `TSSLConfig` must preserve the frozen
option-bridge precedence by folding the three compatibility booleans into
`Options` before creating `TSSLContextConfig`.

## Scope

- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.pas`
- `src/fafafa.ssl.factory.pas`
- `tests/test_tsslcontextconfig_surface.pas`
- `tests/scripts/test_tsslcontextconfig_surface_contract.sh`
- `docs/reference/API_REFERENCE.md`
- `docs/plans/2026-05-25-tsslconfig-scope-surgery-blueprint.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification

```bash
bash tests/scripts/test_tsslcontextconfig_surface_contract.sh
bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh
bash tests/scripts/test_tsslconfig_migration_targets_contract.sh
bash tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh
bash tests/scripts/test_tssllibrarydefaults_surface_contract.sh
bash tests/scripts/test_active_roadmap_references_contract.sh
bash tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh
FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc python3 scripts/compile_all_modules.py --rebuild
git diff --check
```

## Expected Result

- New code can create context-safe config without touching mixed-scope
  `TSSLConfig` directly.
- Existing `TSSLConfig` callers keep working.
- Legacy option-bridge boolean precedence is preserved when projecting from
  `TSSLConfig` to `TSSLContextConfig`.
- Existing roadmap and scope/migration contracts remain green.
