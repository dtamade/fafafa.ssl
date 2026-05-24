# TSSLConfig Scope Surgery Blueprint

## Goal

Promote the `TSSLConfig` scope-surgery blueprint to the current active
framework-excellence batch, after the connection-boundary owner-surface sync
has closed.

This batch keeps runtime behavior unchanged. Its job is to make the next
implementation route explicit and contract-checked, so future work does not
fall back into treating `TSSLConfig` as a universal configuration record.

## Architecture Boundary

`TSSLConfig` remains a `v1.x` compatibility record. The current source truth and
API reference already split it into:

- ordinary context/config fields
- library-scoped defaults
- context-scoped fields
- connection-scoped hints
- compatibility-only fields
- compatibility-only option-bridge flags

This batch does not remove public fields, add runtime behavior, or rewire any
backend. It activates the blueprint and points follow-up implementation toward
additive, owner-correct surfaces only.

## Scope

- `docs/plans/2026-05-25-tsslconfig-scope-surgery-blueprint.md`
- `docs/ROADMAP.md`
- `tests/scripts/test_active_roadmap_references_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Execution Steps

1. Add a failing roadmap contract that requires the active batch to point at
   this `TSSLConfig` blueprint.
2. Create this plan and update `docs/ROADMAP.md` so route selection no longer
   points at the finished connection-boundary wave.
3. Re-run the active roadmap contract and existing `TSSLConfig` scope/migration
   contracts.
4. Update root working records and finish with whitespace/diff checks.

## Verification

```bash
bash tests/scripts/test_active_roadmap_references_contract.sh
bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh
bash tests/scripts/test_tsslconfig_migration_targets_contract.sh
bash tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh
git diff --check
```

## Expected Result

- `docs/ROADMAP.md` points `current_active_batch` and `next_route_candidate` at
  this plan.
- Active roadmap verification fails if the repo drifts back to the finished
  connection-boundary batch.
- The existing `TSSLConfig` scope bucket, migration-target, and active-guidance
  contracts remain green.
- The next real implementation slice can start from a clear question:
  whether an additive `TSSLContextConfig`/projection surface reduces
  mixed-scope usage without breaking `v1.x` compatibility.
