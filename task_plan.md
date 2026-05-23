# Task Plan: ISSLConnection Whole-Surface Taxonomy

## Goal
Classify the current shipped `ISSLConnection` surface exactly, then pin that taxonomy into the active design docs and contracts so future batches can flow from a single source of truth.

## Status
Complete

## Current Plan
- [docs/plans/2026-05-24-isslconnection-whole-surface-taxonomy.md](docs/plans/2026-05-24-isslconnection-whole-surface-taxonomy.md)

## Done
- Established the framework excellence roadmap and anchored the repo around an explicit north-star / wave model.
- Confirmed the current shipped `ISSLConnection` surface is the correct next batch, not another reopened closed family.
- Wrote the whole-surface taxonomy into `docs/reference/INTERFACE_DESIGN_V2.md`.
- Added a focused contract that verifies the 41-method partition and the owner-mapping buckets.
- Refreshed the route/plan docs so the next batch stays aligned after this one lands.

## Verification
- `bash tests/scripts/test_isslconnection_whole_surface_taxonomy_contract.sh`
- `bash tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
- `bash tests/scripts/test_isslconnection_control_owner_path_contract.sh`
- `bash tests/scripts/test_isslconnection_text_owner_path_contract.sh`
- `bash tests/scripts/test_isslconnectioninfo_source_classification_contract.sh`
- `bash tests/scripts/test_active_roadmap_references_contract.sh`
- `bash tests/scripts/test_architecture_current_route_truth_contract.sh`
- `bash tests/scripts/test_architecture_current_public_entrypoint_truth_contract.sh`
- `git diff --check`
