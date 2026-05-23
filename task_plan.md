# Task Plan: TSSLConfig Migration and Timeout Truth Closeout Audit

## Goal
Verify that the current `TSSLConfig` migration map, library-defaults split, and timeout owner truth are already aligned across source, docs, and focused contracts, and record the closeout cleanly.

## Status
Complete

## Current Plan
- No source changes required for this batch; the live contracts already match the intended `TSSLConfig` surface direction.

## Done
- Confirmed the `TSSLLibraryDefaults` public surface is already complete and passing its focused contract.
- Confirmed `TSSLConfig` migration targets, scope buckets, logging surface, option-bridge surface/default/precedence, `ServerName`, active guidance, and timeout-related guidance are all aligned on current head.
- Confirmed the current `timeout-owner-truth-resync` plan is already represented by existing focused contracts rather than a missing code path.
- Discovered one stale local script name assumption: `tests/scripts/test_tsslconfig_timeout_owner_truth_resync.sh` does not exist; the real coverage is split across the existing timeout and direct-library contracts.

## Verification
- `bash tests/scripts/test_tssllibrarydefaults_surface_contract.sh`
- `bash tests/scripts/test_tsslconfig_migration_targets_contract.sh`
- `bash tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
- `bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
- `bash tests/scripts/test_tsslconfig_option_bridge_surface_truth_contract.sh`
- `bash tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
- `bash tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
- `bash tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
- `bash tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`
- `bash tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
- `bash tests/scripts/test_connector_timeout_safety_contract.sh`
- `bash tests/scripts/test_context_builder_session_timeout_safety_contract.sh`
- `bash tests/scripts/test_migration_guide_phase24_tbuffersize_truth_contract.sh`
