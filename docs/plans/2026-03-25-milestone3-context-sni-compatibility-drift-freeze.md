# Milestone 3 Context/SNI Compatibility Drift Freeze

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Perform bounded fresh discovery for the remaining Milestone 3 context/SNI compatibility line on the current worktree. Only open a new implementation family if a fresh failing contract appears; otherwise freeze the milestone with current evidence.

**Architecture:** Treat the already-closed SNI families as reference, not open work. Re-verify the current builder validation/runtime/preference contracts that still define the compatibility boundary:
- builder validation guidance differentiates client vs server context-level SNI
- builder/runtime still preserves legacy context `ServerName` state where compatibility requires it
- connection-level hostname overrides still take precedence over inherited context defaults
- intentionally retained context-level compatibility tests stay explicitly labeled

**Tech Stack:** Free Pascal, shell contract tests, focused local regression runs

## Files
- Add: `docs/plans/2026-03-25-milestone3-context-sni-compatibility-drift-freeze.md`
- Reference: `src/fafafa.ssl.context.builder.pas`
- Reference: `tests/config/test_config_validation.pas`
- Reference: `tests/test_connection_builder_hostname_precedence.pas`
- Reference: `tests/test_tls_connector_hostname_override_precedence.pas`
- Reference: `tests/test_freepascal_context_server_name_inheritance.pas`
- Reference: `tests/test_context_builder_server_servername_runtime_consistency.pas`
- Reference: `tests/integration/test_cross_backend_consistency_contract.pas`
- Reference: `tests/integration/test_cross_backend_errors_contract.pas`
- Reference: `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Steps
1. Audit the current builder/test state and confirm the intended compatibility boundary is already present on disk.
2. Run focused Milestone 3 regressions:
   - compatibility-label shell contract
   - config validation warning alignment
   - connection-builder hostname precedence
   - TLS connector hostname override precedence
   - FreePascal context `ServerName` inheritance
   - builder server `ServerName` runtime consistency
3. Run adjacent integration contracts plus milestone verification:
   - cross-backend consistency contract
   - cross-backend errors contract
   - `python3 scripts/compile_all_modules.py`
   - `bash scripts/run_minimal_ci_gate.sh --fast-local`
4. If all focused evidence is green, update working memory to freeze Milestone 3 on the current worktree and move the next queue to Milestone 4 / next fresh discovery.
5. If any fresh RED appears, stop the freeze path and open a new implementation family from that failing contract only.

## Verification
- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
- `mkdir -p tmp/config_validation_context_sni_m3 && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_validation_context_sni_m3 -FEtmp/config_validation_context_sni_m3 -otmp/config_validation_context_sni_m3/test_config_validation tests/config/test_config_validation.pas && ./tmp/config_validation_context_sni_m3/test_config_validation`
- `mkdir -p tmp/connection_builder_hostname_precedence_m3 && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/connection_builder_hostname_precedence_m3 -FEtmp/connection_builder_hostname_precedence_m3 -otmp/connection_builder_hostname_precedence_m3/test_connection_builder_hostname_precedence tests/test_connection_builder_hostname_precedence.pas && ./tmp/connection_builder_hostname_precedence_m3/test_connection_builder_hostname_precedence`
- `mkdir -p tmp/tls_connector_hostname_override_precedence_m3 && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls_connector_hostname_override_precedence_m3 -FEtmp/tls_connector_hostname_override_precedence_m3 -otmp/tls_connector_hostname_override_precedence_m3/test_tls_connector_hostname_override_precedence tests/test_tls_connector_hostname_override_precedence.pas && ./tmp/tls_connector_hostname_override_precedence_m3/test_tls_connector_hostname_override_precedence`
- `mkdir -p tmp/freepascal_context_server_name_inheritance_m3 && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_context_server_name_inheritance_m3 -FEtmp/freepascal_context_server_name_inheritance_m3 -otmp/freepascal_context_server_name_inheritance_m3/test_freepascal_context_server_name_inheritance tests/test_freepascal_context_server_name_inheritance.pas && ./tmp/freepascal_context_server_name_inheritance_m3/test_freepascal_context_server_name_inheritance`
- `mkdir -p tmp/context_builder_server_servername_runtime_consistency_m3 && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_server_servername_runtime_consistency_m3 -FEtmp/context_builder_server_servername_runtime_consistency_m3 -otmp/context_builder_server_servername_runtime_consistency_m3/test_context_builder_server_servername_runtime_consistency tests/test_context_builder_server_servername_runtime_consistency.pas && ./tmp/context_builder_server_servername_runtime_consistency_m3/test_context_builder_server_servername_runtime_consistency`
- `mkdir -p tmp/cross_backend_consistency_contract_m3 && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -Fu./tests/openssl -FUtmp/cross_backend_consistency_contract_m3 -FEtmp/cross_backend_consistency_contract_m3 -otmp/cross_backend_consistency_contract_m3/test_cross_backend_consistency_contract tests/integration/test_cross_backend_consistency_contract.pas && ./tmp/cross_backend_consistency_contract_m3/test_cross_backend_consistency_contract`
- `mkdir -p tmp/cross_backend_errors_contract_m3 && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -Fu./tests/openssl -FUtmp/cross_backend_errors_contract_m3 -FEtmp/cross_backend_errors_contract_m3 -otmp/cross_backend_errors_contract_m3/test_cross_backend_errors_contract tests/integration/test_cross_backend_errors_contract.pas && ./tmp/cross_backend_errors_contract_m3/test_cross_backend_errors_contract`
- `python3 scripts/compile_all_modules.py`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `git diff --check -- docs/plans/2026-03-25-milestone3-context-sni-compatibility-drift-freeze.md task_plan.md findings.md progress.md`
