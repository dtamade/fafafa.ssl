# Task Plan: Framework Excellence Sequential Execution Master Plan

## Objective

Absorb the remaining roadmap work into one ordered execution plan so future
rounds move through real code-heavy stages instead of reopening scope selection.

## Current State

- Latest commit before this planning batch: `a715d14 feat: route builder
  through context-safe config`.
- `TSSLContextConfig` surface adoption and builder adoption are both verified.
- `docs/ROADMAP.md` still pointed at the older `TSSLConfig` scope-surgery
  blueprint, which no longer captured the full ordered queue requested by the
  user.

## Planned Batch

- Create
  `docs/plans/2026-05-25-framework-excellence-sequential-execution-master-plan.md`.
- Update `docs/ROADMAP.md` so the active batch and next route candidate point
  to the sequential master plan.
- Update `tests/scripts/test_active_roadmap_references_contract.sh` so route
  drift back to the old blueprint fails.
- Update working records and commit the planning batch.

## Verification

- RED:
  - `bash tests/scripts/test_active_roadmap_references_contract.sh`
  - failed because the sequential master plan did not exist yet.
- GREEN:
  - `bash -n tests/scripts/test_active_roadmap_references_contract.sh`
  - `bash tests/scripts/test_active_roadmap_references_contract.sh`
  - `bash tests/scripts/test_architecture_current_route_truth_contract.sh`
  - `git diff --check`

## Review Conclusion

- Verified. The active roadmap route now points at the sequential execution
  master plan, and the old `TSSLConfig` blueprint is retained as a completed
  input rather than the active route.

# Task Plan: TSSLContextConfig Builder Adoption

## Objective

Move the recommended `TSSLContextBuilder` build path onto the new
`TSSLContextConfig` surface for stable context-scoped fields, while preserving
existing builder behavior for backend-sensitive and post-create operations.

## Current State

- `TSSLContextConfig` and `TSSLFactory.CreateContext(const TSSLContextConfig)`
  are already additive public surface.
- `TSSLContextBuilder` is the recommended high-level entrypoint for ordinary
  new code.
- Before this batch, builder still created raw contexts and manually applied
  context fields.

## Planned Batch

- Add an internal builder projection helper:
  `BuildContextConfig(AContextType, ALibraryType)`.
- Route `BuildClient` / `BuildServer` through
  `TSSLFactory.CreateContext(const TSSLContextConfig)`.
- Preserve builder post-create behavior for PEM / PKCS#11 loading, HTTP hooks,
  OCSP stapled response loading, replay-store installers, deprecated SNI
  warnings, and backend-gated custom cipher overrides.
- Add a focused builder adoption contract and runtime projection assertions.
- Keep adjacent FreePascal custom cipher capability truth aligned when the
  builder test exposes stale published/unpublished expectations.
- Update working records.

## Verification

- RED:
  - `bash tests/scripts/test_tsslcontextconfig_builder_adoption_contract.sh`
  - failed first because the builder had no internal `TSSLContextConfig`
    projection helper.
- GREEN:
  - `FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc bash tests/scripts/test_tsslcontextconfig_builder_adoption_contract.sh`
  - `bash tests/scripts/test_tsslcontextconfig_surface_contract.sh`
  - `bash tests/scripts/test_custom_cipher_capability_truth_contract.sh`
  - compile and run
    `tests/test_backend_custom_cipher_capability_truth_contract.pas`
  - `FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc python3 scripts/compile_all_modules.py --rebuild`
  - `bash -n tests/scripts/test_tsslcontextconfig_builder_adoption_contract.sh`
  - `git diff --check`

## Review Conclusion

- Verified. Builder context creation now flows through
  `TSSLFactory.CreateContext(const TSSLContextConfig)` for stable
  context-scoped fields, while post-create responsibilities stay on the
  existing builder path. The adjacent FreePascal custom-cipher capability drift
  is also closed so backend publication, runtime setters, docs, and contracts
  agree.

# Task Plan: TSSLContextConfig Surface Adoption

## Objective

Add the first runtime-usable `TSSLConfig` scope-surgery slice: a context-safe
configuration record and projection helpers that new code can use without
touching library-scoped defaults, connection-scoped hints, or compatibility-only
SNI fields.

## Current State

- `TSSLConfig` scope buckets and migration targets are contract-checked.
- `TSSLLibraryDefaults` already proved the additive-surface pattern for
  library-scoped defaults.
- The active blueprint identified additive `TSSLContextConfig` / projection as
  the next implementation slice.

## Planned Batch

- Add `TSSLContextConfig` in `src/fafafa.ssl.base.pas`.
- Add `CreateDefaultContextConfig(...)`,
  `ContextConfigFromSSLConfig(...)`, and
  `SSLConfigFromContextConfig(...)`.
- Re-export the type and helpers from `src/fafafa.ssl.pas`.
- Add `TSSLFactory.CreateContext(const TSSLContextConfig)`.
- Add focused Pascal and shell contract coverage.
- Update API reference and working records.

## Verification

- RED:
  - `bash tests/scripts/test_tsslcontextconfig_surface_contract.sh`
  - failed first because base source did not declare `TSSLContextConfig`
  - second RED exposed that legacy option-bridge precedence was not folded into
    `Options` during projection
- GREEN:
  - `bash tests/scripts/test_tsslcontextconfig_surface_contract.sh`
  - `bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - `bash tests/scripts/test_tsslconfig_migration_targets_contract.sh`
  - `bash tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`
  - `bash tests/scripts/test_tssllibrarydefaults_surface_contract.sh`
  - `bash tests/scripts/test_active_roadmap_references_contract.sh`
  - `bash tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
  - `FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc python3 scripts/compile_all_modules.py --rebuild`
  - `git diff --check`

## Review Conclusion

- Verified. `TSSLContextConfig` is now an additive, context-safe config surface
  with focused runtime coverage, legacy projection helpers, facade re-exports,
  and a factory overload. Existing `TSSLConfig` callers and option-bridge
  precedence stay protected by adjacent contracts.

# Task Plan: TSSLConfig Scope Surgery Blueprint

## Objective

Move the active route from the finished connection-boundary wave to the
`TSSLConfig` scope-surgery blueprint, keeping runtime behavior unchanged and
making the next implementation slice explicit.

## Current State

- Wave B connection-boundary owner-surface sync has been committed.
- Existing `TSSLConfig` truth is already split across scope buckets, migration
  targets, active guidance, and option-bridge contracts.
- `docs/ROADMAP.md` still pointed `current_active_batch` and
  `next_route_candidate` at the finished Wave B plan.

## Planned Batch

- `docs/plans/2026-05-25-tsslconfig-scope-surgery-blueprint.md`
- `docs/ROADMAP.md`
- `tests/scripts/test_active_roadmap_references_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification

- First red check:
  - `bash tests/scripts/test_active_roadmap_references_contract.sh`
  - expected failure: missing
    `docs/plans/2026-05-25-tsslconfig-scope-surgery-blueprint.md`
- Green checks:
  - `bash tests/scripts/test_active_roadmap_references_contract.sh`
  - `bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - `bash tests/scripts/test_tsslconfig_migration_targets_contract.sh`
  - `bash tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`
  - `git diff --check`

## Review Conclusion

- Verified. The active roadmap route now points at the `TSSLConfig` scope
  surgery blueprint, and the existing `TSSLConfig` scope/migration/guidance
  contracts stayed green. Runtime behavior remains unchanged.

# Task Plan: Wave B Connection Boundary Completion

## Objective

Sync source/doc truth around the connection-side owner surfaces and
compatibility mirrors, keeping runtime behavior unchanged.

## Current State

- The active batch has been implemented and verified on current evidence.
- Source comments already distinguish core mirrors from owner surfaces; this
  round makes that language explicit for the remaining optional owner
  interfaces and the doc surfaces that summarize them.

## Planned Batch

- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.connection.base.pas`
- `docs/ARCHITECTURE.md`
- `docs/reference/API_REFERENCE.md`
- `docs/ROADMAP.md`

## Verification

- See the batch plan in
  `docs/plans/2026-05-25-connection-boundary-completion-waveb.md`
- run `bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
- run `bash tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
- run `bash tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh`
- run `python3 scripts/compile_all_modules.py --rebuild`
- run `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local`
- finish with `git diff --check`

## Verification Result

- `tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
  passed
- `tests/scripts/test_isslcertificateverification_residual_classification_contract.sh`
  passed
- `tests/scripts/test_isslcertificateverification_root_test_residual_contract.sh`
  passed
- `tests/scripts/test_issldiagnostics_active_guidance_contract.sh` passed
- `tests/scripts/test_isslsessionresumption_active_guidance_contract.sh` passed
- `tests/scripts/test_isslocspstapling_active_guidance_contract.sh` passed
- `tests/scripts/test_architecture_optional_owner_surface_truth_contract.sh`
  passed
- `tests/scripts/test_architecture_current_route_truth_contract.sh` passed
- `tests/scripts/test_active_roadmap_references_contract.sh` passed
- `python3 scripts/compile_all_modules.py --rebuild` passed with `186/186`
  and `0` warnings
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id
  connection_boundary_waveb_20260525` passed with `18` tests passed and `0`
  failed
- `git diff --check` passed

## Review Conclusion

- Connection-side owner surface wording is now aligned across source comments,
  architecture docs, API reference, and roadmap.
- Runtime behavior stayed unchanged; the batch was documentation and comment
  truth sync only.
- The repo is ready for commit.

# Task Plan: TLS13 Early-Data Note Cleanup Wave1

## Objective

Clear the remaining compiler `Note:` families in
`tests/test_freepascal_tls13_early_data.pas` while preserving the early-data
test behavior and replay-store fixture bytes.

## Current State

- The early-data note batch is now green on current evidence.
- Focused compile and the TLS 1.3 completeness gate both passed, and the gate
  log has no `Note:` matches.
- This batch stayed tightly scoped to one file,
  `tests/test_freepascal_tls13_early_data.pas`, and preserved the replay-store
  fixture bytes.

## Planned Batch

- `tests/test_freepascal_tls13_early_data.pas`

## Verification

- Recompile the file directly.
- Grep the focused log for the target `Note:` families.
- Rerun the TLS 1.3 completeness gate with a fresh run id.
- Finish with `git diff --check` and a commit.

# Task Plan: TLS13 Source Note Cleanup Wave1

## Objective

Clear the repeated source-level compiler `Note:` families exposed by the TLS 1.3
completeness gate, starting with the shared backend/provider units and keeping
behavior unchanged.

## Current State

- The source-note batch is complete on current evidence.
- Focused compiles, the full source rebuild, and the TLS 1.3 completeness gate
  are all green for the targeted source note families.
- The remaining gate `Note:` output is test-harness-only in
  `test_freepascal_tls13_early_data.pas`, so the next batch should target that
  file separately.

## Planned Batch

- `src/fafafa.ssl.openssl.api.store.pas`
- `src/fafafa.ssl.pkcs11.provider.pas`
- `src/fafafa.ssl.cert.pinning.pas`
- `src/fafafa.ssl.wolfssl.lib.pas`

## Verification

- Recompile the affected source units.
- Grep the focused logs for the target `Note:` families.
- Rerun the TLS 1.3 completeness gate with a fresh run id.
- Finish with `git diff --check` and a commit.

## Next Queue

- `tests/test_freepascal_tls13_early_data.pas`
- Its `Note:` families are still visible in the gate log and should be handled
  as the next distinct batch.

# Task Plan: TLS13 Warning-Wave Clearance

## Objective

Clear all current `Warning:` families exposed by the FreePascal TLS 1.3
completeness gate.

## Current State

- The latest authoritative gate run is `warning_clearance_20260525`.
- The gate passed with `18` passed and `0` failed.
- `rg -n "Warning:" tmp/warning_clearance_20260525_tls13_completeness.log`
  returns no matches.
- Closed warning families in this round:
  - `Case statement does not handle all possible cases`
  - implicit string type conversion
  - `Function result does not seem to be set`
- Compiler `Note:` output remains, but notes are not part of the current
  warning-wave objective.

## Verification

- Focused compiles:
  - `tmp/case_wave1_servercertverify_compile_green.log`
  - `tmp/case_wave1_client_certificateverify_compile_green.log`
  - `tmp/case_wave1_early_data_compile_green.log`
  - `tmp/string_wave1_servercertverify_compile_green.log`
  - `tmp/function_wave1_early_data_compile_green.log`
- Focused runs:
  - `tmp/case_wave1_servercertverify_run.log`
  - `tmp/case_wave1_client_certificateverify_run.log`
  - `tmp/case_wave1_early_data_run.log`
  - `tmp/string_wave1_servercertverify_run.log`
  - `tmp/function_wave1_early_data_run.log`
- Full gate:
  - `tmp/warning_clearance_20260525_tls13_completeness.log`
  - `tmp/test-reports/freepascal_tls13_completeness_warning_clearance_20260525.md`

## Next Queue

- No remaining TLS 1.3 completeness `Warning:` waves on current evidence.
- If we continue after this batch, the next cleanup class should be tracked as
  `Note:` cleanup, not folded into warning-wave closure.

# Task Plan: TLS13 ServerCertVerify Range-Check Warning Cleanup

## Objective

Close the `Warning: Range check error while evaluating constants` family in
`tests/test_tls13_servercertverify.pas` without changing BigInt test semantics.

## Current State

- Focused red compile showed 62 warnings, with 47 range-check hits clustered in
  `TestBigIntQWordVectorSuiteWaveD/F`.
- The fix was to make the unsigned intent explicit with `QWord(...)` casts on
  the 64-bit vector literals, matching the already-clean WaveI style.
- Focused compile for `tests/test_tls13_servercertverify.pas` is clean for the
  target range-check warning family.
- The target binary passed.
- The TLS 1.3 completeness gate passed with `18` passed and `0` failed.
- Remaining gate warnings are different families:
  `Case statement does not handle all possible cases`,
  implicit string conversion, and one `Function result does not seem to be set`.

## Verification

- Focused red evidence:
  - `tests/test_tls13_servercertverify.pas` compiled with 62 warnings before the
    fix, including the range-check family at lines `3109-3168`.
- Focused green evidence:
  - `tmp/range_check_wave1_servercertverify_compile_green.log`
  - `tmp/range_check_wave1_servercertverify_run.log`
- Full gate evidence:
  - `tmp/range_check_wave1_tls13_completeness.log`
  - `tmp/test-reports/freepascal_tls13_completeness_range_check_wave1_20260525.md`

## Next Queue

- Treat the remaining `case statement`, string conversion, and
  `function result` warnings as separate waves.

## Objective

Close the module-test `Warning: Unreachable code` batch exposed by
`run_all_module_tests.sh --fast-local`.

## Current State

- Managed-result warning cleanup is closed on current evidence.
- Broad module-test compile logs then exposed test `Unreachable code` warnings
  in constant assertion blocks.
- Root cause: FPC folds direct compile-time constant comparisons and marks the
  failure branch unreachable.
- Target tests now keep the same expected values and failure messages but route
  constant operands through local `RuntimeInteger` helpers.
- Focused compile for all 9 target files is clean for
  `Warning: Unreachable code`.
- Full module test gate passes and broad module-test compile logs contain no
  `Warning:` entries.

## Verification

Completed:

- focused compile for:
  - `tests/certificate/test_p2_pkcs12.pas`
  - `tests/certificate/test_p2_ocsp.pas`
  - `tests/certificate/test_p2_ts.pas`
  - `tests/certificate/test_p2_cms.pas`
  - `tests/certificate/test_p2_ct.pas`
  - `tests/certificate/test_p2_pkcs7.pas`
  - `tests/crypto/test_p2_store.pas`
  - `tests/crypto/test_p2_comp.pas`
  - `tests/crypto/test_p4_engine.pas`
- `rg -n "Warning: Unreachable code" tmp/unreachable_code_wave1_*_compile.log`
  - result: no matches
- `FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc FAFAFA_FAST_LOCAL=1 FAFAFA_FPC_UNIT_OUTPUT_DIR=tmp/unreachable_code_wave1_module_units bash scripts/run_all_module_tests.sh --fast-local`
  - result: `22` passed, `0` failed, `0` skipped
- `rg -n "Warning:" tmp/test-reports/*20260524_235928_1700710*_compile.log`
  - result: no matches

Completed after verification:

- `git diff --check`
- final review
- commit `6ac93d7 test: clear module unreachable-code warnings`

## Per-Round Contract

Each round must have:

- One named target batch.
- Focused contract verification before broader compile checks.
- `git diff --check`.
- Brief review conclusion before commit.
- Git commit after the round.

## Next Round

After committing this batch, continue from fresh compile evidence. Keep TLS 1.3
case/range/string-conversion warnings separate from this module-test constants
cleanup.
