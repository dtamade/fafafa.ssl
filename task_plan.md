# Task Plan: Stage 3 Public Import Guidance Convergence

## Objective

Stage 3 second slice: update `docs/README.md` "快速开始" section to teach
builder as the ordinary entrypoint, replacing the old factory-direct pattern.

## Current State

- Stage 3 first slice updated the facade unit header to teach builder.
- `docs/README.md` "快速开始" still used `TSSLFactory.CreateContext(sslCtxClient)`
  as the primary example without builder.
- Root README and GETTING_STARTED already teach the two-unit pattern.

## Planned Batch

- Extend `test_public_unit_import_guidance_truth_contract.sh` to require
  `docs/README.md` teaches builder.
- Update `docs/README.md` "快速开始" to use `TSSLContextBuilder`.
- Split the direct `ISSLConnection` path into a separate "lower-level" block.
- Run focused contracts, full compile, shell syntax, and whitespace checks.

## Verification

- RED:
  - `bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
  - failed because `docs/README.md` did not import `fafafa.ssl.context.builder`.
- GREEN:
  - `bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
  - `bash tests/scripts/test_facade_main_entry_truth_contract.sh`
  - `bash tests/scripts/test_tsslconfig_stage2_public_guidance_truth_contract.sh`
  - `bash tests/scripts/test_tsslconfig_stage2_active_docs_examples_drift_contract.sh`
  - `bash tests/scripts/test_active_roadmap_references_contract.sh`
  - `FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc python3 scripts/compile_all_modules.py --rebuild`
  - `bash -n tests/scripts/test_public_unit_import_guidance_truth_contract.sh`
  - `git diff --check`

## Review Conclusion

- Verified. `docs/README.md` "快速开始" now teaches builder as the ordinary
  entrypoint. All active public-facing docs converge on the two-unit pattern.

- The previous batch completed builder ordinary material projection through
  `TSSLContextConfig`.
- `docs/ROADMAP.md` still undercounted Stage 2 progress and still pointed the
  next code-heavy slice at factory direct application, which was already done.
- README and API reference still presented the FreePascal replay-store factory
  example through legacy `TSSLConfig`, even though the context-safe factory path
  now directly owns those fields.

## Planned Batch

- Add a focused public-guidance truth contract.
- Update `docs/ROADMAP.md` so it records all four completed Stage 2 slices.
- Move README and API replay-store factory examples to `TSSLContextConfig`.
- Keep legacy `TSSLConfig` compatibility visible, but no longer teach it as the
  recommended new factory/config path.
- Run focused guidance contracts, the `TSSLContextConfig` runtime contract, full
  module compile, shell syntax, and whitespace checks.

## Verification

- RED:
  - `bash tests/scripts/test_tsslconfig_stage2_public_guidance_truth_contract.sh`
  - failed first because `docs/ROADMAP.md` still said
    "`TSSLConfig` scope surgery 已经完成前两条实现 slice".
- GREEN:
  - `bash tests/scripts/test_tsslconfig_stage2_public_guidance_truth_contract.sh`
  - `bash tests/scripts/test_active_roadmap_references_contract.sh`
  - `bash tests/scripts/test_tsslconfig_migration_targets_contract.sh`
  - `bash tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`
  - `bash tests/scripts/test_tsslcontextconfig_surface_contract.sh`
  - `bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - `FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc python3 scripts/compile_all_modules.py --rebuild`
  - `bash -n tests/scripts/test_tsslconfig_stage2_public_guidance_truth_contract.sh`
  - `git diff --check`

## Review Conclusion

- Verified. The public route and replay-store guidance now match the current
  implementation state: builder and `TSSLContextConfig` are the preferred paths,
  legacy `TSSLConfig` stays documented as compatibility, and Stage 2 no longer
  points the next work item at a completed factory-direct slice.

# Task Plan: TSSLContextConfig Builder Material Projection

## Objective

Complete the first Stage 2 code-heavy slice by moving ordinary builder
certificate/key/trust material through `TSSLContextConfig`, instead of keeping
those stable context-scoped values on the builder post-create path.

## Current State

- Stage 1 already made `TSSLFactory.CreateContext(const TSSLContextConfig)`
  apply context-safe fields directly.
- Builder adoption already routes stable protocol/session/ALPN/early-data
  fields through `TSSLContextConfig`.
- Before this batch, builder still loaded ordinary certificate files, private
  key files, CA file/path, and system roots after context creation, duplicating
  responsibilities now owned by the context-safe config factory path.

## Planned Batch

- Add a focused contract proving builder material projection happens inside
  `BuildContextConfig(...)`.
- Extend the builder runtime probe so ordinary certificate/key/CA material is
  observable after factory creation.
- Project ordinary file/trust material into `TSSLContextConfig`.
- Remove duplicate builder post-create ordinary certificate/key/CA/system-root
  loading.
- Keep PEM, PKCS#11 URI/PIN semantics, HTTP hooks, OCSP stapled response file,
  replay-store installers, and backend-gated custom cipher overrides on the
  owner-specific builder post-create path.
- Preserve the server missing-private-key validation boundary before ordinary
  certificate files can be loaded by the factory path.

## Verification

- RED:
  - `bash tests/scripts/test_tsslcontextconfig_builder_material_projection_contract.sh`
  - failed first because `BuildContextConfig(...)` did not project ordinary
    certificate/key/trust material and builder still loaded those values
    post-create.
  - after moving ordinary material, `test_context_builder_try` exposed a
    missing-private-key boundary regression before server validation was moved
    ahead of context creation.
- GREEN:
  - `bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
  - `bash tests/scripts/test_tsslconfig_migration_targets_contract.sh`
  - `bash tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`
  - `bash tests/scripts/test_tssllibrarydefaults_surface_contract.sh`
  - `bash tests/scripts/test_system_roots_public_surface_contract.sh`
  - `FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc bash tests/scripts/test_tsslcontextconfig_builder_material_projection_contract.sh`
  - `FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc bash tests/scripts/test_tsslcontextconfig_builder_adoption_contract.sh`
  - `FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc bash tests/scripts/test_tsslcontextconfig_factory_direct_application_contract.sh`
  - `FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc python3 scripts/compile_all_modules.py --rebuild`
  - `git diff --check`
  - `bash -n tests/scripts/test_tsslcontextconfig_builder_material_projection_contract.sh`
  - `bash -n tests/scripts/test_system_roots_public_surface_contract.sh`
  - `bash -n tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`

## Review Conclusion

- Verified. Ordinary builder file/trust material now flows through
  `TSSLContextConfig` and the direct factory path, while backend-sensitive and
  owner-specific builder responsibilities remain post-create. Adjacent
  `TSSLConfig` scope, system-root, builder adoption, factory direct-application,
  and full compile gates stayed green.

# Task Plan: TSSLContextConfig Factory Direct Application

## Objective

Complete Stage 1 of the framework-excellence sequential master plan by making
`TSSLFactory.CreateContext(const TSSLContextConfig)` apply context-safe fields
directly, instead of projecting the new surface back through legacy
`TSSLConfig` as its main internal transport.

## Current State

- `TSSLContextConfig` surface adoption is already additive and public.
- `TSSLContextBuilder` already routes stable context-scoped fields through the
  `TSSLContextConfig` factory overload.
- Before this batch, the factory overload still bounced through
  `SSLConfigFromContextConfig(...)`, which could let legacy normalization rewrite
  the new surface's option truth.

## Planned Batch

- Add a focused direct-application contract for the factory overload.
- Add runtime coverage proving a disabled `ssoEnableSessionCache` option stays
  disabled even when `SessionCacheSize > 0`.
- Introduce direct context-safe normalization/application helpers in
  `src/fafafa.ssl.factory.pas`.
- Keep `TSSLFactory.CreateContext(const TSSLConfig)` compatible.
- Update API reference and working records.

## Verification

- RED:
  - `bash tests/scripts/test_tsslcontextconfig_factory_direct_application_contract.sh`
  - failed first because `NormalizeContextConfigOptions(...)` and the direct
    apply helper did not exist.
- GREEN:
  - `FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc bash tests/scripts/test_tsslcontextconfig_factory_direct_application_contract.sh`
  - `FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc bash tests/scripts/test_tsslcontextconfig_surface_contract.sh`
  - `FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc bash tests/scripts/test_tsslcontextconfig_builder_adoption_contract.sh`
  - `bash tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
  - `bash -n tests/scripts/test_tsslcontextconfig_factory_direct_application_contract.sh`
  - `git diff --check`
  - `FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc python3 scripts/compile_all_modules.py --rebuild`

## Review Conclusion

- Verified. The context-safe factory overload now has its own direct
  normalization/application path and no longer uses legacy `TSSLConfig` as the
  main transport for new `TSSLContextConfig` callers. Adjacent builder,
  additive-surface, and legacy option-bridge contracts stayed green.

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
