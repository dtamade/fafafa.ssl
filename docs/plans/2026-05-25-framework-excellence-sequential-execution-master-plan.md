# Framework Excellence Sequential Execution Master Plan

## Goal

Execute the remaining framework-excellence work as one ordered queue, instead
of repeatedly reopening route selection after each small batch.

This plan absorbs the current roadmap tasks into a single sequence:

1. keep the active roadmap synchronized with real progress
2. finish the `TSSLConfig` scope-surgery implementation line
3. simplify the curated facade and ordinary public entrypoint
4. advance FreePascal backend excellence where fresh evidence justifies it
5. tighten performance, CI, release, and verification evidence around the new
   architecture

## Operating Rule

Each stage must finish with:

- a focused RED check before production behavior changes
- the smallest maintainable implementation that turns the check GREEN
- relevant focused contracts
- `python3 scripts/compile_all_modules.py --rebuild`
- `git diff --check`
- updates to `task_plan.md`, `findings.md`, and `progress.md`
- one clear git commit

If a stage exposes an unrelated blocker, record it and either fix it inside the
stage only when it protects the same contract, or queue it after the current
stage. Do not restart roadmap planning unless the sequence itself becomes
wrong.

## Stage 0: Route Control Sync

**Status:** complete in commit `f16298a`.

**Purpose:** Make the repo's active entrypoint point to this sequential plan so
the next sessions do not drift back to older route-selection documents.

**Files:**

- `docs/ROADMAP.md`
- `tests/scripts/test_active_roadmap_references_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

**Verification:**

```bash
bash tests/scripts/test_active_roadmap_references_contract.sh
bash tests/scripts/test_architecture_current_route_truth_contract.sh
git diff --check
```

**Exit condition:** `docs/ROADMAP.md` names this file as both
`current_active_batch` and `next_route_candidate`.

## Stage 1: TSSLContextConfig factory direct application

**Status:** complete in the Stage 1 implementation batch.

**Purpose:** Reduce the remaining legacy bounce where
`TSSLFactory.CreateContext(const TSSLContextConfig)` projects back through
`TSSLConfig` before applying context-safe fields.

**Primary files:**

- `src/fafafa.ssl.factory.pas`
- `src/fafafa.ssl.base.pas`
- `tests/test_tsslcontextconfig_surface.pas`
- `tests/scripts/test_tsslcontextconfig_surface_contract.sh`
- new focused contract if needed:
  `tests/scripts/test_tsslcontextconfig_factory_direct_application_contract.sh`

**Implementation direction:**

- Keep `TSSLFactory.CreateContext(const TSSLConfig)` compatible.
- Keep `TSSLContextConfig` additive.
- Add a context-safe apply helper only for fields that are already
  context-scoped.
- Preserve current validation and option-bridge behavior.
- Do not move PEM, PKCS#11, HTTP hooks, OCSP response-file loading,
  replay-store installers, or backend-gated custom cipher overrides into a
  generic config helper unless a focused test proves that is the correct owner.

**Verification:**

```bash
bash tests/scripts/test_tsslcontextconfig_factory_direct_application_contract.sh
bash tests/scripts/test_tsslcontextconfig_surface_contract.sh
bash tests/scripts/test_tsslcontextconfig_builder_adoption_contract.sh
bash tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh
FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc python3 scripts/compile_all_modules.py --rebuild
git diff --check
```

**Exit condition:** The context-safe factory overload applies its stable fields
without relying on `TSSLConfig` as the main internal transport for new
context-safe callers.

## Stage 2: TSSLConfig scope-surgery completion

**Status:** complete. All active public guidance and examples now treat builder /
`TSSLContextConfig` as the preferred new path while keeping legacy `TSSLConfig`
as `v1.x` compatibility. Ordinary builder certificate/key/trust file material
flows through `TSSLContextConfig`; PEM, PKCS#11, HTTP hooks, OCSP response-file
loading, replay-store installers, and backend-gated custom cipher overrides
remain on their owner-specific post-create paths.

**Purpose:** Continue moving callers and docs away from the mixed-scope
`TSSLConfig` mental model while preserving `v1.x` compatibility.

**Primary files:**

- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.factory.pas`
- `src/fafafa.ssl.context.builder.pas`
- `docs/reference/API_REFERENCE.md`
- `docs/plans/2026-05-18-tsslconfig-public-surface-slimming-roadmap.md`
- `tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
- `tests/scripts/test_tsslconfig_migration_targets_contract.sh`
- `tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`

**Implementation direction:**

- Keep `TSSLConfig` as a compatibility record.
- Make the newer owner-correct surfaces the preferred implementation path.
- Keep library defaults on `TSSLLibraryDefaults`.
- Keep connection hints on connector / acceptor / connection-control surfaces.
- Keep deprecated context-level SNI out of new context-safe flows.

**Verification:**

```bash
bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh
bash tests/scripts/test_tsslconfig_migration_targets_contract.sh
bash tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh
bash tests/scripts/test_tssllibrarydefaults_surface_contract.sh
FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc python3 scripts/compile_all_modules.py --rebuild
git diff --check
```

**Exit condition:** New public guidance and high-level code paths no longer
treat `TSSLConfig` as the universal place for library, context, connection, and
compatibility settings.

## Stage 3: Curated facade simplification

**Status:** complete. All active public-facing docs and examples converge on
`uses fafafa.ssl, fafafa.ssl.context.builder;` with builder / connector /
acceptor as the primary entrypoint. Advanced surfaces remain available but
clearly labeled.

**Purpose:** Make the ordinary user path shorter and more obvious:
`uses fafafa.ssl, fafafa.ssl.context.builder;` with builder / connector /
acceptor as the primary entrypoint.

**Primary files:**

- `src/fafafa.ssl.pas`
- `docs/README.md`
- `docs/ARCHITECTURE.md`
- `docs/reference/API_REFERENCE.md`
- `docs/guides/*`
- `examples/*`
- existing facade and active-docs contract scripts under `tests/scripts/`

**Implementation direction:**

- Prefer curated re-exports for ordinary application code.
- Do not remove compatibility exports in `v1.x` without a replacement and a
  migration contract.
- Demote raw helpers in active docs when they are not the ordinary path.
- Keep advanced backend-specific escape hatches available but clearly labeled.

**Verification:**

```bash
bash tests/scripts/test_facade_main_entry_truth_contract.sh
bash tests/scripts/test_public_unit_import_guidance_truth_contract.sh
bash tests/scripts/test_active_examples_public_import_truth_contract.sh
bash tests/scripts/test_user_guide_ordinary_entrypoint_truth_contract.sh
FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc python3 scripts/compile_all_modules.py --rebuild
git diff --check
```

**Exit condition:** Ordinary docs and examples converge on the curated facade
and builder / connector path, while advanced surfaces remain explicitly
advanced.

## Stage 4: FreePascal backend excellence

**Purpose:** Move the pure Pascal backend from "feature-complete enough" toward
a backend that is worth choosing for Pascal-first deployments.

**Primary files:**

- `src/fafafa.ssl.freepascal.*.pas`
- `tests/test_freepascal_tls13_*.pas`
- `tests/scripts/run_freepascal_tls13_completeness_gate.sh`
- `docs/BACKEND_CAPABILITY_MATRIX.md`
- `docs/ROADMAP.md`

**Implementation direction:**

- Do not reopen already-closed early-data durability families without fresh
  RED evidence.
- Keep current early-data capability as experimental until the project can
  honestly claim more than local persistent fail-closed replay protection.
- Prioritize fresh correctness, interop, observability, and performance gaps
  over repeating existing closeout work.
- Treat distributed / cross-host replay coordination as a deliberate future
  design topic, not an accidental TODO inside local store code.

**Verification:**

```bash
FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local
FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc python3 scripts/compile_all_modules.py --rebuild
git diff --check
```

**Exit condition:** The next FreePascal backend batch is selected from fresh
evidence and advances correctness, interop, performance, observability, or
packaging simplicity rather than reopening closed durability archaeology.

## Stage 5: Performance and operational excellence

**Purpose:** Make architecture claims, performance claims, and release evidence
traceable without slowing every feature batch.

**Primary files:**

- `scripts/run_minimal_ci_gate.sh`
- `scripts/compile_all_modules.py`
- benchmark scripts under `scripts/`
- `.github/workflows/*.yml`
- `docs/test_reports/*`
- `docs/ROADMAP.md`

**Implementation direction:**

- Keep the default local gate fast enough for every batch.
- Add heavier benchmark / cross-platform evidence only where it protects a
  meaningful release or performance claim.
- Prefer a small number of high-signal gates over many stale scripts.
- Make each gate explain what decision it protects.

**Verification:**

```bash
FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc python3 scripts/compile_all_modules.py --rebuild
bash scripts/run_minimal_ci_gate.sh --fast-local
git diff --check
```

**Exit condition:** The repo has a faster default development loop and a clear
heavier evidence path for release/performance claims.

## Execution Order

Run stages in this order:

1. Stage 0: Route Control Sync
2. Stage 1: TSSLContextConfig factory direct application
3. Stage 2: TSSLConfig scope-surgery completion
4. Stage 3: Curated facade simplification
5. Stage 4: FreePascal backend excellence
6. Stage 5: Performance and operational excellence

If Stage 1 or Stage 2 reveals that the config split needs one more additive
surface, create that surface before moving to facade simplification. If Stage 4
reveals no fresh FreePascal RED, skip implementation there and move to Stage 5
with a recorded finding rather than manufacturing work.

## Anti-Slowdown Rules

- Do not create another roadmap-only batch unless the current sequence becomes
  false.
- Do not ask for scope selection again; the selected scope is this full
  sequential plan.
- Do not write a new broad design doc before Stage 1.
- Do not run every historical contract on every small batch; run focused
  contracts plus compile-all, then add broader gates only at stage boundaries.
- Do not reopen Wave C, release formalization, OCSP, CT, validation, or
  closed early-data durability families without fresh RED evidence.
