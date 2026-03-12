# Runtime Contract Cleanup Summary (March 7, 2026)

This note is the short version of the March 7, 2026 runtime-contract cleanup.

Use it when you want a quick handoff, PR summary, or commit-summary source without reading the full plan trail.

## What changed

The repository now has explicit runtime or compile-only contract coverage for every active Pascal `program` entrypoint under `tests/integration` and `tests/certificate`.

The cleanup work also:
- moved active smoke programs to current OpenSSL loader semantics where needed,
- added stable ASCII completion markers for runtime contracts,
- fixed a small number of real test or production drift issues discovered during the sweep,
- consolidated overlapping contract scripts into a smaller current script set, and
- added a top-level runtime regression batch plus coverage guard.

## Current entrypoints

For the broadest active-program sweep, run:

```bash
bash tests/scripts/test_active_program_runtime_contract_batch.sh
```

For the cheapest high-signal daily gate, run:

```bash
bash scripts/run_minimal_ci_gate.sh --fast-local
python3 scripts/compile_all_modules.py
```

For the current script map, check `docs/plans/2026-03-07-runtime-contracts-current-index.md`.

For the historical cleanup trail, check `docs/plans/2026-03-07-runtime-contracts-historical-index.md`.

## Notable fixes found during the sweep

A few issues turned out to be more than contract gaps:
- `src/fafafa.ssl.openssl.certificate.pas` now loads the required BN or ASN.1 helpers before formatting certificate serial numbers.
- `tests/integration/test_x509_basic.pas` now matches current X.509 name encoding and version semantics.
- `tests/certificate/test_cert_store.pas` now uses the canonical native-handle helper instead of an outdated direct interface call.
- `tests/certificate/test_certificate_unit.pas` now matches the current `TSSLStringArray`-based certificate metadata API.

## Current state

At the end of this cleanup:
- active `program` entrypoints left uncovered: `0`
- deleted runtime-contract stale refs left in docs and memory files: `0`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`: passing
- `python3 scripts/compile_all_modules.py`: passing (`231/231`)

## If you need the shortest PR summary

You can summarize the work like this:

> Added full active-program runtime/compile-only contract coverage for `tests/integration` and `tests/certificate`, fixed a few real API/semantic drifts found during the sweep, consolidated overlapping contract scripts, and added a top-level runtime regression batch with coverage protection.
