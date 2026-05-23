# Task Plan: FreePascal embedded SCT fail-closed alignment

## Goal
Keep the FreePascal client CT/SCT runtime surface fail-closed when an embedded X.509 SCT list is malformed, and close the last failing completeness-gate group without widening scope.

## Status
Complete

## Current Plan
- [docs/plans/2026-05-23-freepascal-embedded-sct-fail-closed-alignment.md](docs/plans/2026-05-23-freepascal-embedded-sct-fail-closed-alignment.md)

## Done
- Reproduced the remaining completeness-gate failure in `test_freepascal_client_ct_sct_surface`.
- Kept the embedded SCT fallback path fail-closed instead of swallowing malformed `signed_certificate_timestamp` errors.
- Re-ran the focused CT/SCT runtime proof and the fast-local completeness gate.

## Verification
- `mkdir -p tmp/test_freepascal_client_ct_sct_surface && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_freepascal_client_ct_sct_surface -FEtmp/test_freepascal_client_ct_sct_surface -otmp/test_freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface tests/test_freepascal_client_ct_sct_surface.pas && ./tmp/test_freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local`
- `python3 -u scripts/compile_all_modules.py` still stops only at the pre-existing `fafafa.ssl.pkcs11.engine.pas` boundary (`185/186` compiled)
- `git diff --check`
